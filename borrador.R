#CODIGO NUEVO
# ============================================================
# BLOQUE 1: DIAGNÓSTICO DE FALTANTES + PARTICIÓN TEMPORAL
# ============================================================
# clases 23, 24, 25 (datos faltantes).
#
# Y = Q (respuesta), X = predictores, R = !is.na(Q)
# MCAR: filtrar es insesgado
# MAR:  filtrar sesga si cambian proporciones de X
# MNAR: filtrar siempre sesga
#
# No se puede PROBAR el mecanismo, solo diagnosticar el patrón.
# Filtrar (LEEME prohíbe imputar) se justifica SI declaramos
# el supuesto y reportamos la limitación.
#
# ============================================================
# ¿QUÉ ES R = 0 Y R = 1? (LEER ANTES DE SEGUIR)
# ============================================================
# Para cada fila de datos, R es un semáforo que indica si el
# valor de Q está observado o no:
#
#   R = 1  ->  el caudal Q SÍ está observado ese día
#   R = 0  ->  el caudal Q FALTA ese día (es NA)
#
# ¿Para qué sirve R? Para hacer UNA pregunta clave:
#   "¿Los días donde falta Q son DISTINTOS de los días donde está Q?"
#
# Y esa pregunta se responde comparando X (lo que SÍ veo siempre)
# entre los dos grupos:
#
#   Grupo R = 1 (días con Q):     X = P, Temp, lluvia, etc.
#   Grupo R = 0 (días sin Q, NA): X = P, Temp, lluvia, etc.
#
# Interpretación:
#   - Si X es IGUAL en ambos grupos   -> MCAR (el NA es al azar)
#   - Si X es DISTINTO en ambos grupos -> MAR  (el NA depende de X)
#   - Si además Q_ayer es distinto    -> MNAR (el NA depende de Y)
#
# ============================================================

# ============================================================
# 1.1. DIAGNÓSTICO: tasa de NA por año (S23)
# ============================================================
# Si fuera MCAR puro, cada año tendría ~2% de NA.
# Si unos años concentran más NA, MCAR no se cumple.
anio_num <- as.integer(format(datos$fecha, "%Y"))
tasa_na_anio <- aggregate(
  is.na(datos$Q) ~ anio_num,
  data = datos,
  FUN = mean
)
names(tasa_na_anio) <- c("anio", "tasa_na_Q")
cat("\n=== Tasa de NA en Q por año ===\n")
print(tasa_na_anio, row.names = FALSE)
# 2014 = 5.95%, 2016 = 5.26%, el resto ~1%. 3 veces el promedio.
# -> descarta MCAR puro.

# ============================================================
# 1.2. DIAGNÓSTICO: tasa de NA por cuenca (S23)
# ============================================================
# Si varía mucho entre cuencas, la probabilidad de faltar
# depende de la cuenca. Eso es MAR condicional en cuenca.
tasa_na_cuenca <- aggregate(
  is.na(datos$Q) ~ datos$cuenca,
  data = datos,
  FUN = mean
)
names(tasa_na_cuenca) <- c("cuenca", "tasa_na_Q")
cat("\n=== Tasa de NA en Q por cuenca (top 10) ===\n")
print(head(tasa_na_cuenca[order(-tasa_na_cuenca$tasa_na_Q), ], 10),
      row.names = FALSE)
# 10102001 = 6%, otras casi 0%. Varía 300x.
# -> sigue descartando MCAR.

# ============================================================
# 1.3. DIAGNÓSTICO PRELIMINAR con Q_ayer (cobertura 7.6%)
# ============================================================
# Solo cubre el primer día de cada racha (232 casos de 3038).
# Sirve como preliminar; el diagnóstico principal está en 1.3bis
# (X, 100% cobertura) y 1.3ter (Q_ult_obs, 100% cobertura).
#
# ============================================================
# AQUÍ APARECE R POR PRIMERA VEZ
# ============================================================
# Comparamos Q_ayer entre:
#   - Grupo R = 1: días donde Q está observado hoy
#   - Grupo R = 0: días donde Q falta hoy (es NA)
#
# Idea: Q_ayer es la "pista" de lo que venía pasando antes del NA.
# Si los días con NA tenían Q_ayer ALTO, es señal de MNAR:
# los sensores fallan más cuando el río viene crecido.
#
# OJO: Q_ayer solo existe el primer día de cada racha.
# Por eso la cobertura es solo del 7.6% de los NA.
#
# CLAVE: usamos is.na(d$Q) LOCAL, no datos$Q_falta, para no
# depender del tipo de esa columna.
# ============================================================
d_ayer <- datos[!is.na(datos$Q_ayer), ]

# R LOCAL: TRUE = R=0 (Q falta), FALSE = R=1 (Q está)
q_falta_local <- is.na(d_ayer$Q)

comparacion_mcar_prelim <- data.frame(
  grupo   = c("R=1 (Q observado)", "R=0 (Q faltante)"),
  n       = c(sum(!q_falta_local),   # cuenta días R=1
              sum(q_falta_local)),    # cuenta días R=0
  media   = c(mean(d_ayer$Q_ayer[!q_falta_local]),   # media Q_ayer en R=1
              mean(d_ayer$Q_ayer[ q_falta_local])),  # media Q_ayer en R=0
  mediana = c(median(d_ayer$Q_ayer[!q_falta_local]),
              median(d_ayer$Q_ayer[ q_falta_local])),
  sd      = c(sd(d_ayer$Q_ayer[!q_falta_local]),
              sd(d_ayer$Q_ayer[ q_falta_local]))
)
cat("\n=== 1.3 PRELIMINAR: Q_ayer por R (cobertura 7.6%) ===\n")
print(comparacion_mcar_prelim, row.names = FALSE)
# Resultado:
#   R=1: n=183003, mediana Q_ayer = 27.3
#   R=0: n=232,    mediana Q_ayer = 38.5  (+41%)
# -> Los días que faltan venían con caudal más alto.
# -> Señal de MNAR, pero con solo 232 casos.

# ============================================================
# 1.3bis. DIAGNÓSTICO PRINCIPAL: X entre R=0 y R=1 (S24)
# ============================================================
# S24 p.21-22: comparar X (siempre observado) entre R=0 y R=1.
# Cobertura: 100% de los días (X nunca es NA).
# Si X difiere entre R=0 y R=1 -> MAR condicional en X.
#
# ============================================================
# AQUÍ ESTÁ LA COMPARACIÓN MÁS IMPORTANTE DE R
# ============================================================
# Comparamos X (lo que SIEMPRE veo: P, Temp, lluvia) entre:
#   - Grupo R = 1: días con Q observado
#   - Grupo R = 0: días sin Q (NA)
#
# ¿Por qué esta es la principal? Porque X nunca es NA.
# Tenemos cobertura 100% (los 186300 días entran).
#
# Interpretación:
#   - X igual en ambos grupos   -> MCAR (el NA no depende de X)
#   - X distinto en ambos grupos -> MAR (el NA depende de X)
# ============================================================
vars_obs <- c("P", "Temp", "lluvia_3d", "lluvia_7d",
              "lluvia_30d", "temp_7d")

comp_x <- do.call(rbind, lapply(vars_obs, function(v) {
  d <- datos[!is.na(datos[[v]]), ]
  
  # R LOCAL: FALSE = día R=1 (Q está), TRUE = día R=0 (Q falta)
  q_falta_local <- is.na(d$Q)
  
  g1 <- d[[v]][!q_falta_local]        # valores de X en días R=1
  g0 <- d[[v]][ q_falta_local]        # valores de X en días R=0
  
  data.frame(
    variable   = v,
    n_R1       = length(g1),          # cuántos días R=1
    n_R0       = length(g0),          # cuántos días R=0
    media_R1   = mean(g1),            # media de X en días R=1
    media_R0   = mean(g0),            # media de X en días R=0
    mediana_R1 = median(g1),
    mediana_R0 = median(g0),
    dif_pct    = round(100 * (mean(g0) - mean(g1)) / mean(g1), 1)
    #                     ^^^^^^^^^^^^^^^^^^^^^^^
    #                     cuánto cambia X al pasar de R=1 a R=0
  )
}))
cat("\n=== 1.3bis PRINCIPAL: Comparación de X entre R=0 y R=1 ===\n")
print(comp_x, row.names = FALSE)
# Resultado:
#   P:          +1.0%   (casi igual)
#   Temp:       +0.7%   (casi igual)
#   lluvia_3d:  +5.6%   (empieza a notarse)
#   lluvia_7d:  +7.6%   (más claro)
#   lluvia_30d: +11.9%  <- EL MÁS GRANDE
#   temp_7d:    +0.3%   (casi igual)
# -> Los días R=0 tienen 12% más lluvia acumulada a 30 días.
# -> Eso es MAR condicional en lluvia_30d y mes.

# ============================================================
# 1.3ter. PROXY ROBUSTO: último Q observado antes del run
# ============================================================
# Cobertura: 100% (salvo el primer día de cada río).
# A diferencia de Q_ayer, funciona dentro de rachas.
#
# ============================================================
# AQUÍ APARECE R OTRA VEZ, CON MEJOR COBERTURA
# ============================================================
# Comparamos Q_ult_obs (último Q observado antes del NA) entre:
#   - Grupo R = 1: días donde Q está observado
#   - Grupo R = 0: días donde Q falta (NA)
#
# Diferencia con 1.3: Q_ayer solo cubre el PRIMER día de cada
# racha. Q_ult_obs busca más atrás si es necesario, así que
# cubre el 100% de los NA.
#
# Si R=0 tiene Q previo más alto -> MNAR (fallas en crecidas).
# Si R=0 tiene Q previo más bajo -> MNAR (fallas en sequías).
# ============================================================
calcular_ult_obs <- function(x) {
  # Para cada posición, devuelve el último valor no-NA que se vio antes.
  # Ejemplo: x = c(5, NA, NA, 8, NA)
  #          out = c(NA, 5, 5, 5, 8)
  n <- length(x)
  out <- rep(NA_real_, n)
  last <- NA_real_
  for (i in seq_len(n)) {
    out[i] <- last
    if (!is.na(x[i])) last <- x[i]
  }
  out
}

datos <- datos[order(datos$cuenca, datos$fecha), ]
datos$Q_ult_obs <- ave(datos$Q, datos$cuenca, FUN = calcular_ult_obs)

d_ult <- datos[!is.na(datos$Q_ult_obs), ]

# R LOCAL otra vez
q_falta_local <- is.na(d_ult$Q)

comp_ult_obs <- data.frame(
  grupo   = c("R=1 (Q observado)", "R=0 (Q faltante)"),
  n       = c(sum(!q_falta_local),   # días R=1
              sum(q_falta_local)),    # días R=0
  media   = c(mean(d_ult$Q_ult_obs[!q_falta_local]),   # R=1
              mean(d_ult$Q_ult_obs[ q_falta_local])),  # R=0
  mediana = c(median(d_ult$Q_ult_obs[!q_falta_local]),
              median(d_ult$Q_ult_obs[ q_falta_local])),
  sd      = c(sd(d_ult$Q_ult_obs[!q_falta_local]),
              sd(d_ult$Q_ult_obs[ q_falta_local]))
)
cat("\n=== 1.3ter: Último Q observado por R (cobertura 100%) ===\n")
print(comp_ult_obs, row.names = FALSE)
# Resultado:
#   R=1: n=183231, mediana Q_ult_obs = 27.3
#   R=0: n=3035,   mediana Q_ult_obs = 33.7  (+23%)
# -> Con 3035 casos (no solo 232) se confirma: los días que
#    faltan venían con caudal más alto. Señal de MNAR en crecidas.

# ============================================================
# 1.4. DIAGNÓSTICO: tasa de NA por mes (S23)
# ============================================================
# Si hay estacionalidad fuerte, la probabilidad depende de X
# (mes), lo cual es compatible con MAR.
tasa_na_mes <- aggregate(
  is.na(datos$Q) ~ datos$mes,
  data = datos,
  FUN = mean
)
cat("\n=== Tasa de NA en Q por mes ===\n")
print(tasa_na_mes, row.names = FALSE)
# Resultado: oct-dic 2.0-2.5%, ene-mar 1.0-1.3%.
# -> Más NA en primavera (período lluvioso).

# ============================================================
# 1.5. Declaración del supuesto y la limitación
# ============================================================
# Ya diagnosticamos. Ahora escribimos qué asumimos.
# Esto va al informe, no al análisis.
cat("\n=== DECLARACIÓN DEL SUPUESTO ===\n")
cat("Los NA en Q no son MCAR puros: se concentran por año (2014, 2016),\n")
cat("cuenca (10102001 con 6%) y mes (oct-dic).\n")
cat("Diagnóstico con X (S24): comparando X entre R=0 y R=1,\n")
cat("lluvia_30d es 12% mayor en R=0 -> MAR condicional en lluvia.\n")
cat("Diagnóstico con último Q observado (proxy de Y, 100% cobertura):\n")
cat("comparando Q_ult_obs entre R=0 y R=1, la mediana es 23% mayor\n")
cat("en R=0 -> señal de MNAR (fallas en crecidas).\n")
cat("Asumimos MAR condicional en cuenca/año/mes. Filtrar es defendible\n")
cat("para predicción bajo este supuesto.\n")
cat("Limitación: si es MNAR, el modelo subestima eventos extremos.\n")

# ============================================================
# 1.6. Actualizar `usable` para exigir TODOS los predictores
# ============================================================
# Antes solo pedías Q, Q_manana, Q_ayer. Ahora exigimos también
# lluvia_30d y temp_7d completos (los que tienen más NA al inicio).
#
# Este es el FILTRO de NA. Se hace AQUÍ (después de diagnosticar)
# para que el diagnóstico previo tenga sentido.
datos$usable <- !is.na(datos$Q) &
  !is.na(datos$Q_manana) &
  !is.na(datos$Q_ayer) &
  !is.na(datos$P) &
  !is.na(datos$P_ayer) &
  !is.na(datos$lluvia_3d) &
  !is.na(datos$lluvia_7d) &
  !is.na(datos$lluvia_30d) &
  !is.na(datos$temp_7d)

# ============================================================
# 1.7. PARTICIONES 
# ============================================================
# hecho a mano acaa

#entrenamiento: 2000-2012
entrenam <- subset(datos,usable &
                     fecha>= as.Date("2000-01-01") &
                     fecha <= as.Date("2012-12-30"))

# test final:2013-2016
test_final <- subset(datos,usable &
                       fecha >=as.Date("2013-01-01") &
                       fecha <=as.Date("2016-12-30"))

# verificamos no solapamiento
stopifnot(max(entrenam$fecha) < min(test_final$fecha))



# ============================================================
# 1.8. 3 VENTANAS INTERNAS DE VALIDACIÓN (sección 3.2)
# ============================================================
# La sección 3.2 del proyecto pide "tres particiones internas válidas".
# Creamos 3 ventanas con entrenamiento creciente:
# - Ventana 1: entrena 2000-2009, valida 2010
# - Ventana 2: entrena 2000-2010, valida 2011
# - Ventana 3: entrena 2000-2011, valida 2012
crear_ventana <- function(anio_val) {
  # Entrenamiento: todos los años anteriores al año de validación.
  # Restamos 1 día al corte por la misma razón que el corte 30-12:
  # la respuesta del 31-12-(anio-1) es el 01-01-anio, que pertenece
  # a la validación.
  train <- subset(entrenam,
                  fecha <  as.Date(paste0(anio_val, "-01-01")) - 1)
  valid <- subset(entrenam,
                  fecha >= as.Date(paste0(anio_val, "-01-01")) &
                    fecha <= as.Date(paste0(anio_val, "-12-30")))
  list(train = train, valid = valid, anio = anio_val)
}

ventanas <- lapply(2010:2012, crear_ventana)

# Verificar no solapamiento en cada ventana
for (v in ventanas) {
  stopifnot(max(v$train$fecha) < min(v$valid$fecha))
}
cat("\n3 ventanas OK, sin solapamiento temporal\n")

# ============================================================
# 1.9. Reporte de tamaños
# ============================================================
cat("\n=== TAMAÑOS ===\n")
cat("Entrenamiento (2000-2012):", nrow(entrenam), "filas\n")
cat("Test final (2013-2016):", nrow(test_final), "filas\n")
for (v in ventanas) {
  cat("Ventana", v$anio, ": train =", nrow(v$train),
      "| valid =", nrow(v$valid), "\n")
}

# ============================================================
# 1.10. Cambio de población entrenamiento -> test (S24)
# ============================================================
# En S24 se compara "población" (Y completo) con "quienes reportan".
# Aquí comparamos entrenamiento con test.
cat("\n=== DISTRIBUCIÓN DE Q_manana EN ENT vs TEST ===\n")
cat("\nEntrenamiento:\n"); print(summary(entrenam$Q_manana))
cat("\nTest final:\n"); print(summary(test_final$Q_manana))
# Resultado: mediana baja de 29.7 a 20.2 (-32%), máximo de 15800 a 3540 (-78%).
# Es la megasequía de Chile central.