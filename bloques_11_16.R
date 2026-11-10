# BLOQUES 11 A 16: comparación, prueba final, incertidumbre, diagnóstico y sensibilidades
# Van al FINAL de Codigo_principal.R (después del bloque 10). También se pueden correr
# directamente sobre una sesión donde ya se corrieron los bloques 1 a 10.

# Comprobación: estos objetos de los bloques 1 a 10 deben existir (todos TRUE)
necesarios <- c("datos", "base_m1", "X", "formula_m1", "columnas_X", "lambdas", "alphas",
                "ajustar_m1", "filas_entrena", "filas_valida", "ganador", "resultados_grilla",
                "formulas_m2", "formula_C_k15", "ajustar_m2", "ganador_m2",
                "resultados_m2", "resultados_k15", "vars_rf", "configs_rf", "cfg", "train_full",
                "rf_full", "n_arboles", "tam_submue", "resultados_rf", "por_anio_rf",
                "ganador_rf", "atributos")
print(sapply(necesarios, exists))
stopifnot(all(sapply(necesarios, exists)))
if (!exists("t_inicio")) t_inicio <- Sys.time()


# BLOQUE 11. TABLA COMPARATIVA ÚNICA Y ELECCIÓN DEL MODELO #
# Junta referencia, M1, M2 y M3 en las MISMAS filas de validación (base_m1), con la
# misma agregación: mediana entre ríos por año, y promedio de los 7 años.
# La regla para elegir entre familias se fija aquí, ANTES de mirar la prueba final.

# 11.1 La configuración ganadora de cada familia (elegida en los bloques 8, 9 y 10)
m1_rios <- subset(resultados_grilla, tipo == ganador$tipo & lambda == ganador$lambda)
m2_todo <- rbind(resultados_m2, resultados_k15)
m2_rios <- subset(m2_todo, config == ganador_m2$config)
m3_rios <- subset(resultados_rf, config == ganador_rf$config)

# 11.2 Referencia estacional en las mismas filas: mediana del caudal de mañana
# por río y mes, calculada solo con el entrenamiento de cada ventana
est_rios <- data.frame()
for (anio_ext in 2006:2012) {
  tr <- filas_entrena(anio_ext); va <- filas_valida(anio_ext)
  tipico <- aggregate(Q_manana ~ cuenca + mes_manana, data = base_m1[tr, ], FUN = median)
  clave_t <- paste(tipico$cuenca, tipico$mes_manana)
  v <- base_m1[va, ]
  pred_est <- tipico$Q_manana[match(paste(v$cuenca, v$mes_manana), clave_t)]
  cu <- v$cuenca
  est_rios <- rbind(est_rios, data.frame(
    anio = anio_ext, cuenca = names(tapply(v$Q_manana, cu, length)),
    MAE  = as.vector(tapply(abs(pred_est - v$Q_manana), cu, mean)),
    RMSE = as.vector(sqrt(tapply((pred_est - v$Q_manana)^2, cu, mean)))))
}

# 11.3 Una tabla larga: una fila por modelo, año y río
uniformar <- function(df, modelo, col_mae, col_rmse) {
  data.frame(modelo = modelo, anio = df$anio, cuenca = as.character(df$cuenca),
             dias = df$dias, MAE = df[[col_mae]], RMSE = df[[col_rmse]],
             MAE_pers = df$MAE_pers, RMSE_pers = df$RMSE_pers)
}
comparacion <- rbind(
  uniformar(m1_rios, "Persistencia", "MAE_pers", "RMSE_pers"),
  uniformar(m1_rios, "M1 ridge/lasso", "MAE_m1", "RMSE_m1"),
  uniformar(m2_rios, "M2 GAM", "MAE_m2", "RMSE_m2"),
  uniformar(m3_rios, "M3 Random Forest", "MAE_rf", "RMSE_rf")
)
# la estacional se pega con la persistencia de las mismas filas
est_rios <- merge(est_rios, comparacion[comparacion$modelo == "Persistencia",
                  c("anio", "cuenca", "dias", "MAE_pers", "RMSE_pers")], by = c("anio", "cuenca"))
comparacion <- rbind(comparacion, data.frame(modelo = "Estacional", est_rios[, names(comparacion)[-1]]))

# 11.4 Comprobación: todos los modelos se evaluaron en los mismos días y con la misma
# persistencia (comparación pareada, sección 4.2 de las instrucciones)
dias_por_modelo <- tapply(comparacion$dias, comparacion$modelo, sum)
print(dias_por_modelo)   # deben ser todos iguales
stopifnot(length(unique(dias_por_modelo)) == 1)
pers_por_modelo <- tapply(comparacion$MAE_pers, comparacion$modelo, sum)
stopifnot(diff(range(pers_por_modelo)) < 1e-8)

# 11.5 Mejora frente a persistencia y resumen
comparacion$mejora_MAE  <- 1 - comparacion$MAE  / comparacion$MAE_pers
comparacion$mejora_RMSE <- 1 - comparacion$RMSE / comparacion$RMSE_pers

por_anio_todos <- aggregate(cbind(mejora_MAE, mejora_RMSE) ~ modelo + anio, data = comparacion, FUN = median)
tabla_final <- aggregate(cbind(mejora_MAE, mejora_RMSE) ~ modelo, data = por_anio_todos, FUN = mean)
tabla_final$peor_anio_MAE    <- as.vector(tapply(por_anio_todos$mejora_MAE, por_anio_todos$modelo, min)[tabla_final$modelo])
tabla_final$rios_que_mejoran <- as.vector(tapply(comparacion$mejora_MAE > 0, comparacion$modelo, sum)[tabla_final$modelo])   # de 210 pares río-año
tabla_final$dias_evaluados   <- as.vector(dias_por_modelo[tabla_final$modelo])
tabla_final <- tabla_final[order(-tabla_final$mejora_MAE), ]

# 11.6 REGLA DE SELECCIÓN ENTRE FAMILIAS (la misma de M2 y M3): mayor mejora en MAE;
# si la diferencia es menor que 0,5 puntos, desempata la mejora en RMSE.
candidatos <- tabla_final[!tabla_final$modelo %in% c("Persistencia", "Estacional"), ]
empatados  <- candidatos[candidatos$mejora_MAE >= max(candidatos$mejora_MAE) - 0.005, ]
modelo_elegido <- empatados$modelo[which.max(empatados$mejora_RMSE)]
cat("Modelo elegido en desarrollo:", modelo_elegido, "\n")

tabla_final[, 2:4] <- round(tabla_final[, 2:4], 3)
tabla_final            # LA TABLA COMPARATIVA DEL INFORME

# mediana de la mejora por año y modelo (estabilidad entre ventanas)
round(xtabs(mejora_MAE ~ anio + modelo, data = por_anio_todos), 3)

# guardamos los resultados de desarrollo para no tener que recalcularlos
saveRDS(list(comparacion = comparacion, tabla_final = tabla_final,
             modelo_elegido = modelo_elegido, ganador_m1 = ganador,
             ganador_m2 = ganador_m2, ganador_rf = ganador_rf),
        "resultados_desarrollo.rds")





# BLOQUE 12. PRUEBA FINAL 2013-2016 (se corre UNA sola vez) #
# Todo lo que se elige (configuración de cada familia y modelo final) quedó fijado en
# los bloques 8 a 11, solo con desarrollo. Aquí NO se elige nada: cada procedimiento
# se reajusta con todo el desarrollo (orígenes 2000-01-01 a 2012-12-30) y se evalúa una
# vez en 2013-01-01 a 2016-12-30. Se informan los tres procedimientos ya definidos
# (lo permite la sección 4.1), pero la recomendación es el modelo elegido en el bloque 11.

dir.create("resultados", showWarnings = FALSE)   # aquí se guardan tablas y figuras

# 12.1 Filas de prueba: mismas condiciones que base_m1 (caudal de hoy, ayer y mañana,
# y todas las pistas), pero en 2013-2016. El 31-12-2016 no tiene "mañana".
filas_prueba <- datos$usable & complete.cases(datos[, columnas_X]) &
  datos$fecha >= as.Date("2013-01-01") & datos$fecha <= as.Date("2016-12-30")
base_prueba <- datos[filas_prueba, ]
tr_final <- filas_entrena(2013)   # todo el desarrollo: orígenes hasta 2012-12-30
cat("Filas de prueba:", nrow(base_prueba), "| filas de entrenamiento final:", length(tr_final), "\n")

# 12.2 Predicciones de cada procedimiento
pred_prueba <- base_prueba[, c("cuenca", "fecha", "Q", "Q_manana", "mes_manana")]
pred_prueba$pred_pers <- base_prueba$Q                      # persistencia

# estacional: mediana del caudal de mañana por río y mes, con todo el desarrollo
tipico <- aggregate(Q_manana ~ cuenca + mes_manana, data = base_m1[tr_final, ], FUN = median)
pred_prueba$pred_est <- tipico$Q_manana[match(paste(base_prueba$cuenca, base_prueba$mes_manana),
                                              paste(tipico$cuenca, tipico$mes_manana))]

# M1: la misma matriz X, construida para las filas de prueba (mismas columnas)
X_prueba <- sparse.model.matrix(formula_m1, data = base_prueba,
                                contrasts.arg = list(cuenca_f = contrasts(base_prueba$cuenca_f, contrasts = FALSE)))[, -1]
stopifnot(identical(colnames(X_prueba), colnames(X)))
ajuste_m1_final <- ajustar_m1(tr_final, alphas[as.character(ganador$tipo)])
k_m1 <- which(lambdas == ganador$lambda)
pred_prueba$pred_M1 <- base_prueba$Q * exp(predict(ajuste_m1_final, newx = X_prueba)[, k_m1])

# M2: la configuración ganadora (puede ser C_k15, de la ampliación)
formulas_m2_todas <- c(formulas_m2, list(C_k15 = formula_C_k15))
m2_final <- ajustar_m2(tr_final, formulas_m2_todas[[as.character(ganador_m2$config)]])
pred_prueba$pred_M2 <- base_prueba$Q * exp(predict(m2_final, newdata = base_prueba))

# M3: rf_full (bloque 10.7) ya es el ganador ajustado con todo el desarrollo
te_rf <- base_prueba[, vars_rf]
te_rf$cuenca <- factor(te_rf$cuenca, levels = levels(train_full$cuenca))
te_rf$mes    <- factor(te_rf$mes,    levels = levels(train_full$mes))
pred_prueba$pred_M3 <- base_prueba$Q * exp(predict(rf_full, newdata = te_rf))

stopifnot(!anyNA(pred_prueba[, c("pred_pers", "pred_est", "pred_M1", "pred_M2", "pred_M3")]))

# 12.3 MAE y RMSE por río en la prueba (sección 4.2: error por entidad y denominadores)
modelos_prueba <- c(Persistencia = "pred_pers", Estacional = "pred_est",
                    "M1 ridge/lasso" = "pred_M1", "M2 GAM" = "pred_M2", "M3 Random Forest" = "pred_M3")
prueba_rios <- data.frame()
for (nm in names(modelos_prueba)) {
  p <- pred_prueba[[modelos_prueba[nm]]]; o <- pred_prueba$Q_manana; cu <- pred_prueba$cuenca
  prueba_rios <- rbind(prueba_rios, data.frame(
    modelo = nm, cuenca = names(tapply(o, cu, length)), dias = as.vector(tapply(o, cu, length)),
    MAE  = as.vector(tapply(abs(p - o), cu, mean)),
    RMSE = as.vector(sqrt(tapply((p - o)^2, cu, mean)))))
}
pers_rios <- subset(prueba_rios, modelo == "Persistencia")
prueba_rios$MAE_pers  <- pers_rios$MAE[match(prueba_rios$cuenca, pers_rios$cuenca)]
prueba_rios$RMSE_pers <- pers_rios$RMSE[match(prueba_rios$cuenca, pers_rios$cuenca)]
prueba_rios$mejora_MAE  <- 1 - prueba_rios$MAE  / prueba_rios$MAE_pers
prueba_rios$mejora_RMSE <- 1 - prueba_rios$RMSE / prueba_rios$RMSE_pers

# 12.4 Resumen: mediana entre ríos (cada río pesa lo mismo), en todo 2013-2016
tabla_prueba <- aggregate(cbind(mejora_MAE, mejora_RMSE) ~ modelo, data = prueba_rios, FUN = median)
tabla_prueba$rios_que_mejoran <- as.vector(tapply(prueba_rios$mejora_MAE > 0, prueba_rios$modelo, sum)[tabla_prueba$modelo])
tabla_prueba$dias_evaluados   <- as.vector(tapply(prueba_rios$dias, prueba_rios$modelo, sum)[tabla_prueba$modelo])
tabla_prueba$elegido_en_desarrollo <- tabla_prueba$modelo == modelo_elegido
tabla_prueba <- tabla_prueba[order(-tabla_prueba$mejora_MAE), ]
tabla_prueba[, 2:3] <- round(tabla_prueba[, 2:3], 3)
cat("\n=== PRUEBA FINAL 2013-2016 ===\n"); print(tabla_prueba, row.names = FALSE)

# por año de prueba (estabilidad): mediana entre ríos de la mejora en MAE
pred_prueba$anio <- as.integer(format(pred_prueba$fecha, "%Y"))
mejora_anual <- function(col) {
  sapply(2013:2016, function(a) {
    d <- pred_prueba[pred_prueba$anio == a, ]
    median(1 - tapply(abs(d[[col]] - d$Q_manana), d$cuenca, mean) /
               tapply(abs(d$pred_pers - d$Q_manana), d$cuenca, mean))
  })
}
prueba_por_anio <- sapply(modelos_prueba[-1], mejora_anual)
rownames(prueba_por_anio) <- 2013:2016
round(prueba_por_anio, 3)

write.csv(prueba_rios,  "resultados/prueba_por_rio.csv", row.names = FALSE)
write.csv(tabla_prueba, "resultados/tabla_prueba.csv",   row.names = FALSE)
write.csv(round(prueba_por_anio, 4), "resultados/prueba_por_anio.csv")
write.csv(tabla_final,  "resultados/tabla_desarrollo.csv", row.names = FALSE)
saveRDS(pred_prueba, "resultados/predicciones_prueba.rds")   # se conservan las predicciones (sección 5.4)

col_elegido <- c("M1 ridge/lasso" = "pred_M1", "M2 GAM" = "pred_M2",
                 "M3 Random Forest" = "pred_M3")[as.character(modelo_elegido)]




# BLOQUE 13. INCERTIDUMBRE DE LA DIFERENCIA CON PERSISTENCIA #
# Bootstrap por bloques móviles de FECHAS en la prueba: se remuestrean bloques de 30
# días consecutivos, y cada fecha lleva consigo los 30 ríos de ese día. Así se respeta
# la dependencia en el tiempo (un día se parece al siguiente) y la simultaneidad entre
# cuencas (una tormenta afecta a varios ríos el mismo día).
# Ojo (sección 4.3): las predicciones están fijas, el modelo NO se reajusta. El intervalo
# describe la incertidumbre por los días de prueba, condicional a los ajustes; no incluye
# la incertidumbre de selección y entrenamiento.

# matrices fecha x río de errores absolutos y cuadráticos (NA si ese día no se evaluó)
err_abs <- function(col) tapply(abs(pred_prueba[[col]] - pred_prueba$Q_manana),
                                list(as.character(pred_prueba$fecha), pred_prueba$cuenca), mean)
err_cua <- function(col) tapply((pred_prueba[[col]] - pred_prueba$Q_manana)^2,
                                list(as.character(pred_prueba$fecha), pred_prueba$cuenca), mean)
A_mod <- err_abs(col_elegido); A_per <- err_abs("pred_pers")
C_mod <- err_cua(col_elegido); C_per <- err_cua("pred_pers")

n_fechas <- nrow(A_mod); largo_bloque <- 30; n_rep <- 1000
mejora_med <- function(idx) {
  c(MAE  = median(1 - colMeans(A_mod[idx, ], na.rm = TRUE) / colMeans(A_per[idx, ], na.rm = TRUE), na.rm = TRUE),
    RMSE = median(1 - sqrt(colMeans(C_mod[idx, ], na.rm = TRUE)) / sqrt(colMeans(C_per[idx, ], na.rm = TRUE)), na.rm = TRUE))
}
set.seed(2026)
boot <- t(replicate(n_rep, {
  inicios <- sample(1:(n_fechas - largo_bloque + 1), ceiling(n_fechas / largo_bloque), replace = TRUE)
  idx <- unlist(lapply(inicios, function(s) s:(s + largo_bloque - 1)))[1:n_fechas]
  mejora_med(idx)
}))
observado <- mejora_med(1:n_fechas)
incertidumbre <- data.frame(
  metrica = c("mejora MAE", "mejora RMSE"),
  observado = round(observado, 3),
  IC95_inf  = round(apply(boot, 2, quantile, 0.025), 3),
  IC95_sup  = round(apply(boot, 2, quantile, 0.975), 3),
  prop_reps_sin_mejora = round(colMeans(boot <= 0), 3))
cat("\n=== INCERTIDUMBRE (", modelo_elegido, "frente a persistencia, prueba 2013-2016 ) ===\n")
print(incertidumbre, row.names = FALSE)
write.csv(incertidumbre, "resultados/incertidumbre_bootstrap.csv", row.names = FALSE)

png("resultados/fig_bootstrap.png", width = 900, height = 600, res = 120)
hist(boot[, "MAE"], breaks = 40, col = "steelblue", border = "white",
     main = paste("Bootstrap por bloques de 30 días:", modelo_elegido),
     xlab = "Mediana entre ríos de la mejora en MAE frente a persistencia")
abline(v = observado["MAE"], lwd = 2); abline(v = 0, lty = 2)
dev.off()

# complemento: variación entre las 7 ventanas de desarrollo (ventanas dependientes:
# se informa como rango, no como intervalo de confianza)
subset(por_anio_todos, modelo == modelo_elegido)




# BLOQUE 14. DIAGNÓSTICO DEL MODELO ELEGIDO (prueba 2013-2016) #
# Sección 4.4: errores en subgrupos con sus tamaños, extremos, y una situación en que
# la recomendación no es confiable. Los umbrales se calculan con el DESARROLLO.

# 14.1 Caudales bajos, normales, altos y extremos (umbrales de desarrollo, por río)
umbral <- function(p) tapply(base_m1$Q_manana, base_m1$cuenca, quantile, p)
u10 <- umbral(0.10); u90 <- umbral(0.90); u99 <- umbral(0.99)
pred_prueba$tipo <- "normal"
pred_prueba$tipo[pred_prueba$Q_manana <= u10[pred_prueba$cuenca]] <- "bajo (<= p10)"
pred_prueba$tipo[pred_prueba$Q_manana >= u90[pred_prueba$cuenca]] <- "alto (>= p90)"
pred_prueba$extremo <- pred_prueba$Q_manana >= u99[pred_prueba$cuenca]

mejora_grupo <- function(d, col) {   # mediana entre ríos de la mejora en MAE, dentro de un grupo de días
  m <- tapply(abs(d[[col]] - d$Q_manana), d$cuenca, mean) / tapply(abs(d$pred_pers - d$Q_manana), d$cuenca, mean)
  median(1 - m, na.rm = TRUE)
}
grupos <- split(pred_prueba, pred_prueba$tipo)
grupos[["extremo (>= p99)"]] <- pred_prueba[pred_prueba$extremo, ]
diag_caudal <- data.frame(
  grupo = names(grupos),
  dias  = sapply(grupos, nrow),
  rios  = sapply(grupos, function(d) length(unique(d$cuenca))),
  t(sapply(grupos, function(d) round(sapply(modelos_prueba[-(1:2)], function(col) mejora_grupo(d, col)), 3))),
  check.names = FALSE, row.names = NULL)
cat("\n=== MEJORA EN MAE POR TIPO DE CAUDAL (prueba) ===\n"); print(diag_caudal, row.names = FALSE)
write.csv(diag_caudal, "resultados/diagnostico_caudales.csv", row.names = FALSE)

# 14.2 Por tipo de cuenca: altura media y embalse grande (atributos solo para describir)
elegido_rios <- subset(prueba_rios, modelo == modelo_elegido)
elegido_rios <- merge(elegido_rios, atributos[, c("cuenca", "gauge_name", "area", "elev_mean", "big_dam")],
                      by = "cuenca")
elegido_rios$altura <- cut(elegido_rios$elev_mean, c(0, 1000, 1500, Inf),
                           labels = c("bajo 1000 m", "1000-1500 m", "sobre 1500 m"))
diag_cuencas <- rbind(
  data.frame(grupo = paste("altura", levels(elegido_rios$altura)),
             rios  = as.vector(table(elegido_rios$altura)),
             mejora_MAE = round(as.vector(tapply(elegido_rios$mejora_MAE, elegido_rios$altura, median)), 3)),
  data.frame(grupo = c("sin embalse grande", "con embalse grande"),
             rios  = as.vector(table(elegido_rios$big_dam)),
             mejora_MAE = round(as.vector(tapply(elegido_rios$mejora_MAE, elegido_rios$big_dam, median)), 3)))
cat("\n=== MEJORA EN MAE POR TIPO DE CUENCA (prueba,", modelo_elegido, ") ===\n"); print(diag_cuencas, row.names = FALSE)
write.csv(diag_cuencas, "resultados/diagnostico_cuencas.csv", row.names = FALSE)
elegido_rios[order(elegido_rios$mejora_MAE), c("gauge_name", "elev_mean", "big_dam", "dias", "mejora_MAE", "mejora_RMSE")]

# 14.3 Figura de diagnóstico: altura de la cuenca contra mejora en la prueba
png("resultados/fig_diagnostico_altura.png", width = 1000, height = 700, res = 120)
plot(elegido_rios$elev_mean, elegido_rios$mejora_MAE, pch = 19,
     col = ifelse(elegido_rios$big_dam == 1, "darkorange", "steelblue"),
     xlab = "Altura media de la cuenca (m)",
     ylab = "Mejora en MAE frente a persistencia (prueba 2013-2016)",
     main = paste(modelo_elegido, ": mejora por cuenca en la prueba"))
abline(h = 0, lty = 2)
legend("bottomleft", c("Sin embalse grande", "Con embalse grande"),
       col = c("steelblue", "darkorange"), pch = 19)
dev.off()




# BLOQUE 15. ANÁLISIS DE SENSIBILIDAD (solo con desarrollo, 7 ventanas) #
# S1 (representación): respuesta en m3/s frente a cambio log, con M3.
# S2 (validación): elegir con 3 ventanas (2010-2012, el plan original) en vez de 7.
# S3 (sugerida por la ficha): M3 con atributos físicos en vez del código de cuenca.
# S1 y S3 se hacen con M3 (configuración ganadora): es el procedimiento más flexible y
# el que más depende de cómo se representan los datos.

ventana_rf <- function(anio, vars, respuesta_log = TRUE, datos_rf = datos) {
  train <- subset(datos_rf, usable_rf & fecha <= as.Date(paste0(anio - 1, "-12-30")))
  valid <- subset(datos_rf, usable_rf & fecha >= as.Date(paste0(anio, "-01-01")) &
                    fecha <= as.Date(paste0(anio, "-12-30")))
  for (v in intersect(c("cuenca", "mes"), vars)) {
    train[[v]] <- factor(train[[v]]); valid[[v]] <- factor(valid[[v]], levels = levels(train[[v]]))
  }
  y <- if (respuesta_log) train$cambio_manana else train$Q_manana
  set.seed(3407)
  fit <- randomForest(x = train[, vars], y = y, ntree = n_arboles, mtry = min(cfg$mtry, length(vars)),
                      nodesize = cfg$nodesize, sampsize = tam_submue)
  p <- predict(fit, newdata = valid[, vars])
  pred <- if (respuesta_log) valid$Q * exp(p) else pmax(p, 0)   # en m3/s no puede ser negativo
  m <- tapply(abs(pred - valid$Q_manana), valid$cuenca, mean) / tapply(abs(valid$Q - valid$Q_manana), valid$cuenca, mean)
  r <- sqrt(tapply((pred - valid$Q_manana)^2, valid$cuenca, mean)) / sqrt(tapply((valid$Q - valid$Q_manana)^2, valid$cuenca, mean))
  c(MAE = median(1 - m), RMSE = median(1 - r))
}

# S1: respuesta original (caudal de mañana en m3/s), mismas pistas
s1 <- t(sapply(2006:2012, ventana_rf, vars = vars_rf, respuesta_log = FALSE))
cat("Listo S1\n")

# S3: atributos físicos estables en vez del código de cuenca (sin índices hidrológicos)
attr_fisicos <- c("area", "elev_mean", "slope_mean", "forest_frac", "snow_frac", "big_dam", "gauge_lat")
datos_attr <- merge(datos, atributos[, c("cuenca", attr_fisicos)], by = "cuenca")
datos_attr <- datos_attr[order(datos_attr$cuenca, datos_attr$fecha), ]
vars_attr <- c(setdiff(vars_rf, "cuenca"), attr_fisicos)
s3 <- t(sapply(2006:2012, ventana_rf, vars = vars_attr, datos_rf = datos_attr))
cat("Listo S3\n")

base_rf <- as.matrix(subset(por_anio_rf, config == ganador_rf$config)[order(subset(por_anio_rf, config == ganador_rf$config)$anio),
                                                                       c("mejora_MAE", "mejora_RMSE")])
sensibilidad <- data.frame(
  variante = c("M3 ganador (cambio log, código de cuenca)", "S1: respuesta en m3/s",
               "S3: atributos físicos en vez de cuenca"),
  mejora_MAE  = round(c(mean(base_rf[, 1]), mean(s1[, "MAE"]),  mean(s3[, "MAE"])), 3),
  mejora_RMSE = round(c(mean(base_rf[, 2]), mean(s1[, "RMSE"]), mean(s3[, "RMSE"])), 3),
  peor_anio_MAE = round(c(min(base_rf[, 1]), min(s1[, "MAE"]), min(s3[, "MAE"])), 3))
cat("\n=== SENSIBILIDADES S1 y S3 (desarrollo, promedio de 7 ventanas) ===\n"); print(sensibilidad, row.names = FALSE)
write.csv(sensibilidad, "resultados/sensibilidad_S1_S3.csv", row.names = FALSE)

# S2: ¿cambia la elección si se valida solo en 2010-2012?
elegir <- function(df, anios) {   # misma regla de los bloques 9 a 11, en los años indicados
  pa <- aggregate(cbind(mejora_MAE, mejora_RMSE) ~ anio + config, data = df[df$anio %in% anios, ], FUN = median)
  tb <- aggregate(cbind(mejora_MAE, mejora_RMSE) ~ config, data = pa, FUN = mean)
  tb <- tb[order(-tb$mejora_MAE), ]
  e  <- tb[tb$mejora_MAE >= tb$mejora_MAE[1] - 0.005, ]
  e[which.max(e$mejora_RMSE), ]
}
resultados_grilla$config <- paste(resultados_grilla$tipo, resultados_grilla$lambda)
familias <- list("M1 ridge/lasso" = resultados_grilla, "M2 GAM" = m2_todo, "M3 Random Forest" = resultados_rf)
s2 <- do.call(rbind, lapply(names(familias), function(f) {
  g7 <- elegir(familias[[f]], 2006:2012); g3 <- elegir(familias[[f]], 2010:2012)
  data.frame(familia = f, config_7_ventanas = g7$config, mejora_MAE_7 = round(g7$mejora_MAE, 3),
             config_3_ventanas = g3$config, mejora_MAE_3 = round(g3$mejora_MAE, 3))
}))
s2$elegido_7 <- s2$familia == s2$familia[which.max(s2$mejora_MAE_7)]
s2$elegido_3 <- s2$familia == s2$familia[which.max(s2$mejora_MAE_3)]
cat("\n=== SENSIBILIDAD S2: 7 ventanas frente a 3 (2010-2012) ===\n"); print(s2, row.names = FALSE)
write.csv(s2, "resultados/sensibilidad_S2.csv", row.names = FALSE)




# BLOQUE 16. CIERRE: tiempo, versiones y sesión #
cat("\nTiempo total del análisis:", round(difftime(Sys.time(), t_inicio, units = "mins"), 1), "minutos\n")
writeLines(capture.output(sessionInfo()), "resultados/sessionInfo.txt")   # versiones para el README
cat("Resultados guardados en la carpeta resultados/\n")
