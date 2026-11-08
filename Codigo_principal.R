# Proyecto Tópicos Aplicados - Caso 01: caudal del día siguiente (CAMELS-CL)
#
# Pregunta: ¿cuánto mejora un modelo estadístico la predicción del caudal de
# mañana frente a decir "mañana igual que hoy" (persistencia)?
#
# Para correrlo: guardar este script en la misma carpeta que los 4 csv
# (caudal_m3s.csv, precipitacion_mm.csv, temperatura_media_c.csv, atributos.csv)
# y en RStudio ir a Session -> Set Working Directory -> To Source File Location.
#
# El script sigue el mismo orden que el informe técnico:
#   Bloque 1: revisión de los datos ............... sección 3 del informe
#   Bloque 2: una fila por río y día, caudal cero . sección 4
#   Bloque 3: la respuesta y las pistas ........... sección 5
#   Bloque 4: persistencia y logaritmo ............ sección 6
#   Bloque 5: modelos m1 a m4, validados en 2012 .. sección 7
#   Bloque 6: validación en 7 ventanas ............ sección 8
#   Bloque 7: dónde falla m4, RMSE y estacional ... sección 9
# Las medidas (MAE, RMSE, mejora) están definidas en la sección 2 del informe.


# BLOQUE 1. REVISIÓN DE LOS DATOS (sección 3 del informe) #
# Objetivo: comprobar calendario, unidades y códigos de ausencia antes de
# hacer cualquier cosa, como pide el enunciado.

# check.names = FALSE evita que R cambie los nombres de las columnas:
# como son números (códigos de cuenca), les pondría una X adelante.
caudal <- read.csv("caudal_m3s.csv", check.names = FALSE)

dim(caudal)            # 6210 filas (días) y 31 columnas (fecha + 30 ríos)
caudal[1:5, 1:6]       # las primeras 5 filas y 6 columnas
str(caudal[, 1:6])     # las columnas de ríos deben salir como num (número), no chr (texto)
sum(is.na(caudal))     # 3038 datos faltantes: en el archivo vienen como un espacio " ",
                       # y R los lee bien como NA

caudal$fecha <- as.Date(caudal$fecha)   # de texto a fecha, para poder calcular con ella

range(caudal$fecha)          # 2000-01-01 a 2016-12-31
table(diff(caudal$fecha))    # los 6209 saltos entre filas son de 1 día: no falta ni se repite
                             # ningún día (17 años x 365 + 5 días de años bisiestos = 6210)

# faltantes por río, de mayor a menor
faltantes <- colSums(is.na(caudal[, -1]))
sort(faltantes, decreasing = TRUE)

# revisaremos el río con más faltantes: ¿en qué años se concentran?
anio <- format(caudal$fecha, "%Y")
tapply(is.na(caudal[["10102001"]]), anio, sum) #se concentra mayormente en 2016
tapply(is.na(caudal[["7116001"]]), anio, sum) #se concentra mayormente en 2014

# aquí no podemos simplemente imputar: si rellenaramos datos, por ejemplo con
# una línea recta entre el día anterior y el siguiente, después nuestro
# modelo sería optimista, y el resultado se vería mejor de lo q es.

# un segundo detalle, es que los datos faltante de Liquiñe (10102001), son del 2016
# que sería dentro de la prueba final, por lo que este río tendrá menos días
#para evaluarse y el resultado será más incierto x lo tanto hay que reportar junto
# al error de cada río, cuántos días se evaluaron.

# finalmente, los días sin caudal no se rellenan, solo no entrarán en la evaluación
# ni para el modelo ni para persistencia y luego en el informe tendremos que
# reportar cuántos días se evaluaron por río. (Decisión 1 del informe)

precipitacion <- read.csv("precipitacion_mm.csv", check.names = FALSE)
temperatura   <- read.csv("temperatura_media_c.csv", check.names = FALSE)

dim(precipitacion)
dim(caudal)
dim(temperatura)
sum(is.na(precipitacion))   # 0: la lluvia está completa
sum(is.na(temperatura))     # 0: la temperatura está completa

names(caudal)[2:6]          # primeras 5 cuencas de la tabla de caudal
names(precipitacion)[2:6]   # primeras 5 cuencas de la tabla de lluvia: ¡otro orden!

#efectivamente tienen el mismo tamaño que la tabla caudal. No les faltan datos.
# Notamos que los nombres de las tablas no están ordenados de la misma manera,
# por lo que no podemos simplemente pegar por posición: le asignaríamos a un río
# la lluvia de otro, y R no avisaría. (Decisión 2 del informe)


# BLOQUE 2. UNA FILA POR RÍO Y DÍA, Y EL CAUDAL CERO (sección 4 del informe) #
# Objetivo: pasar de "una columna por río" a "una fila por río y día", que es
# la unidad de análisis del enunciado, y unir las tres tablas por cuenca y fecha.

cuencas <- names(caudal)[-1]   # los 30 códigos de cuenca (sin la columna fecha)

# rep(..., each = 6210): cada código se repite 6210 veces seguidas (una por día)
# rep(..., times = 30): la secuencia completa de fechas se repite 30 veces (una por río)
# unlist: pone las columnas de caudal una debajo de la otra, en el mismo orden
caudal_largo <- data.frame(
  cuenca = rep(cuencas, each = nrow(caudal)),
  fecha  = rep(caudal$fecha, times = length(cuencas)),
  Q      = unlist(caudal[, -1], use.names = FALSE)
)

dim(caudal_largo) #186300 filas: combinaciones de rio y día (30x6210)
head(caudal_largo)
# comprobación a mano: los dos valores deben coincidir (4.14)
caudal[1, "9127001"]
subset(caudal_largo, cuenca == "9127001" & fecha == as.Date("2000-01-01"))

# hacemos lo equivalente para precipitaciones (código hecho a mano acá :) )
precipitacion$fecha <- as.Date(precipitacion$fecha)
cuencas_p <- names(precipitacion)[-1]

precipitacion_largo <- data.frame(
  cuenca = rep(cuencas_p, each = nrow(precipitacion)),
  fecha  = rep(precipitacion$fecha, times = length(cuencas_p)),
  P      = unlist(precipitacion[, -1], use.names = FALSE)
)

# lo mismo con temperatura (se llama Temp y no T, porque en R T significa TRUE)
temperatura$fecha <- as.Date(temperatura$fecha)
cuencas_t <- names(temperatura)[-1]
temperatura_largo <- data.frame(
  cuenca = rep(cuencas_t, each = nrow(temperatura)),
  fecha  = rep(temperatura$fecha, times = length(cuencas_t)),
  Temp   = unlist(temperatura[, -1], use.names = FALSE)
)

# unimos las tres tablas: merge busca filas con la misma cuenca y la misma fecha,
# así que el orden distinto de las columnas originales ya no importa
datos <- merge(caudal_largo, precipitacion_largo, by = c("cuenca", "fecha"))
datos <- merge(datos, temperatura_largo, by = c("cuenca", "fecha"))

# solo para probar que haya salido bien
dim(datos)   # debe seguir en 186300 filas
subset(datos, cuenca == "8130002" & fecha == as.Date("2000-01-02"))
precipitacion[2, "8130002"]

# CAUDAL IGUAL A CERO: al pasar a logaritmo (más abajo) notamos un problema
# cuando Q == 0, pues log(0) se indetermina (-Inf).
subset(datos, Q == 0)
# vemos que es un único caso donde Q==0: Toltén (9437002) el 2003-01-01, un río
# que lleva cientos de m3/s, así que es casi seguro un error de registro.
# tenemos 2 opciones: marcarlo como NA, o usar log(Q+epsilon).
# Lo marcamos como NA (dato faltante), coherente con no inventar caudales.
# Se corrige aquí, ANTES de crear Q_manana y Q_ayer, para que el 0 no se copie
# a esas columnas. (Decisión 3 del informe; quedan 3039 caudales faltantes)
datos$Q[which(datos$Q == 0)] <- NA


# BLOQUE 3. LA RESPUESTA Y LAS PISTAS (sección 5 del informe) #
# Objetivo: agregar a cada fila lo que queremos predecir (el caudal de mañana)
# y las pistas que se conocen al final del día de hoy. (Decisión 4 del informe)

# LO QUE QUEREMOS PREDECIR: EL CAUDAL DE MAÑANA

# Ahora, a cada fila le agregamos el caudal del día siguiente del mismo
# río. Primero ordenamos por río, y dentro de cada río por fecha. Así,
# cada fila tendrá abajo el "mañana" (funciona porque el calendario está
# completo: lo comprobamos en el bloque 1)

datos <- datos[order(datos$cuenca, datos$fecha), ]
rownames(datos) <- NULL

datos$Q_manana <- c(datos$Q[-1], NA) # todo corrido una posición hacia arriba,
# con un NA final para que tenga el mismo largo.

# código para visualizar que hay un error: "mañana" solo vale si la
# fila siguiente es del mismo río. Si el río cambia se pone NA
datos[6209:6212, c("cuenca", "fecha", "Q", "Q_manana")]
datos[12419:12422, c("cuenca", "fecha", "Q", "Q_manana")]

# Arreglamos el problema: comparamos la cuenca de cada fila con la de la fila
# siguiente; si son distintas, es el último día de un río y no tiene "mañana"
ultimo_dia <- c(datos$cuenca[-1] != datos$cuenca[-nrow(datos)], TRUE)
datos$Q_manana[ultimo_dia] <- NA

# volvamos a mirar los datos que estaban mal
datos[6209:6212, c("cuenca", "fecha", "Q", "Q_manana")]
datos[12419:12422, c("cuenca", "fecha", "Q", "Q_manana")]
sum(ultimo_dia) # debe dar 30: el último día de cada uno de los 30 ríos
# ahora sí está todo bien


# AHORA, ORDENAREMOS LAS PISTAS

# 1. El caudal de ayer: con el de hoy, indica si el río viene subiendo o bajando.
# Para "mañana" corrimos la columna hacia arriba y el problema estaba en el último día de cada río.
# Para "ayer" hay que correrla hacia abajo, y el problema estará en el primer día de cada río,
# el 2000-01-01, que no tiene ayer. (El TRUE va al principio en vez de al final.)

datos$Q_ayer <- c(NA, datos$Q[-nrow(datos)])
primer_dia <- c(TRUE, datos$cuenca[-1] != datos$cuenca[-nrow(datos)])
datos$Q_ayer[primer_dia] <- NA

# 2. La lluvia acumulada: la lluvia de un solo día no basta para entender un río.
# las ventanas de varios días miden cosas distintas.
# Lluvia de hoy y de ayer: la respuesta rápida, porque un río chico sube el mismo día.
# Últimos 3 o 7 días: el temporal completo.
# Últimos 30 días: qué tan mojado está el suelo. La misma lluvia produce más caudal
# si el suelo ya está saturado.

# no usamos la lluvia de mañana ;) pq sería info futura.
# suma_ultimos(x, n) suma los últimos n días INCLUYENDO hoy. Supuesto que se
# declara en el informe: la lluvia de hoy está disponible al cierre del día.
suma_ultimos <- function(x, n) {
  resultado <- rep(NA, length(x))
  for (i in n:length(x)) {
    resultado[i] <- sum(x[(i - n + 1):i])
  }
  resultado
}

# creamos lluvia a los 7 días
datos$lluvia_7d <- suma_ultimos(datos$P, 7)
datos$lluvia_7d[datos$fecha < as.Date("2000-01-01") + 6] <- NA # los primeros 6 días de cada río no tienen 7 días hacia atrás
# (una ventana de n días deja n - 1 días en NA; como todos los ríos parten el
# 2000-01-01, basta con borrar esas fechas y así no se mezcla con el río anterior)

# comprobación a mano: las dos líneas deben dar lo mismo
subset(datos$lluvia_7d, datos$cuenca == "8130002" & datos$fecha == as.Date("2000-06-10"))
sum(subset(datos, cuenca == "8130002" & fecha >= as.Date("2000-06-04") & fecha <= as.Date("2000-06-10"))$P)

# ahora, lluvia 3 días y 30 días
datos$lluvia_3d <- suma_ultimos(datos$P, 3)
datos$lluvia_3d[datos$fecha < as.Date("2000-01-01") + 2] <- NA

datos$lluvia_30d <- suma_ultimos(datos$P, 30)
datos$lluvia_30d[datos$fecha < as.Date("2000-01-01") + 29] <- NA

# ahora usamos la lluvia de ayer (mismo arreglo del primer día que con Q_ayer)
datos$P_ayer <- c(NA, datos$P[-nrow(datos)])
datos$P_ayer[primer_dia] <- NA

# 3. La temperatura de la última semana: sirve para el deshielo en ríos con nieve
datos$temp_7d <- suma_ultimos(datos$Temp, 7) / 7 # temp media de los ultimos 7 días
datos$temp_7d[datos$fecha < as.Date("2000-01-01") + 6] <- NA

# 4. El mes, como categoría y no como número: así el modelo no supone que
# diciembre (12) y enero (1) están lejos, ni que el efecto crece de a pasos iguales
datos$mes <- factor(as.integer(format(datos$fecha, "%m")))


# BLOQUE 4. PERSISTENCIA Y LOGARITMO (sección 6 del informe) #
# Objetivo: medir la vara que el modelo debe superar, y decidir en qué escala
# trabajar.

# AHORA VEREMOS LA VARA A SUPERAR: PERSISTENCIA
# veremos cuánto se equivoca persistencia ("mañana igual que hoy")
datos$usable <- !is.na(datos$Q) & !is.na(datos$Q_manana) # marca los días que existe el caudal de hoy y mañana (los únicos en los que se puede medir el error de persistencia)

desarrollo <- subset(datos, usable & fecha <= as.Date("2012-12-30")) # el corte es 30-12 porque la respuesta del 31 de dic es el 1 de enero 2013 que pertenece a la prueba final (Decisión 5 del informe)

desarrollo$error_pers <- desarrollo$Q - desarrollo$Q_manana #predicción - valor real
mae_pers <- tapply(abs(desarrollo$error_pers), desarrollo$cuenca, mean)   # MAE por río
sort(mae_pers)

# Que el Maule tenga un MAE de 110 y el Yerba Loca uno de 0,08 dice sobre todo
# cuánta agua lleva cada río, no qué tan bien funciona persistencia en él.

# Para comparar ríos hay que mirar el error en relación con el tamaño de cada uno.
# tapply calcula la media de cada río, así que la división es río por río.

media_Q <- tapply(desarrollo$Q, desarrollo$cuenca, mean)
mae_relativo <- mae_pers / media_Q
sort(mae_relativo)

# en los ríos en que la persistencia se equivoca menos es en el 10111001 con 0.0331,
# y en el que se equivoca más es en el 7116001 con 0.3784

# ahora, veremos qué atributos tienen esos ríos (los atributos se usan aquí solo
# para describir, no como pistas del modelo)
atributos <- read.csv("atributos.csv", check.names = FALSE)

orden <- sort(mae_relativo)
extremos <- names(c(head(orden, 3), tail(orden, 3)))   # los 3 mejores y los 3 peores

atributos[atributos$gauge_id %in% extremos, c("gauge_id", "gauge_name", "area", "big_dam")]

# podemos ver que los tres donde más se equivoca son los que tienen un
# área considerablemente menor. En los que menos se equivoca es en aquellos
# que tienen un área considerablemente mayor (y dos de ellos salen de un lago:
# San Pedro y Toltén).

# Tiene sentido físico:

#Río chico: cuando llueve, el agua llega rápido al cauce y el río puede duplicarse de un día a otro.
# "Mañana igual que hoy" falla mucho.
#Río grande: el agua recorre una cuenca enorme y llega de a poco, así que el caudal cambia lento y
# persistencia acierta casi siempre.

# Donde un modelo con lluvia tiene más espacio para mejorar es en los ríos chicos
# de lluvia, sin lagos ni embalses


# PREPARAMOS EL MODELO

# comparamos con y sin logaritmo: hay mucha asimetría, dominarán los ríos que llevan
# más agua, y la proporción de cambio si son dos ríos con muy distinto caudal,
# será distinto. Al ponerlo en logaritmo se suaviza un poco.
par(mfrow = c(1, 2))
hist(desarrollo$Q, breaks = 100, main = "Caudal (m3/s)", xlab = "Q")
hist(log(desarrollo$Q), breaks = 100, main = "Logaritmo del caudal", xlab = "log(Q)")
par(mfrow = c(1, 1))

# trabajaremos en logaritmo (Decisión 6 del informe). Tres razones:
#  - el caudal en m3/s es muy asimétrico;
#  - la regresión minimiza errores al cuadrado, y en m3/s los ríos grandes
#    dominarían el ajuste;
#  - los ríos cambian en proporción a lo que llevan: pasar de 10 a 20 y de
#    1000 a 2000 es el mismo evento, y en log vale lo mismo (log(2) = 0.69).
datos$log_Q         <- log(datos$Q)                          # cuánta agua lleva hoy
datos$cambio_hoy    <- log(datos$Q) - log(datos$Q_ayer)      # + viene subiendo, - viene bajando
datos$cambio_manana <- log(datos$Q_manana) - log(datos$Q)    # LO QUE EL MODELO PREDICE
# si el modelo predice cambio_manana = 0, está prediciendo exactamente persistencia

summary(datos$log_Q)          # ya no aparece -Inf: el caudal 0 se corrigió arriba
summary(datos$cambio_manana)


# BLOQUE 5. MODELOS m1 A m4, VALIDADOS EN 2012 (sección 7 del informe) #
# Objetivo: construir el modelo de a una pieza, para ver qué aporta cada una.
# Se entrena con 2000-2011 y se valida en 2012. Ojo: la tabla se llama
# "prueba", pero 2012 es un año de VALIDACIÓN, dentro del desarrollo. La
# prueba final es 2013-2016 y no se toca hasta el final.

# SEPARAMOS ENTRENAMIENTO Y VALIDACIÓN
# el modelo necesita Q_ayer para calcular cambio_hoy, así que para comparar
# modelo y persistencia en los mismos días agregamos esa condición a usable
datos$usable <- !is.na(datos$Q) & !is.na(datos$Q_manana) & !is.na(datos$Q_ayer)

entrenamiento <- subset(datos, usable & fecha <= as.Date("2011-12-30"))
prueba        <- subset(datos, usable & fecha >= as.Date("2012-01-01") &
                          fecha <= as.Date("2012-12-30"))

# error de persistencia en el año de validación (2012)
prueba$pred_pers <- prueba$Q
mae_pers_2012 <- tapply(abs(prueba$pred_pers - prueba$Q_manana), prueba$cuenca, mean)

# Patrón que se repite en cada modelo:
#  1. lm(cambio_manana ~ pistas) ajusta una regresión lineal del cambio de mañana
#  2. Q * exp(predict(...)) deshace el logaritmo: Q mañana = Q hoy * e^cambio
#  3. tapply(...) da el MAE por río, y mejora = 1 - MAE modelo / MAE persistencia
#     (0,15 = 15 % menos error; 0 = empate; negativo = peor que persistencia)


# PRIMER MODELO: solo con comportamiento del río, solo con dos pistas:
# si el rio viene subiendo o bajando y cuánta agua lleva hoy.
m1 <- lm(cambio_manana ~ cambio_hoy + log_Q, data = entrenamiento)
summary(m1)

# interpretación: Si hoy el río subió un 10 % el modelo predice que mañana suba otro 1,4 %
# Por cada unidad, el cambio predicho para mañana es 0,7 puntos porcentuales menor.
# cuanto más agua lleva un río hoy, más tiende a bajar mañana: los ríos crecidos vuelven a su nivel.
# Ojo: el efecto se mide respecto de hoy, no del promedio.
# El R2 es bajo (~0,02), y es esperable: mide cuánto del CAMBIO diario se explica,
# y persistencia explica 0 % del cambio. Lo que importa es la mejora frente a persistencia.

# LE GANA A LA PERSISTENCIA EN 2012?
prueba$pred_m1 <- prueba$Q * exp(predict(m1, newdata = prueba)) # deshacemos el log: Q mañana = Q hoy * e^cambio
mae_m1 <- tapply(abs(prueba$pred_m1 - prueba$Q_manana), prueba$cuenca, mean)

mejora <- 1 - mae_m1 / mae_pers_2012
sort(round(mejora, 3))
median(mejora)       # mediana entre ríos: cada río pesa lo mismo (Decisión 7)
sum(mejora > 0)      # en cuántos ríos le gana a persistencia
# la mediana de la mejora es -0.0023, y en 14 de los 30 ríos el modelo le gana a la persistencia.
# O sea, empata: con un solo coeficiente para los 30 ríos, un log_Q alto puede ser
# "río grande" o "río crecido", y el modelo no los distingue.

# SEGUNDO MODELO: QUE CADA RÍO TENGA SU COMPORTAMIENTO
# cuenca: cada río tiene su propio intercepto (punto de partida)
# cuenca:log_Q: cada río tiene su propia velocidad de bajada después de una crecida
m2 <- lm(cambio_manana ~ cambio_hoy + log_Q + cuenca + cuenca:log_Q, data = entrenamiento)
prueba$pred_m2 <- prueba$Q * exp(predict(m2, newdata = prueba))
mae_m2 <- tapply(abs(prueba$pred_m2 - prueba$Q_manana), prueba$cuenca, mean)

mejora2 <- 1 - mae_m2 / mae_pers_2012
sort(round(mejora2, 3))
median(mejora2)
sum(mejora2 > 0)
length(coef(m2)) # 61 = intercepto + cambio_hoy + log_Q + 29 cuencas + 29 interacciones (un río queda de referencia)

# El avance: de empatar con persistencia pasamos a una mejora del 3 %, y el modelo gana en 21 de 30 ríos

# TERCER MODELO: AGREGAR LLUVIA (mismo efecto para todos los ríos)
# log1p(x) = log(1 + x): casi todos los días la lluvia es 0 (y log(0) no existe)
# y unos pocos días son enormes; el log evita que esos pocos días dominen.
m3 <- lm(cambio_manana ~ cambio_hoy + log_Q + cuenca + cuenca:log_Q +
           log1p(P) + log1p(P_ayer) + log1p(lluvia_3d) + log1p(lluvia_7d) + log1p(lluvia_30d),
         data = entrenamiento)

prueba$pred_m3 <- prueba$Q * exp(predict(m3, newdata = prueba))
mae_m3 <- tapply(abs(prueba$pred_m3 - prueba$Q_manana), prueba$cuenca, mean)
mejora_m3 <- 1 - mae_m3 / mae_pers_2012

median(mejora_m3)
sum(mejora_m3 > 0)

# comparamos m2 (sin lluvia) y m3 (con lluvia) en dos tipos de río
rios <- c("7116001", "8130002", "8117005",   # chicos de lluvia: Upeo, Diguillín, Chillán
          "10111001", "9437002")             # salen de un lago: San Pedro, Toltén
round(cbind(m2 = mejora2[rios], m3 = mejora_m3[rios]), 3) #En los tres ríos chicos de lluvia el salto es grande

# la lluvia sube la mediana (de 3,3 % a 4,7 %), pero el modelo gana en menos ríos (de 21 a 19).
# O sea, la lluvia ayuda mucho en algunos ríos y perjudica en otros.

# el modelo aprende "si llueve 20 mm, el río sube tanto por ciento",
# eso influye más en esteros chicos, pero no en un río que sale de un lago
# porque el lago absorbe la lluvia y el río casi no se mueve.
# (San Pedro pasa de 14 % a -196 % y Toltén de 5 % a -54 %)

# CUARTO MODELO: que cada río responda a la lluvia a su manera
m4 <- lm(cambio_manana ~ cambio_hoy + log_Q + cuenca + cuenca:log_Q +
           log1p(P) + log1p(P_ayer) + log1p(lluvia_3d) + log1p(lluvia_7d) + log1p(lluvia_30d) +
           cuenca:log1p(P) + cuenca:log1p(P_ayer) + cuenca:log1p(lluvia_3d),
         data = entrenamiento)

# la última línea agrega las interacciones: cada río tiene su propia respuesta a la lluvia de hoy,
# de ayer y de 3 días. Se aplican solo a la lluvia de corto plazo, que es donde los ríos
# más se diferencian, para no inflar demasiado el número de coeficientes.

prueba$pred_m4 <- prueba$Q * exp(predict(m4, newdata = prueba))
mae_m4 <- tapply(abs(prueba$pred_m4 - prueba$Q_manana), prueba$cuenca, mean)
mejora_m4 <- 1 - mae_m4 / mae_pers_2012

median(mejora_m4) #m4 se equivoca un 11 % menos que persistencia
sum(mejora_m4 > 0)  # gana en 26 ríos
length(coef(m4))    # 153 = 61 de m2 + 5 de lluvia + 3 x 29 interacciones con la lluvia
                    # (con ~130.000 filas de entrenamiento: más de 800 filas por coeficiente)

# comparamos los cinco ríos en los tres modelos
round(cbind(m2 = mejora2[rios], m3 = mejora_m3[rios], m4 = mejora_m4[rios]), 3)
# los ríos de lago se recuperan (San Pedro +20 %, Toltén +24 %) y los chicos mantienen
# su ganancia: la heterogeneidad entre cuencas no es un detalle.


# BLOQUE 6. VALIDACIÓN EN 7 VENTANAS (sección 8 del informe) #
# Objetivo: repetir la comparación en cada año de 2006 a 2012, para ver si la
# mejora es estable o fue suerte de un año. El entrenamiento siempre parte en
# 2000 y crece ("ventanas de origen creciente", como pide el enunciado).
# Se usan 7 ventanas y no 3 porque 2010-2012 son años secos, y elegir el modelo
# solo con ellos lo sesgaría hacia años secos. (Decisión 8 del informe)

# Guardar las fórmulas en una lista, para no reescribirlas en cada ventana.
# Son las cuatro de arriba, más m5 y m6, que agregan temperatura y mes:
formulas <- list(
  m1 = cambio_manana ~ cambio_hoy + log_Q,
  m2 = cambio_manana ~ cambio_hoy + log_Q + cuenca + cuenca:log_Q,
  m3 = cambio_manana ~ cambio_hoy + log_Q + cuenca + cuenca:log_Q +
    log1p(P) + log1p(P_ayer) + log1p(lluvia_3d) + log1p(lluvia_7d) + log1p(lluvia_30d),
  m4 = cambio_manana ~ cambio_hoy + log_Q + cuenca + cuenca:log_Q +
    log1p(P) + log1p(P_ayer) + log1p(lluvia_3d) + log1p(lluvia_7d) + log1p(lluvia_30d) +
    cuenca:log1p(P) + cuenca:log1p(P_ayer) + cuenca:log1p(lluvia_3d),
  # m5 = m4 + temperatura de hoy, de la última semana y mes (166 coeficientes)
  m5 = cambio_manana ~ cambio_hoy + log_Q + cuenca + cuenca:log_Q +
    log1p(P) + log1p(P_ayer) + log1p(lluvia_3d) + log1p(lluvia_7d) + log1p(lluvia_30d) +
    cuenca:log1p(P) + cuenca:log1p(P_ayer) + cuenca:log1p(lluvia_3d) +
    Temp + temp_7d + mes,
  # m6 = m5 + respuesta a la temperatura propia de cada río (195 coeficientes)
  m6 = cambio_manana ~ cambio_hoy + log_Q + cuenca + cuenca:log_Q +
    log1p(P) + log1p(P_ayer) + log1p(lluvia_3d) + log1p(lluvia_7d) + log1p(lluvia_30d) +
    cuenca:log1p(P) + cuenca:log1p(P_ayer) + cuenca:log1p(lluvia_3d) +
    Temp + temp_7d + mes + cuenca:temp_7d
)

# Para cada año de 2006 a 2012, arma el entrenamiento y la validación,
# calcula el error de persistencia y ajusta los seis modelos
# (7 años x 6 modelos = 42 ajustes; tarda unos minutos):
resultados <- data.frame()

for (anio in 2006:2012) {
  # entrenamiento: desde 2000 hasta el 30 de diciembre del año anterior
  entrenamiento <- subset(datos, usable & fecha <= as.Date(paste0(anio - 1, "-12-30")))
  # validación: el año completo, hasta el 30 de diciembre
  validacion <- subset(datos, usable & fecha >= as.Date(paste0(anio, "-01-01")) &
                         fecha <= as.Date(paste0(anio, "-12-30")))

  # la vara de ese año: error de persistencia por río
  mae_pers <- tapply(abs(validacion$Q - validacion$Q_manana), validacion$cuenca, mean)

  for (nombre in names(formulas)) {
    modelo <- lm(formulas[[nombre]], data = entrenamiento)
    pred <- validacion$Q * exp(predict(modelo, newdata = validacion))
    mae_modelo <- tapply(abs(pred - validacion$Q_manana), validacion$cuenca, mean)

    # una fila por año, modelo y río
    resultados <- rbind(resultados, data.frame(
      anio = anio, modelo = nombre, cuenca = names(mae_modelo),
      mejora = 1 - mae_modelo / mae_pers
    ))
  }
  cat("Listo", anio, "\n")
}

# resumir. Dos tablas de años por modelos:

# mediana de la mejora entre ríos
round(tapply(resultados$mejora, list(resultados$anio, resultados$modelo), median), 3)

# en cuántos ríos gana cada modelo
tapply(resultados$mejora > 0, list(resultados$anio, resultados$modelo), sum)

# La fila de 2012 coincide con el bloque 5: el bucle hace lo mismo que el código paso a paso.
# m4 tiene la mejor mediana en 6 de 7 años y nunca es negativo (4,4 % a 18,8 %):
# es el modelo elegido (Decisión 9 del informe).
#O sea, hay años en que agregar lluvia ayuda mucho y años en que estorba.

#Esto calcula la lluvia anual promedio entre las 30 cuencas (mm al año):

anio_dato <- format(datos$fecha, "%Y")
lluvia_anual <- tapply(datos$P, anio_dato, sum) / 30
round(lluvia_anual)
# los años más secos de la validación (2007 y 2010) son los más difíciles.
# 2013-2016 también son secos: en la prueba final conviene esperar una mejora más
# cercana a la de 2010-2012 que al promedio.

# ¿La temperatura ayuda en los ríos de nieve de Santiago?
nieve <- c("5721001", "5722001", "5722002", "5710001")   # Yerba Loca, Arrayán, Almendros, Maipo
r_nieve <- subset(resultados, cuenca %in% nieve & modelo %in% c("m4", "m5", "m6"))
round(tapply(r_nieve$mejora, list(r_nieve$cuenca, r_nieve$modelo), median), 3)
# temperatura y mes ayudan en años lluviosos y empeoran en los secos; en promedio
# empatan con m4 (12,6 % y 12,8 % frente a 12,5 %) y no arreglan los ríos de nieve.
# Por eso no entran al modelo final (Decisión 10 del informe, pendiente de confirmar
# con el grupo).


# BLOQUE 7. DÓNDE FALLA m4, RMSE Y REFERENCIA ESTACIONAL (sección 9 del informe) #

# DÓNDE GANA Y DÓNDE PIERDE m4
# resumimos por río la mejora de m4 en los 7 años y en cuántos años gana
r4 <- subset(resultados, modelo == "m4")

tabla_rios <- data.frame(
  cuenca         = names(tapply(r4$mejora, r4$cuenca, median)),
  mejora_mediana = round(tapply(r4$mejora, r4$cuenca, median), 3),
  anios_que_gana = tapply(r4$mejora > 0, r4$cuenca, sum)   # de 0 a 7
)

# agregamos nombre, área, altura media y embalse. Los atributos se usan aquí
# solo para DESCRIBIR los resultados, no como pistas del modelo.
atributos$cuenca <- as.character(atributos$gauge_id)
tabla_rios <- merge(tabla_rios,
                    atributos[, c("cuenca", "gauge_name", "area", "elev_mean", "big_dam")],
                    by = "cuenca")

tabla_rios[order(tabla_rios$mejora_mediana), ]
# pierden las cuencas altas de Chile central (nieve, y varias reguladas);
# ganan los 7 años los ríos de lluvia de baja altura

# gráfico: altura de la cuenca contra mejora de m4 (naranjo = con embalse grande)
plot(tabla_rios$elev_mean, tabla_rios$mejora_mediana,
     col = ifelse(tabla_rios$big_dam == 1, "darkorange", "steelblue"), pch = 19,
     xlab = "Altura media de la cuenca (m)",
     ylab = "Mejora de m4 frente a persistencia (mediana 2006-2012)")
abline(h = 0, lty = 2)   # sobre la línea: m4 le gana a persistencia
legend("bottomleft", legend = c("Sin embalse grande", "Con embalse grande"),
       col = c("steelblue", "darkorange"), pch = 19)
# la mejora baja con la altura y cruza el cero cerca de los 1500 m

# agregamos RMSE y referencia estacional
# RMSE: castiga más los errores grandes, que ocurren en las crecidas.
# Referencia estacional: caudal típico (mediana) de cada río en el mes de mañana,
# calculado solo con el entrenamiento. No usa el caudal de hoy.

datos$mes_manana <- as.integer(format(datos$fecha + 1, "%m"))   # mes del día que se predice

resultados_m4 <- data.frame()

for (anio in 2006:2012) {
  entrenamiento <- subset(datos, usable & fecha <= as.Date(paste0(anio - 1, "-12-30")))
  validacion <- subset(datos, usable & fecha >= as.Date(paste0(anio, "-01-01")) &
                         fecha <= as.Date(paste0(anio, "-12-30")))

  # las tres predicciones
  modelo <- lm(formulas$m4, data = entrenamiento)
  validacion$pred_m4   <- validacion$Q * exp(predict(modelo, newdata = validacion))
  validacion$pred_pers <- validacion$Q

  # referencia estacional: mediana del caudal de mañana por río y mes, solo con entrenamiento
  tipico <- aggregate(Q_manana ~ cuenca + mes_manana, data = entrenamiento, FUN = median)
  names(tipico)[3] <- "pred_est"
  validacion <- merge(validacion, tipico, by = c("cuenca", "mes_manana"))   # le pega a cada día su caudal típico

  # errores por río (dias = número de días evaluados, que hay que reportar)
  for (cu in unique(validacion$cuenca)) {
    d <- validacion[validacion$cuenca == cu, ]
    resultados_m4 <- rbind(resultados_m4, data.frame(
      anio = anio, cuenca = cu, dias = nrow(d),
      MAE_pers  = mean(abs(d$pred_pers - d$Q_manana)),
      MAE_m4    = mean(abs(d$pred_m4   - d$Q_manana)),
      MAE_est   = mean(abs(d$pred_est  - d$Q_manana)),
      RMSE_pers = sqrt(mean((d$pred_pers - d$Q_manana)^2)),
      RMSE_m4   = sqrt(mean((d$pred_m4   - d$Q_manana)^2))
    ))
  }
  cat("Listo", anio, "\n")
}

# mejora frente a persistencia, con la misma fórmula de siempre
resultados_m4$mejora_MAE  <- 1 - resultados_m4$MAE_m4  / resultados_m4$MAE_pers
resultados_m4$mejora_RMSE <- 1 - resultados_m4$RMSE_m4 / resultados_m4$RMSE_pers
resultados_m4$mejora_est  <- 1 - resultados_m4$MAE_est / resultados_m4$MAE_pers

round(cbind(
  MAE  = tapply(resultados_m4$mejora_MAE,  resultados_m4$anio, median),
  RMSE = tapply(resultados_m4$mejora_RMSE, resultados_m4$anio, median),
  estacional = tapply(resultados_m4$mejora_est, resultados_m4$anio, median)
), 3)
# - la columna MAE coincide con m4 en el bloque 6
# - la mejora en RMSE es mayor y más estable (13-19 %, promedio 15,4 %): el aporte
#   de m4 se concentra en las crecidas, justo donde persistencia falla
# - la referencia estacional pierde por mucho (su error es 2,3 a 5,5 veces el de
#   persistencia): conocer cómo está el río hoy es la información que más pesa


# PENDIENTE (sección 11 del informe): caudales altos y bajos, sensibilidad con
# la respuesta en m3/s, sensibilidad con atributos físicos en vez del código de
# cuenca, y prueba final 2013-2016 (una sola vez, reentrenando m4 con 2000-2012).
