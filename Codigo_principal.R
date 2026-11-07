# Proyecto Tópicos Aplicados - Caso 01: caudal del día siguiente (CAMELS-CL)
#
# Para correrlo: guardar este script en la misma carpeta que los 4 csv
# (caudal_m3s.csv, precipitacion_mm.csv, temperatura_media_c.csv, atributos.csv)
# y en RStudio ir a Session -> Set Working Directory -> To Source File Location.


# REVISIÓN DE LOS DATOS #

caudal <- read.csv("caudal_m3s.csv", check.names = FALSE)

head(caudal)        # cuántas filas y columnas tiene
caudal[1:5, 1:6]       # las primeras 5 filas y 6 columnas
str(caudal[, 1:6])     # qué tipo de dato es cada columna
sum(is.na(caudal))     # cuántos datos faltantes detecta R

caudal$fecha <- as.Date(caudal$fecha)

range(caudal$fecha)          # primera y última fecha
table(diff(caudal$fecha))    # días entre una fila y la siguiente

faltantes <- colSums(is.na(caudal[, -1]))
sort(faltantes, decreasing = TRUE)

# revisaremos el río con más faltantes
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
# reportar cuántos días se evaluaron por río.

precipitacion <- read.csv("precipitacion_mm.csv", check.names = FALSE)
temperatura   <- read.csv("temperatura_media_c.csv", check.names = FALSE)

dim(precipitacion)
dim(caudal)
dim(temperatura)
sum(is.na(precipitacion))
sum(is.na(temperatura))

names(caudal)[2:6]
names(precipitacion)[2:6]

#efectivamente tienen el mismo tamaño que la tabla caudal. No les faltan datos.
# Notamos que los nombres de las tablas no están ordenados de la misma manera,
# por lo que no podemos simplemente pegar por posición.


# UNA FILA POR RÍO Y DÍA #

cuencas <- names(caudal)[-1]

caudal_largo <- data.frame(
  cuenca = rep(cuencas, each = nrow(caudal)),
  fecha  = rep(caudal$fecha, times = length(cuencas)),
  Q      = unlist(caudal[, -1], use.names = FALSE)
)

dim(caudal_largo) #186300 filas: combinaciones de rio y día (30x6210)
head(caudal_largo)
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

# lo mismo con temperatura
temperatura$fecha <- as.Date(temperatura$fecha)
cuencas_t <- names(temperatura)[-1]
temperatura_largo <- data.frame(
  cuenca = rep(cuencas_t, each = nrow(temperatura)),
  fecha  = rep(temperatura$fecha, times = length(cuencas_t)),
  Temp   = unlist(temperatura[, -1], use.names = FALSE)
)

# unimos las tres tablas
datos <- merge(caudal_largo, precipitacion_largo, by = c("cuenca", "fecha"))
datos <- merge(datos, temperatura_largo, by = c("cuenca", "fecha"))

# solo para probar que haya salido bien
dim(datos)
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
# a esas columnas.
datos$Q[which(datos$Q == 0)] <- NA


# LO QUE QUEREMOS PREDECIR: EL CAUDAL DE MAÑANA #

# Ahora, a cada fila le agregamos el caudal del día siguiente del mismo
# río. Primero ordenamos por río, y dentro de cada río por fecha. Así,
# cada fila tendrá abajo el "mañana"

datos <- datos[order(datos$cuenca, datos$fecha), ]
rownames(datos) <- NULL

datos$Q_manana <- c(datos$Q[-1], NA) # todo corrido una posición hacia arriba,
# con un NA final para que tenga el mismo largo.

# código para visualizar que hay un error: "mañana" solo vale si la
# fila siguiente es del mismo río. Si el río cambia se pone NA
datos[6209:6212, c("cuenca", "fecha", "Q", "Q_manana")]
datos[12419:12422, c("cuenca", "fecha", "Q", "Q_manana")]

# Arreglamos el problema:
ultimo_dia <- c(datos$cuenca[-1] != datos$cuenca[-nrow(datos)], TRUE)
datos$Q_manana[ultimo_dia] <- NA

# volvamos a mirar los datos que estaban mal
datos[6209:6212, c("cuenca", "fecha", "Q", "Q_manana")]
datos[12419:12422, c("cuenca", "fecha", "Q", "Q_manana")]
sum(ultimo_dia) # debe dar 30: el último día de cada uno de los 30 ríos
# ahora sí está todo bien


# AHORA, ORDENAREMOS LAS PISTAS #

# 1. El caudal de ayer: con el de hoy, indica si el río viene subiendo o bajando.
# Para "mañana" corrimos la columna hacia arriba y el problema estaba en el último día de cada río.
# Para "ayer" hay que correrla hacia abajo, y el problema estará en el primer día de cada río,
# el 2000-01-01, que no tiene ayer.

datos$Q_ayer <- c(NA, datos$Q[-nrow(datos)])
primer_dia <- c(TRUE, datos$cuenca[-1] != datos$cuenca[-nrow(datos)])
datos$Q_ayer[primer_dia] <- NA

# 2. La lluvia acumulada: la lluvia de un solo día no basta para entender un río.
# las ventanas de varios días miden cosas distintas.
# Lluvia de hoy y de ayer: la respuesta rápida, porque un río chico sube el mismo día.
# Últimos 3 o 7 días: el temporal completo.
# Últimos 30 días: qué tan mojado está el suelo. La misma lluvia produce más caudal
# si el suelo ya está saturado.

# no usamos la lluvia de mañana ;) pq sería info futura
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

# comprobación a mano: las dos líneas deben dar lo mismo
subset(datos$lluvia_7d, datos$cuenca == "8130002" & datos$fecha == as.Date("2000-06-10"))
sum(subset(datos, cuenca == "8130002" & fecha >= as.Date("2000-06-04") & fecha <= as.Date("2000-06-10"))$P)

# ahora, lluvia 3 días y 30 días
datos$lluvia_3d <- suma_ultimos(datos$P, 3)
datos$lluvia_3d[datos$fecha < as.Date("2000-01-01") + 2] <- NA

datos$lluvia_30d <- suma_ultimos(datos$P, 30)
datos$lluvia_30d[datos$fecha < as.Date("2000-01-01") + 29] <- NA

# ahora usamos la lluvia de ayer
datos$P_ayer <- c(NA, datos$P[-nrow(datos)])
datos$P_ayer[primer_dia] <- NA

datos$temp_7d <- suma_ultimos(datos$Temp, 7) / 7 # temp media de los ultimos 7 días
datos$temp_7d[datos$fecha < as.Date("2000-01-01") + 6] <- NA

datos$mes <- factor(as.integer(format(datos$fecha, "%m")))


# AHORA VEREMOS LA VARA A SUPERAR: PERSISTENCIA #
# veremos cuánto se equivoca persistencia ("mañana igual que hoy")
datos$usable <- !is.na(datos$Q) & !is.na(datos$Q_manana) # marca los días que existe el caudal de hoy y mañana (los únicos en los que se puede medir el error de persistencia)

desarrollo <- subset(datos, usable & fecha <= as.Date("2012-12-30")) # el corte es 30-12 porque la respuesta del 31 de dic es el 1 de enero 2013 que pertenece a la prueba final

desarrollo$error_pers <- desarrollo$Q - desarrollo$Q_manana #predicción - valor real
mae_pers <- tapply(abs(desarrollo$error_pers), desarrollo$cuenca, mean)
sort(mae_pers)

# Que el Maule tenga un MAE de 110 y el Yerba Loca uno de 0,08 dice sobre todo
# cuánta agua lleva cada río, no qué tan bien funciona persistencia en él.

# Para comparar ríos hay que mirar el error en relación con el tamaño de cada uno.

media_Q <- tapply(desarrollo$Q, desarrollo$cuenca, mean)
mae_relativo <- mae_pers / media_Q
sort(mae_relativo)

# en los ríos en que la persistencia se equivoca menos es en el 10111001 con 0.0331,
# y en el que se equivoca más es en el 7116001 con 0.3784

# ahora, veremos qué atributos tienen esos ríos
atributos <- read.csv("atributos.csv", check.names = FALSE)

orden <- sort(mae_relativo)
extremos <- names(c(head(orden, 3), tail(orden, 3)))

atributos[atributos$gauge_id %in% extremos, c("gauge_id", "gauge_name", "area", "big_dam")]

# podemos ver que los tres donde más se equivoca son los que tienen un
# área considerablemente menor. En los que menos se equivoca es en aquellos
# que tienen un área considerablemente mayor.

# Tiene sentido físico:

#Río chico: cuando llueve, el agua llega rápido al cauce y el río puede duplicarse de un día a otro.
# "Mañana igual que hoy" falla mucho.
#Río grande: el agua recorre una cuenca enorme y llega de a poco, así que el caudal cambia lento y
# persistencia acierta casi siempre.

# Donde un modelo con lluvia tiene más espacio para mejorar es en los ríos chicos
# de lluvia, sin lagos ni embalses


# PREPARAMOS EL MODELO #

# comparamos con y sin logaritmo: hay mucha asimetría, dominarán los ríos que llevan
# más agua, y la proporción de cambio si son dos ríos con muy distinto caudal,
# será distinto. Al ponerlo en logaritmo se suaviza un poco.
par(mfrow = c(1, 2))
hist(desarrollo$Q, breaks = 100, main = "Caudal (m3/s)", xlab = "Q")
hist(log(desarrollo$Q), breaks = 100, main = "Logaritmo del caudal", xlab = "log(Q)")
par(mfrow = c(1, 1))

# trabajaremos en logaritmo
datos$log_Q         <- log(datos$Q)
datos$cambio_hoy    <- log(datos$Q) - log(datos$Q_ayer)
datos$cambio_manana <- log(datos$Q_manana) - log(datos$Q)

summary(datos$log_Q)          # ya no aparece -Inf: el caudal 0 se corrigió arriba
summary(datos$cambio_manana)


# SEPARAMOS ENTRENAMIENTO Y PRUEBA #
# el modelo necesita Q_ayer para calcular cambio_hoy, así que para comparar
# modelo y persistencia en los mismos días agregamos esa condición a usable
datos$usable <- !is.na(datos$Q) & !is.na(datos$Q_manana) & !is.na(datos$Q_ayer)

entrenamiento <- subset(datos, usable & fecha <= as.Date("2011-12-30"))
prueba        <- subset(datos, usable & fecha >= as.Date("2012-01-01") &
                          fecha <= as.Date("2012-12-30"))

# error de persistencia en el año de prueba (2012)
prueba$pred_pers <- prueba$Q
mae_pers_2012 <- tapply(abs(prueba$pred_pers - prueba$Q_manana), prueba$cuenca, mean)


# PRIMER MODELO: solo con comportamiento del río, solo con dos pistas:
# si el rio viene subiendo o bajando y cuánta agua lleva hoy.
m1 <- lm(cambio_manana ~ cambio_hoy + log_Q, data = entrenamiento)
summary(m1)

# interpretación: Si hoy el río subió un 10 % el modelo predice que mañana suba otro 1,4 %
# Por cada unidad, el cambio predicho para mañana es 0,7 puntos porcentuales menor.
# cuanto más agua lleva un río hoy, más tiende a bajar mañana: los ríos crecidos vuelven a su nivel.
# Ojo: el efecto se mide respecto de hoy, no del promedio.

# LE GANA A LA PERSISTENCIA EN 2012?
prueba$pred_m1 <- prueba$Q * exp(predict(m1, newdata = prueba)) # deshacemos el log: Q mañana = Q hoy * e^cambio
mae_m1 <- tapply(abs(prueba$pred_m1 - prueba$Q_manana), prueba$cuenca, mean)

mejora <- 1 - mae_m1 / mae_pers_2012
sort(round(mejora, 3))
median(mejora)
sum(mejora > 0)
# la mediana de la mejora es -0.0023, y en 14 de los 30 ríos el modelo le gana a la persistencia.

# SEGUNDO MODELO: QUE CADA RÍO TENGA SU COMPORTAMIENTO
m2 <- lm(cambio_manana ~ cambio_hoy + log_Q + cuenca + cuenca:log_Q, data = entrenamiento)
prueba$pred_m2 <- prueba$Q * exp(predict(m2, newdata = prueba))
mae_m2 <- tapply(abs(prueba$pred_m2 - prueba$Q_manana), prueba$cuenca, mean)

mejora2 <- 1 - mae_m2 / mae_pers_2012
sort(round(mejora2, 3))
median(mejora2)
sum(mejora2 > 0)
length(coef(m2)) # 61 = intercepto + cambio_hoy + log_Q + 29 cuencas + 29 interacciones (un río queda de referencia)

# El avance: de empatar con persistencia pasamos a una mejora del 3 %, y el modelo gana en 21 de 30 ríos

# TERCER MODELO: AGREGAR LLUVIA:
m3 <- lm(cambio_manana ~ cambio_hoy + log_Q + cuenca + cuenca:log_Q +
           log1p(P) + log1p(P_ayer) + log1p(lluvia_3d) + log1p(lluvia_7d) + log1p(lluvia_30d),
         data = entrenamiento)

prueba$pred_m3 <- prueba$Q * exp(predict(m3, newdata = prueba))
mae_m3 <- tapply(abs(prueba$pred_m3 - prueba$Q_manana), prueba$cuenca, mean)
mejora_m3 <- 1 - mae_m3 / mae_pers_2012

median(mejora_m3)
sum(mejora_m3 > 0)
