################################################################################
#  Curso: Modelos Computacionales con Aplicaciones en Demografía               #
#  Daniel Ciganda                                                              #   
#  lab 4: Modelos del proceso reproductivo                                   #
################################################################################
library(data.table); library(lubridate); library(ggplot2)

#install.packages("devtools")
#devtools::install_github("cardiomoon/ggiraphExtra")

##################################
##################################
#  ANALISIS 1era PARTE           #      
##################################
##################################
# La idea es explorar como las distintas combinaciones de parámetros afectan el resultado del modelo.
# Durante este ejercicio nos vamos a concentrar en dos parámetros:
# la edad media a la unión y el punto de inflexion del declive de la fecundabilidad -.

mau <- 24      # mean age at union
alpha <- 36    # age fecundabiliy decline

source("comfert_tfr.R")
source("plots.R")

tfr <- comfert_tfr(mau, alpha)

# Una primera aproximacion es variar un parametro mientras mantenemos el otro constante.
mau_vals <- seq(18, 30, 2)
start <- Sys.time()
out_1 <- sapply(mau_vals, function(x) comfert_tfr(x, alpha))
end <- Sys.time()
print(end-start)

# plot
plot(mau_vals, out_1, main = paste("Resultados Edad Media a la Unión","-",
                                   "Período No Sus. fijo en 6m"),
     ylab = "TFR", xlab = "Edad Media Unión")

# Ahora variamos el punto de inflexión de fec. dejando constante la edad a la unión.
alpha_vals <- seq(30, 42, 2) 
start <- Sys.time()
out_2 <- sapply(alpha_vals, function(x) comfert_tfr(mau, x))
end <- Sys.time()
print(end-start)

# Grafiquemos los resultados
plot(alpha_vals, out_2, main = paste("Resultados Age Decline Fec.","-",
                                     "Edad Media a la Unión fija en 24"),
     ylab = "TFR", xlab = "Age Fec. Decline")

# Grafiquemos el area en el espacio de parametros que estuvimos explorando 
mau <- c(mau_vals, rep(mau, length(alpha_vals)))
alpha <- c(rep(alpha, length(mau_vals)), alpha_vals)

design <- cbind(mau, alpha)
plot(design)

# Para exlplorar una mayor parte de la superficie del espacio usamos un
# diseño factorial.
d_fact <- expand.grid(mau = mau_vals, alpha = alpha_vals)
plot(d_fact)

# La idea es mapear las 49 combinaciones en d_fact al resultado del modelo 
# es decir, la Tasa Glogal de Fecundidad
# Correr el modelo todas estas veces lleva tiempo asi que vamos a computar en paralelo

# llamar "parallel" y definir el cluster
library(parallel)
crs <- detectCores()
cl <- makeCluster(crs, type = "PSOCK", outfile = "parallel_comfert.txt")

# exportar parametros
param_list <- split(d_fact, sort(1:dim(d_fact)[1] %% crs))
clusterExport(cl, "param_list")

clusterEvalQ(cl, library(lubridate))
clusterEvalQ(cl, library(data.table))

# exportar funcion
invisible(
  clusterCall(cl, function() {
    source("Dia 4/comfert_tfr.R") 
  }))

out <- parLapply(cl, param_list, function(x) mapply(comfert_tfr, x$mau, x$alpha))
stopCluster(cl)

d_fact$out <- unlist(out)

# Grafiquemos las tres dimensiones
persp(
  # tomamos los valores unicos de cada factor
  unique(d_fact$mau),
  unique(d_fact$alpha), 
  # creamos una matriz con los outputs
  matrix(d_fact$out, nrow = length(unique(d_fact$mau))),
  xlab = "Mean Age",
  ylab = "Age Fec. Decline",
  zlab = "TFR",
  # parametros graficos
  theta = 20,
  phi = 20,
  shade = 0.4,
  col = "orange"
)

plot_3D <- function(x){
  persp(
    # tomamos los valores unicos de cada factor
    unique(d_fact$mau),
    unique(d_fact$alpha), 
    # creamos una matriz con los outputs
    matrix(d_fact$out, nrow = length(unique(d_fact$mau))),
    xlab = "Mean Age",
    ylab = "Age Fec. Decline",
    zlab = "TFR",
    # parametros graficos
    theta = x,
    phi = 20,
    shade = 0.4,
    col = "orange"
  )
  Sys.sleep(1)
}

lapply(seq(30, 360, 10), plot_3D)


filled.contour(unique(d_fact$mau),
               unique(d_fact$alpha), 
               matrix(d_fact$out, nrow = length(unique(d_fact$mau))),
               xlab = "Mean Age Union",
               ylab = "Age Decline Fec.",
               main = "TFR",
               cex.main = 0.9)


####################################################
############ ANALISIS 2da PARTE ####################
# Los algoritmos graficos que usamos antes realizan una
# interpolacion entre los puntos donde existe información utilizando un meta-modelo simple, es decir un 
# modelo de la relacion inputs-outputs.
# Un meta-modelo nos permite, entre otras cosas, obtener predicciones para combinaciones de parámetros
# sin tener que computar el modelo en esas combinaciones.
# El output en las combinaciones que no observamos es aproximado por una funcion. El objetivo
# es obtener una funcion que no imponga demasiadas restricciones a la forma de la relacion inputs-outputs.

# Un ejemplo: ajustemos un modelo lineal simple a la relacion inputs-outputs
# En R los modelos lineales se ajustan con la funcion lm(), ej. lm(y ~ x_1 + x_k)
lm_model <- with(d_fact, lm(out ~ mau + alpha))
# Revisemos los resultados con summary()
summary(lm_model)

require(ggiraph)
require(ggiraphExtra)
require(plyr)
ggPredict(lm_model,interactive=TRUE)

# Otra forma de comprobar que el modelo representa adecuadamente la superficie que define la relacion entre
# inputs y outputs es generando predicciones con el modelo en nuevas combinaciones de parametros y comparandolas
# con los valores reales del output en esas combinaciones (es decir, valores computados con el modelo.)

# Obtener 10 valores aleatorios de los parametros. Redondear los valores obtenidos a 2 lugares y guardarlos en un 
# data frame "new_points" donde la columnas tienen que tener el mismo nombre que los predictores en el modelo lineal.


new_points <- data.frame(alpha=round(runif(10,30,42),2),mau=round(runif(10,30,42),2))

# Obtener el valor del output para las diez nuevas combinaciones y asignarlos a un vector "new_out"
new_out <- mapply(comfert_tfr, mau = new_points$mau, alpha = new_points$alpha)

# Usando predict() generar predicciones en los nuevos inputs y asignar a "model predictions".

model_predictions <- predict(lm_model,new_points)

# calcular "error", la distancia entre la prediccion y new_out

MSE <- (model_predictions-new_out)^2

# calcular la raiz del error cuadratico medio "rmse": raiz cuadrada de la media de los errores

RMSE <- mean(sqrt(MSE))

# dividir entre el rango de las respuestas para obtener un rmse normalizado que 
# exprese el porcentaje de varianza residual (no explicada)

RMSE/diff(range(d_fact$out))

# Ahora vamos a validar el metamodelo evaluando su capacidad de predecir el resultado del modelo en 
# en nuevas combinaciones de parámetros (cross-validation).
# La idea es comparar las predicciones del meta-modelo contra:
# * Los resultados del modelo observados (computados) en cada una de las combinaciones
#   de parámetros que usamos para ajustar el modelo (training set)
# * Los resultados del modelo obtenidos para las nuevas combinaciones de parámetros (test set)

# Para esto vamos a necesitar un data.frame que tengan los parametros, resultados (out)
# y predicciones para los datos con los que ajustamos el meta-modelo y otro igual
# para las nuevas locaciones para las que obtuvimos predicciones.

pred_out <- predict(lm_model, d_fact[1:2])
original_data <- cbind(d_fact, pred_out)
new_data <- cbind(new_points, new_out, model_predictions)
names(new_data) <- names(original_data)

# Plot: Graficar la predicción contra los resultados observados en el training set
# y superponer esta relación para el test set en otro color.
# Los resultados y sus predicciones (de ambos conjuntos de puntos)
# deberian estar alineados en la diagonal central del grafico.



# Otra forma de evaluar la capacidad del meta-modelo de representar la relacion entre inputs y outputs
# es compararla con la superficie que obtuvimos con el disenio factorial y que visualizamos
# con el mapa de calor

# Empecemos por generar el espacio de parametros
new_mau <- seq(range(mau_vals)[1],range(mau_vals)[2],length.out = 50)
new_alpha <- seq(range(alpha_vals)[1],range(alpha_vals)[2], length.out = 50)
new_param_space <- data.frame(expand.grid(new_mau, new_alpha))
dim(new_param_space)
plot(new_param_space)
colnames(new_param_space) <- colnames(d_fact[,1:2])
head(new_param_space)

# Generemos las predicciones

prediccion <- predict(lm_model,new_param_space)

# y el grafico

#...


##############################
# ANALISIS DE SENSIBILIDAD   #
##############################
# El analisis de sensibilidad busca identificar el efecto de cada parametro en el output.
# Esto puede hacerse obteniendo la proporcion de la varianza explicada correspondiente a cada predictor en el 
# modelo. En R la funcion anova() nos proporciona esta decomposicion.
# Obtener la proporcion de la varianza explicada correspondiente a cada termino en nuestro meta-modelo.
scale_zo <- function(x){
  (x-min(x))/(max(x)-min(x))
}

d_fact_scaled <- as.data.frame(apply(d_fact, 2, scale_zo))
lm_model_scaled <- with(d_fact_scaled, lm(out ~ mau + alpha))
summary(lm_model_scaled)


model_an <- anova(lm_model)
model_an["Sum Sq"]/sum(model_an["Sum Sq"])

####################################################
############ MUESTRA LHS Y MODELO NO PARAMÉTRICO ###
# En el trabajo previo empezamos a ajustar un meta-modelo para representar la relacion 
# inputs-outputs en nuestro modelo comfert_tfr. Los meta-modelos son utiles porque nos permiten 
# explorar el espacio de parametros a un menor costo, realizar analisis de sensibilidad y estimar
# los parametros del modelo utilizando datos observados sobre el proceso que estamos modelando.
# Pero el nivel de precision de todas esta operaciones va a depender de la precision con la que nuestro meta-modelo
# represente la relacion inputs-outputs.

# Otra forma de reducir el tiempo de computación es explorar el espacio de parámetros con un enfoque alternativo 
# a un disenio factorial.

# El paquete "lhs" nos permite obtener una muestra eficiente de nuestro espacio de parametros (latin hypercube sample).
# A diferencia del muestreo aleatorio este procedimiento tiene en cuenta las posiciones de cada muestra
# para garantizar uan cobertura de todo el espacio.

# obtener una muestra de 30 combinaciones de parametros utilizando improvedLHS() y guardar en un data.frame "d_lhs"
library(lhs)
d_lhs <- data.frame(round(improvedLHS(20, 2),2))

# escalar los valores para que esten en el rango de los parametros y graficar las combinacioens de parametros a explorar
descale <- function(x, target_min, target_max){
  x * (target_max - target_min) + target_min 
}

d_lhs[,1] <- round(descale(d_lhs[,1], range(mau_vals)[1],range(mau_vals)[2]),2)
d_lhs[,2] <- round(descale(d_lhs[,2], range(alpha_vals)[1], range(alpha_vals)[2]),2)

plot(d_lhs)
points(d_fact, col = "red")

# cambiar los nombres a los que estabamos utilizando hasta ahora.
colnames(d_lhs) <- names(new_points)

# correr el modelo en las combinaciones definidas en d_lhs
lhs_out <- mapply(comfert_tfr, d_lhs$mau, d_lhs$alpha)

# agregar el resultado a d_lhs
d_lhs$out <- lhs_out

# Ahora vamos a aproximarnos a la interpolacion usando un proceso gaussiano
# que es un modelo no parametrico y por tanto mas flexible, es decir nos va a permitir 
# representar mejor la superficie irregular con la que estamos trabajando.

# Empecemos por installar el paquete "mlegp"
library("mlegp")

# ajustar el modelo usando la funcion mlegp
gp <- mlegp(d_lhs[1:2], d_lhs[,3], nugget = 2)

# predecir los resultados del modelos en "new_param_space" (esto puede tomar unos 5 min)
predictions_gp <- predict.gp(gp, new_param_space)

gp_surface <- matrix(predictions_gp, length(unique(new_mau)))

persp(
  # tomamos los valores unicos de cada factor
  unique(new_mau),
  unique(new_alpha), 
  # creamos una matriz con los outputs
  gp_surface,
  xlab = "Preferencias",
  ylab = "Densidad",
  zlab = "Resultado",
  # parametros graficos
  theta = 20,
  phi = 20,
  shade = 0.6,
  col = "orange"
)

filled.contour(new_mau, new_alpha, gp_surface)

# Comparamos con Schelling
sg_surface <- readRDS("schelling_data.rds")
sg_lm_surface <- readRDS("lm_schelling.rds")
sg_gp_surface <- readRDS("gp_schelling.rds")
new_pref <- seq(0,1,length.out = 50)
new_dens <- seq(0.45, 0.95, length.out = 50)

# original
persp(
  # tomamos los valores unicos de cada factor
  unique(sg_surface$pref),
  unique(sg_surface$dens), 
  # creamos una matriz con los outputs
  matrix(sg_surface$out, nrow = length(unique(sg_surface$pref))),
  xlab = "Preferencias",
  ylab = "Densidad",
  zlab = "Resultado",
  # parametros graficos
  theta = 220,
  phi = 30,
  shade = 0.6,
  col = "orange"
)

# lm model
persp(
  # tomamos los valores unicos de cada factor
  unique(new_pref),
  unique(new_dens), 
  # creamos una matriz con los outputs
  sg_lm_surface,
  xlab = "Preferencias",
  ylab = "Densidad",
  zlab = "Resultado",
  # parametros graficos
  theta = 220,
  phi = 30,
  shade = 0.6,
  col = "orange"
)

# gp model
persp(
  # tomamos los valores unicos de cada factor
  unique(new_pref),
  unique(new_dens), 
  # creamos una matriz con los outputs
  sg_gp_surface,
  xlab = "Preferencias",
  ylab = "Densidad",
  zlab = "Resultado",
  # parametros graficos
  theta = 220,
  phi = 30,
  shade = 0.6,
  col = "orange"
)

filled.contour(unique(sg_surface$pref),
               unique(sg_surface$dens), 
               matrix(sg_surface$out, nrow = length(unique(sg_surface$pref))))

filled.contour(new_pref, new_dens, sg_lm_surface)
filled.contour(new_pref, new_dens, sg_gp_surface)

##################################
# ESTIMACIÓN de PARÁMETROS       #
##################################
# Nos comenzamos a aproximar al procedimiento para estimar los parámetros modelando 
# esta vez la relación de los inputs del modelo con la distancia entre los datos observados 
# y los datos simulados.

asfr_hut <- read.csv("asfr_hutterites.csv", header = T)
plot(asfr_hut)

obs <- sum(asfr_hut[,2])

# compute mse
compute_mse <- function(sim, obs){
  mse <-  sum((sim - obs)^2) / length(sim)
}

# calcular el error cuadrado medio en los puntos que tenemos en el objeto "d_fact"

dist <- sapply(d_fact$out,compute_mse,obs)

# graficar

d_fact$dist <- dist













