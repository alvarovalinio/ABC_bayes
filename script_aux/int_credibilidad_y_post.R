# Prueba intervalos de credibilidad para y_posterior

library(data.table); library(lubridate); library(tidyverse); library(data.table)

# Obtenemos los candidatos de la posterior de alpha y kappa

# Parametros fijados

ini_c <- 2000
n0 <- 100 # size of initial sample of param combinations
ne <- 10 # nr of new evaluations at each iteration of the bayes opt. algorithm - Conviene setear al nr max de procesadores disponibles
N <- 520 # total nr of new evaluations 

# directory to store results
res_dir <- file.path("resultados",
                     paste0("ini_c_", ini_c),
                     paste0("ne_", ne),
                     paste0("N_", N))

post_candidates <- readRDS(file.path("Materiales",
                                res_dir, "evals", "evalued_points.rds")) %>% 
                                    data.table()

# Defenimos un epsilon como cota superior del mse

epsilon <- quantile(post_candidates$mse,probs = seq(0, 1, 0.10))[2]

# Filtramos los post_candidate que caen

post_e <- post_candidates[mse<=epsilon] # Esta seria la version más "rustica"
# Indicatriz


# Obtenemos los intervalos de credibilidad al 95%

intervalos <- apply(post_e[,-3],2,function(y){
  
  int_inf <- quantile(y,0.025)
  
  int_sup <- quantile(y,0.975)
  
  return(c(int_inf,int_sup))
}) 


# Cargamos funcion para simular el proceso

source("Materiales/comfert_abc.R") # ¿Podriamos usar un meta modelo?


Sys.time()

sim_inf <- comfert_abc(alpha=intervalos[1,1],
                       kappa = intervalos[1,2],ini_c = 2000) %>% 
                          rename(fx_inf=fx) %>% data.table()

sim_sup <- comfert_abc(alpha=intervalos[2,1],
                       kappa = intervalos[2,2],ini_c = 2000) %>% 
                          rename(fx_sup=fx) %>% data.table()

Sys.time()
#20 min con 2000

setkey(sim_inf,age)
setkey(sim_sup,age)

sim_interval <- sim_inf[sim_sup,nomatch=0]


# Obtenemos los valores simulados con el "tita optimo"
min_mse <- post_candidates[order(post_candidates$mse),]$mse[1] 
paramset <- which(post_candidates$mse == min_mse)[1] # results dir for min mse

opt_res_dir <- file.path(res_dir, paste0("param_set_", n0+paramset))

sim <- readRDS(file.path("Materiales",opt_res_dir, "fx.RData")) %>% data.table()

setkey(sim,age)

sim <- sim[sim_interval,nomatch=0]

head(sim)


sim %>% ggplot(aes(x=age)) + geom_line(aes(y=fx),col="red") + 
                                geom_line(aes(y=fx_inf),col="blue")+
                                      geom_line(aes(y=fx_sup),col="green")
# Agregar el obs

# Comentarios :

# 1) Sin duda probar usar un meta modelo

# 2) ¿Que modelo usar para ajustar la TFR en funcion de los parametros?
# (SVM, BAGGING, RF, si hay tiempo capaz que DNN?)
# Solo tenemos dos "variables" de entrada, podriamos hacer algo más "simple"?
# ¿Usamos el proceso gaussiano? Es el que esta dentro de la funcion "estimar_comfert"

# 3) Mencionar que estamos usando la version "más rustica" de ABC (aceptar- rechazar valores), capaz que poner
# para futuros trabajos usar la version kernel o otra variante


# 4) Mas incetidumbre : 1) Varias iteraciones con el mismo tita, 2) Iterar el valor de epsilon
# Obs : El optimo, por mas que "agragande" epsilon, es el mismo?

# Idea : Hacer funcion que tome como entrada i) numero de iteraciones de cada ajuste del modelo
# ii) Rango de epsilon (o valor puntual) y devuelva las estimaciones con intervalos empiricos

# Evitar que se prenda fuego la compu

# Hacer intervalo empirico para el tita optimo 

# Para futuro trabajos hacer bootstrap