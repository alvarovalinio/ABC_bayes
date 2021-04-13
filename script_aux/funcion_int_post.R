# Funcion para obtener los intervalos de confianza segun el valor de epsilon

library(data.table)
library(tidyverse)
library(here)

int_post <- function(e=2){
  
  # Vector Edad base
  
  edad <- 10:55 %>% data.table()
  colnames(edad) <- "age"
  
  # Cargamos resultados con los candidatos de la posterior
  
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
  
  # Agregamos id para mapear
  
  post_candidates$id <- 1:nrow(post_candidates)
  
  
  # definir distancia - valor que comprende el 10% de las combinaciones con menor distancia
  epsilon <- quantile(post_candidates$mse,probs = seq(0, 1, 0.05))[e]
  
  accepted <- post_candidates[post_candidates$mse < epsilon,] 
  
  
  # Loop para cargar datos de carpetas
  
  for (i in 1:nrow(accepted)){
    
    ruta <- paste0("Materiales/resultados/ini_c_2000/ne_10/N_520/param_set_",accepted$id[i]+100,"/fx.Rdata") 
    
    y_pred <- read_rds(ruta) %>% data.table()
    
    colnames(y_pred) <- c("age",paste0("fx_",i+100))
    
    setkey(y_pred,age)
    setkey(edad,age)
    
    edad <- edad[y_pred,nomatch=0]
    
  }
  
  quantiles <- apply(edad[,-1],1,function(y){
    
    int_inf <- quantile(y,0.025)
    
    int_sup <- quantile(y,0.975)
    
    mediana <- quantile(y,0.5)
    
    
    return(c(int_inf,int_sup,mediana))
  }) 
  
  
  datos <- data.table(age=edad$age,int_inf=quantiles[1,],int_sup=quantiles[2,],mediana=quantiles[3,])
  
  
  # Agregamos los valores del simulado
  obs <- readr::read_csv(here::here("Materiales/datos","asfr_hutterites.csv"))
  
  datos$fx <- obs$fx
  
  
  
  return(datos)
  
}