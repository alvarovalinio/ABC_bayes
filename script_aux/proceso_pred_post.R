# Script para obtener la distrubcion de la predictiva posterior en cada edad
# utilizando todos los titas que da el meta - modelo (falta filtrar por epsilon)

# Intervalo "más" amplio posible

library(data.table)
library(tidyverse)
library(here)

# Vector Edad base

edad <- 10:55 %>% data.table()
colnames(edad) <- "age"

# Loop para cargar datos de carpetas

for (i in 1:520){
  
  ruta <- paste0("Materiales/resultados/ini_c_2000/ne_10/N_520/param_set_",i+100,"/fx.Rdata") 
  
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

datos$fx <- sim$fx

datos %>% ggplot(aes(x=age))+geom_line(aes(y=fx),color="red")+
  geom_line(aes(y=int_inf),color="blue")+geom_line(aes(y=int_sup),color="green")+geom_line(aes(y=mediana),color="grey")
