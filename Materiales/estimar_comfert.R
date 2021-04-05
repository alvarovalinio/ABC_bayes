################################################################################
#  Curso: Modelos Computacionales con Aplicaciones en Demografía               #
#  Daniel Ciganda                                                              #   
#  Trabajo Final                                                               #
################################################################################
library(data.table); library(lubridate); library(ggplot2); library(mlegp); library(parallel)


#####################################################################
############ 4t PARTE - Inferencia  - ABC - Bayesian optimization ###

# En la sesion anterior logramos aproximar con relativo exito la funcion 
# que mapea los inputs y outputs de comfert
# El objetivo ahora es estimar el valor de los paramteros que mejor se ajuste
# a nuestros datos utilizando el enfoque ABC - Computacion Bayesiana Aproximada

# Nos concentramos en los parametros que controlan la caida del riesgo de concebir 
# (fecundabilidad) con la edad: alpha, kappa.

alpha <- 35         # punto de inflexion
kappa  <- 0.25      # tasa
age <- seq(10, 50,1)

plot(age, 1 / (1 + exp(kappa*(age-alpha))), col = "red", lwd = 2)
lapply(seq(20, 45, 5), function(x) points(age, 1 / (1 + exp(kappa*(age-x)))))
plot(age, 1 / (1 + exp(kappa*(age-alpha))), col = "red", lwd = 2)
lapply(seq(0.1, 0.9, 0.1), function(x) points(age, 1 / (1 + exp(x*(age-alpha)))))


# Empecemos por definir el nivel observado
obs <- read.csv("datos/asfr_hutterites.csv", header = T)
plot(obs)

start <- Sys.time()
ini_c <- 2000
n0 <- 100 # size of initial sample of param combinations
ne <- 10 # nr of new evaluations at each iteration of the bayes opt. algorithm - Conviene setear al nr max de procesadores disponibles
N <- 520 # total nr of new evaluations 

# directory to store results
res_dir <- file.path("resultados",
                     paste0("ini_c_", ini_c),
                     paste0("ne_", ne),
                     paste0("N_", N))

priors <- data.frame(alpha = c(32, 37),
                     kappa = c(0.22, 0.37))

get_sample <- function(priors, n){
  
  lhs_sample <- as.data.frame(lhs::improvedLHS(n, ncol(priors)))
  
  locations <- priors[1, ]
  multipliers <- priors[2, ] - locations
  
  mapped_sample <- mapply(function(x, multiplier, location) (x*multiplier) + location,
                          lhs_sample, multipliers, locations)
  
  mapped_sample <- data.frame(mapped_sample)
  
  colnames(mapped_sample) <- colnames(priors)
  
  return(mapped_sample)
  
}


ini_sample <- get_sample(priors, n0)

# computar comfert_abc en paralelo
detectCores()

parallel_comfert <- function(params, ini_c){
  
  cl <- makeCluster(nrow(params), type = "PSOCK", outfile = "parallel_comfert.txt")
  
  # exportar parametros
  clusterExport(cl, "params", envir = environment())
  clusterExport(cl, "ini_c", envir = environment())
  
  clusterEvalQ(cl, library(lubridate))
  clusterEvalQ(cl, library(data.table))
  
  # exportar funcion
  invisible(
    clusterCall(cl, function() {
      source("comfert_abc.R") 
    }))
  
  sim <- parLapply(cl, 1:nrow(params), function(x) comfert_abc(params[x,1],
                                                               params[x,2],
                                                               ini_c))
  stopCluster(cl)
  
  return(sim)
}

params <- ini_sample
ini_sim <- parallel_comfert(params, ini_c)

# compute mse
compute_mse <- function(x, obs){
  mse <-  sum((x$fx - obs$fx)^2) / length(x$fx)
}

params$mse <- sapply(ini_sim, compute_mse, obs)  

# estimate gp, eval new points and repeat  
n <- n0
while (n < (n0+N)) {
  
  # train gp
  gpo <- mlegp(params[,!names(params) == "mse"], 
               params[, "mse"], verbose = 0)

  
  # compute new points
  new_set <- get_sample(priors, n = n0/2)
  
  # generate predictions
  pred <- predict.gp(gpo, newData = new_set, se.fit = T)
  
  # aquisition function
  mu <- pred$fit
  sigma <- pred$se.fit
  
  kappa_af <- 2
  
  lcb <- mu - kappa_af * sigma
  
  # define new points to evaluate
  new_eval <- new_set[which(lcb %in% sort(lcb)[1:ne]),]
  
  # compute the model at new points
  ne_sim <- parallel_comfert(params = new_eval, ini_c)
  
  # save results
  lapply((n+1):(n+ne), function(x) dir.create(path = paste0(res_dir, "/param_set_", x), recursive = T))
  
  lapply(1:ne, function(x) saveRDS(ne_sim[[x]],
                                   file = paste0(paste0(res_dir,
                                                        "/param_set_", n+x)
                                                 ,"/fx.RData")))
  
  # compute mse in new eval
  new_eval$mse <- sapply(ne_sim, compute_mse, obs)  
  
  # add to previous points
  params <- rbind(params, new_eval)
  
  n <- n+ne
  print(n)
  
}

if(!dir.exists(file.path(res_dir, "evals"))){dir.create(file.path(res_dir, "evals"))}

saveRDS(params[(n0+1):nrow(params),], paste0(paste0(res_dir,"/evals/evalued_points.rds")))

# Aproximate posterior distribution
post_candidates <- readRDS(file.path("Materiales",res_dir, "evals", "evalued_points.rds")) 

# Optimal combination of parameters
min_mse <- post_candidates[order(post_candidates$mse),]$mse[1] 
paramset <- which(post_candidates$mse == min_mse)[1] # results dir for min mse
opt_res_dir <- file.path(res_dir, paste0("param_set_", n0+paramset))

sim <- readRDS(file.path("Materiales",opt_res_dir, "fx.RData"))

# plot obs vs. sim de combinacion con menor distancia
plot(obs)
points(sim, col = "red")

# definir distancia - valor que comprende el 10% de las combinaciones con menor distancia
epsilon <- quantile(post_candidates$mse,probs = seq(0, 1, 0.10))[2]

accepted <- post_candidates[post_candidates$mse < epsilon,] 

# posterior aproximadas
hist(accepted$alpha)
hist(accepted$kappa)


end <- Sys.time()
print(end-start)



