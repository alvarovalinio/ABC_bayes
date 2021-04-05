comfert_abc <- function(alpha,
                        kappa,
                        ini_c,
                        ini_year = 1925,
                        end_year = 1976,
                        parse = T){
  
  if(parse){
    cat("Running..")
    cat(paste("\n Age Decline Fec.:", alpha,
              "\n Rate Decline Fec.:", kappa))
  }
  
  g_shape <- 5      # gamma shape - heterogeneity fecundability  
  g_rate <- 38      # gamma rate - heterogeneity fecundability
  mau <- 18.5       # mean age at union
  sdau <- 0.1       # sd age at union
  nsp <- 6          # non susc. period
  
  ###########################################################
  # TIME                                                    #  
  ###########################################################
  ini <- as.POSIXct(paste0(ini_year,"-01-01 00:00:00"), tz = "UTC")
  end <- as.POSIXct(paste0(end_year,"-01-01 00:00:00"), tz = "UTC")
  time <- ini
  year <- year(time)
  
  ###########################################################
  # AUX FUN / VARS                                          #  
  ###########################################################
  pregnancy <- 270*86400L # 270 Days in seconds
  secs_m <- 2592000L
  secs_y <- 31536000L
  tw_m <- 12*2592000L # twelve months in seconds
  srb <- 0.515 # Sex ratio at birth
  
  ###########################################################
  # INITIAL BIRTHS                                          #  
  ###########################################################
  wtb <- runif(ini_c)
  wt_ini_births <- wtb/max(wtb) * secs_y 
  
  ###########################################################
  # POP                                                     #  
  ###########################################################
  c_names <- c("wt_union",
               "wt_birth",
               "wt_update",
               "age",
               "tob",
               "phi",
               "union",
               "age_union",
               "kids",
               paste0("age_", 1:25))
  pop <- data.table(matrix(nrow = 0, ncol = length(c_names)))
  pop <- setNames(pop, c_names)
  pop[, names(pop) := lapply(.SD, as.numeric)]
  
  ###################
  # Aux Vars        # 
  ###################
  events <- c("union", "birth", "update")
  wt_col <- paste0("wt_", events)
  event_count <- setNames(as.list(rep(0, length(events))), events)
  
  #****************************************************************************************************************************************
  #-------------------------------------------------------- RUNNING THE SIMULATION -------------------------------------------------------#
  #****************************************************************************************************************************************
  while (time < end){
    
    min_idx <- pop[, arrayInd(which.min(as.matrix(.SD)), .dim = dim(.SD)), .SDcols = wt_col]
    time_next_event <- pop[[min_idx[1], min_idx[2]]]
    next_event <- events[min_idx[2]]
    rid <- min_idx[1]
    
    # next event*
    if(length(wt_ini_births) > 0){  
      if(is.null(time_next_event) || time_next_event > min(wt_ini_births)){
        next_event <- "birth"
        time_next_event <- min(wt_ini_births)
        rid <- NA
        wt_ini_births <- wt_ini_births[-which.min(wt_ini_births)]
      }
      wt_ini_births <- wt_ini_births - time_next_event
    }
    
    #****************************************************************************************************************************************
    #--------- UPDATING ---------------------------------------------------------------------------------------------------------------------
    #****************************************************************************************************************************************
    # Clock.
    time <- time + time_next_event
    
    # Update Age of all agents.
    pop[, age := age + time_next_event]
    # Update WTs of all agents
    pop[, c(wt_col):= lapply(.SD, function(x){x-time_next_event}), .SDcols = wt_col]
    
    ########################
    ##       UNION        ##
    ########################
    if (next_event == "union") {
      
      pop[rid, `:=`(union = 1L, 
                    age_union = age,
                    wt_union = Inf)]
      
      wt_conception <- rexp(1, rate = pop[rid, phi] / (1 + exp(kappa*(pop[rid, age/secs_y] - alpha)))) * secs_m
      
      if(wt_conception < tw_m){ # conception within the year
        
        pop[rid, wt_birth := wt_conception + pregnancy]  
        
      }else{ # failed to conceive
        
        pop[rid, wt_update := tw_m]
        
      }
      
    } 
    
    ########################
    ##       BIRTH        ##
    ########################
    if (next_event == "birth") {
      if(is.na(rid)){
        girl <- Inf
      }else{
        girl <- runif(1)
      }
      if (girl > srb){
        pop <- rbindlist(list(pop, pop[.N+1]), fill = T)
        pop[.N, `:=` (age = 0L,
                      age_union = NA,
                      tob = as.integer(time),
                      union = 0L,
                      kids = 0L,
                      wt_birth = Inf,
                      wt_update = Inf
        )]
        
        pop[.N, phi := rgamma(1, g_shape, g_rate)]
        wt_union_years <- rlnorm(1, meanlog = log(mau),
                                 sdlog = sdau) 
        pop[.N, wt_union := wt_union_years*secs_y] # seconds from years
        
      }
      
      # Change Indicators for mother
      if(!is.na(rid)){
        
        pop[rid, kids := kids + 1L]
        pop[rid, (paste0("age_", pop[rid, kids])) := age]
        pop[rid,`:=` (wt_birth = Inf,
                      wt_update = nsp*secs_m)]
        
      }
    }
    
    ########################
    ##       UPDATE       ##
    ########################
    if (next_event == "update") {
      
      wt_conception <- rexp(1, rate = pop[rid, phi] / (1 + exp(kappa*(pop[rid, age/secs_y] - alpha)))) * secs_m
      
      if(wt_conception < tw_m){ # conception within the year
        
        pop[rid, wt_birth := wt_conception + pregnancy] 
        pop[rid, wt_update := Inf]
        
      }else{ # failed to conceive
        
        pop[rid, wt_update := tw_m]
        
      }
      
    }
    
    #****************************************************************************************************************************************#
    #--------- NEW YEAR --------------------------------------------------------------------------------------------------------------------#
    #****************************************************************************************************************************************#
    
    if (year != year(time)){
      
      year <- year + 1
    }
  }
  
  ages <- grep('age', names(pop), value = T)
  pop[, (ages) := lapply(.SD, function(x) x/secs_y), .SDcols = ages]
  
  hst <- pop[age > 50, 7:ncol(pop)]
  hst[, id := 1:nrow(hst)]
  
  hst <- reshape(hst, 
                 direction = "long",
                 varying = paste0("age_", 1:25),
                 v.names = "age",
                 idvar = "id",
                 timevar = "parity",
                 times = 1:25)
  
  hst <- hst[!is.na(age),]
  hst <- hst[order(hst$id)]
  age <- 10:55
  hst <- merge(hst, as.data.frame(age), by = "age", all= T)
  # fecundidad acumulada a age E(x) 
  hst$cum_births <- cumsum(!is.na(hst$kids))
  hst$cum_fert <- hst$cum_births/max(hst$id, na.rm = T)
  lower <- hst[hst$age %in% seq(10,54,1),c("age", "cum_fert")]
  upper <- hst[hst$age %in% seq(11,55,1),c("age", "cum_fert")]
  fx <- upper$cum_fert - lower$cum_fert 
  fx <- as.data.frame(cbind(age = 10:55, fx))
  
  if(parse){
    cat("\n End \n")
  }
  
  
  return(fx)
  
} 