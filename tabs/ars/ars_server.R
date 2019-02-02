##-- Help page ----
observeEvent(input$ars_help,{
  updateTabsetPanel(session = session, inputId = "apps", selected = "ars_help")
})

##-- Help page ----
observeEvent(input$ars_go_app,{
  updateTabsetPanel(session = session, inputId = "apps", selected = "ars")
})

##-- Default limits to distributions ----
ars_limits <- reactive({
  prob <- 10^-4
  if(input$ars_distribution == 1){
    shape1 <- input$ars_shape1
    shape2 <- input$ars_shape2
    
    limInf <- qbeta(prob, shape1 = shape1, shape2 = shape2)
    limSup <- qbeta(1-prob, shape1 = shape1, shape2 = shape2)
  } else {
    if(input$ars_distribution == 2){
      df <- input$ars_gl_qui
      
      limInf <- qchisq(prob, df = df)
      limSup <- qchisq(1-prob, df = df)
    } else {
      if(input$ars_distribution == 3){
        rate <- input$ars_rate_exp
        
        limInf <- qexp(prob, rate = rate)
        limSup <- qexp(1-prob, rate = rate)
      } else {
        if(input$ars_distribution == 4){
          shape <- input$ars_shape_gama
          rate <- input$ars_rate_gama
          
          limInf <- qgamma(prob, shape = shape, rate = rate)
          limSup <- qgamma(1-prob, shape = shape, rate = rate)
        } else {
          if(input$ars_distribution == 5){
            location <- input$ars_location_logis
            scale <- input$ars_scale_logis
            
            limInf <- qlogis(prob, location = location, scale = scale)
            limSup <- qlogis(1-prob, location = location, scale = scale)
          } else {
            if(input$ars_distribution == 6){
              mean <- input$ars_mean_normal
              sd <- sqrt(input$ars_var_normal)
              
              limInf <- qnorm(p = prob, mean = mean, sd = sd)
              limSup <- qnorm(1-prob, mean = mean, sd = sd)
            } else {
              if(input$ars_distribution == 7){
                shape <- input$ars_shape_w
                scale <- input$ars_scale_w
                
                limInf <- qweibull(prob, shape = shape, scale = scale)
                limSup <- qweibull(1-prob, shape = shape, scale = scale)
              }
            }
          }
        }
      }
    }
  }
  return(list(limInf = limInf, limSup = limSup))
})

##-- Updating limits ----
observe({
  limits <- ars_limits()
  limInf <- limits$limInf
  limInf <- round(limInf, 2)
  limSup <- limits$limSup
  limSup <- round(limSup, 2)
  
  updateNumericInput(session, "ars_lim_inf", value = limInf)
  updateNumericInput(session, "ars_lim_sup", value = limSup)
})

##-- Functions and outputs ----
ars_functions <- eventReactive(input$ars_run_button,{
  
  limInf <- input$ars_lim_inf + 1e-10
  limSup <- input$ars_lim_sup - 1e-10
  
  if(input$ars_distribution == 1){
    shape1 <- input$ars_shape1
    shape2 <- input$ars_shape2
    
    fx <- function(x){dbeta(x = x, shape1 = shape1, shape2 = shape2)}
    log_fx <- function(x){dbeta(x = x, shape1 = shape1, shape2 = shape2, log = T)}
    
    const <- pbeta(q = limSup, shape1 = shape1, shape2 = shape2) - pbeta(q = limInf, shape1 = shape1, shape2 = shape2)
  } else {
    if(input$ars_distribution == 2){
      df <- input$ars_gl_qui
      
      fx <- function(x){dchisq(x = x, df = df)}
      log_fx <- function(x){dchisq(x = x, df = df, log = T)}
      
      const <- pchisq(q = limSup, df = df) - pchisq(q = limInf, df = df)
    } else {
      if(input$ars_distribution == 3){
        rate <- input$ars_rate_exp
        
        fx <- function(x){dexp(x = x, rate = rate)}
        log_fx <- function(x){dexp(x = x, rate = rate, log = T)}
        
        const <- pexp(q = limSup, rate = rate) - pexp(q = limInf, rate = rate)
      } else {
        if(input$ars_distribution == 4){
          shape <- input$ars_shape_gama
          rate <- input$ars_rate_gama
          
          fx <- function(x){dgamma(x = x, shape = shape, rate = rate)}
          log_fx <- function(x){dgamma(x = x, shape = shape, rate = rate, log = T)}
          
          const <- pgamma(q = limSup, shape = shape, rate = rate) - pgamma(q = limInf, shape = shape, rate = rate)
        } else {
          if(input$ars_distribution == 5){
            location <- input$ars_location_logis
            scale <- input$ars_scale_logis
            
            fx <- function(x){dlogis(x = x, location = location, scale = scale)}
            log_fx <- function(x){dlogis(x = x, location = location, scale = scale, log = T)}
            
            const <- plogis(q = limSup, location = location, scale = scale) - plogis(q = limInf, location = location, scale = scale)
          } else {
            if(input$ars_distribution == 6){
              mean <- input$ars_mean_normal
              sd <- sqrt(input$ars_var_normal)
              
              fx <- function(x){dnorm(x = x, mean = mean, sd = sd)}
              log_fx <- function(x){dnorm(x = x, mean = mean, sd = sd, log = T)}
              
              const <- pnorm(q = limSup, mean = mean, sd = sd) - pnorm(q = limInf, mean = mean, sd = sd)
            } else {
              if(input$ars_distribution == 7){
                shape <- input$ars_shape_w
                scale <- input$ars_scale_w
                
                fx <- function(x){dweibull(x = x, shape = shape, scale = scale)}
                log_fx <- function(x){dweibull(x = x, shape = shape, scale = scale, log = T)}
                
                const <- pweibull(q = limSup, shape = shape, scale = scale) - pweibull(q = limInf, shape = shape, scale = scale)
              }
            }
          }
        }
      }
    }
  }
  
  if(const == 0) const <- 1
  
  #------------
  log_fxD <-  D(log_fx(x) ~ x)
  
  Points <- seq(from = limInf, to = limSup, length.out = input$ars_n_points + 2)
  Points <- Points[-c(1,length(Points))]
  
  n = input$ars_n
  
  aux <- data.frame(xj = Points[-length(Points)], xj1 = Points[-1])
  zs <- c(limInf, apply(aux, MARGIN = 1,
                        FUN = function(x) UxIntercept(log_fx = log_fx, log_fxD = log_fxD,xj =  x[1], xj1 = x[2])), limSup)
  
  W_Interval <- WeightsInterval(log_fx = log_fx, log_fxD = log_fxD, Points = Points, Intercepts = zs)
  
  Cont <- 1
  obs <- c(0, 0, 0)
  n.samp <- 0
  zsList <- list()
  zsList[[1]] <- zs
  PointsList <- list()
  PointsList[[1]] <- Points
  xTab <- list()
  xTab[[1]] <- c(NA, NA)
  rateAcc <- 0
  #------------
  
  ticks <- ceiling(seq(1, n, length.out = 10))
  
  while(n.samp <= n){
    
    x <- ARS(fx = fx, log_fx = log_fx, log_fxD = log_fxD, limInf = limInf, limSup = limSup,
             Points = Points, Intercepts = zs, W_Interval = W_Interval)
    
    xtab <- x[1:2]
    
    if(x[2]%in% c(1,2)){
      n.samp <- n.samp + 1
      
      obs <- rbind(obs, c(xtab, n.samp))
    } else{
      obs <- rbind(obs, c(xtab, n.samp))
    }
    
    if(x[2] %in% c(0,2)){
      Element <- x[3]
      Interval <- x[4]
      x <- x[1]
      
      Points <- sort(c(Points, x))
      nInterval <- length(W_Interval)
      nZs <- length(zs)
      nPoints <- length(Points)
      
      if(Element == 0){
        p1 <- zs[1]
        p2 <- Points[1]
        p3 <- Points[2]
        
        z1 <- UxIntercept(log_fx = log_fx, log_fxD = log_fxD, xj = p2, xj1 = p3)
        zs <- c(p1, z1, zs[-1])
        
        weightNew <- WeightsInterval(log_fx = log_fx, log_fxD = log_fxD, Points = Points[1:2], Intercepts = zs[1:3])
        W_Interval <- c(weightNew, W_Interval[-1])
      } else{
        if(Element == 1){
          p1 <- Points[1]
          p2 <- Points[2]
          p3 <- Points[3]
          
          z1 <- UxIntercept(log_fx = log_fx, log_fxD = log_fxD, xj = p1, xj1 = p2)
          z2 <- UxIntercept(log_fx = log_fx, log_fxD = log_fxD, xj = p2, xj1 = p3)
          zs <- c(zs[1], z1, z2, zs[-c(1, 2)])
          
          weightNew <- WeightsInterval(log_fx = log_fx, log_fxD = log_fxD, Points = Points[1:3], Intercepts = zs[1:4])
          W_Interval <- c(weightNew, W_Interval[-c(1,2)])
        } else{
          if(Element == nPoints-1){
            p1 <- Points[Element]
            p2 <- Points[Element + 1]
            p3 <- zs[nZs]
            
            z2 <- UxIntercept(log_fx = log_fx, log_fxD = log_fxD, xj = p1, xj1 = p2)
            zs <- c(zs[-nZs], z2, p3)
            
            weightNew <- WeightsInterval(log_fx = log_fx, log_fxD = log_fxD, Points = Points[Element:(Element+1)], Intercepts = zs[Element:(Element+2)])
            W_Interval <- c(W_Interval[-nInterval], weightNew)
          } else{
            if(Element == nPoints-2){
              p1 <- Points[Element]
              p2 <- Points[Element + 1]
              p3 <- Points[Element + 2]
              
              z1 <- UxIntercept(log_fx = log_fx, log_fxD = log_fxD, xj = p1, xj1 = p2)
              z2 <- UxIntercept(log_fx = log_fx, log_fxD = log_fxD, xj = p2, xj1 = p3)
              zs <- c(zs[-c(nZs, (nZs - 1))], z1, z2, zs[nZs])
              
              weightNew <- WeightsInterval(log_fx = log_fx, log_fxD = log_fxD, Points = Points[Element:(Element+2)], Intercepts = zs[Element:(Element+3)])
              W_Interval <- c(W_Interval[-c(nInterval-1, nInterval)], weightNew)
            } else{
              p1 <- Points[Element]
              p2 <- Points[Element + 1]
              p3 <- Points[Element + 2]
              
              z1 <- UxIntercept(log_fx = log_fx, log_fxD = log_fxD, xj = p1, xj1 = p2)
              z2 <- UxIntercept(log_fx = log_fx, log_fxD = log_fxD, xj = p2, xj1 = p3)
              zs <- c(zs[1:(Element)], z1, z2, zs[(Element+2):nZs])
              
              weightNew <- WeightsInterval(log_fx = log_fx, log_fxD = log_fxD, Points = Points[Element:(Element+2)], Intercepts = zs[Element:(Element+3)])
              W_Interval <- c(W_Interval[1:(Element-1)], weightNew, W_Interval[(Element+2):nInterval])
            }
          }
        }
      }
    }
    
    Cont <- Cont + 1
    zsList[[Cont]] <- zs
    PointsList[[Cont]] = Points
    xTab[[Cont]] <- xtab
    rateAcc <- c(rateAcc, n.samp/(Cont-1))
  }
  
  return(list(zsList = zsList, PointsList = PointsList, fx = fx, log_fx = log_fx, log_fxD = log_fxD,
              limInf = limInf, limSup = limSup, const = const, obs = obs, Cont = Cont,
              x = xTab, rateAcc = rateAcc, n = n))
})

output$ars_animation_graph_btn <- renderUI({
  sliderInput(inputId = "ars_animation_graph", label = NULL,
              min = 1, max = ars_functions()$Cont, value = input$ars_iteration_graph,
              step = 1, animate = animationOptions(interval = 1500, loop = FALSE))
})

output$ars_iteration_graph_btn <- renderUI({
  numericInput(inputId = "ars_iteration_graph", label = NULL,
               min = 1, max = ars_functions()$Cont, value = 1)
})

output$ars_animation_table_btn <- renderUI({
  sliderInput(inputId = "ars_animation_table", label = h5(strong("Or just select with the slider selection")),
              min = 1, max = ars_functions()$Cont, value = input$ars_iteration_table,
              step = 1, animate = animationOptions(interval = 1500, loop = FALSE))
})

output$ars_iteration_table_btn <- renderUI({
  numericInput(inputId = "ars_iteration_table", label = h5(strong("Input an iteration:")),
               min = 1, max = ars_functions()$Cont, value = 1)
})

ars_plots_tables <- reactive({
  
  ParGraph <- ars_functions()
  
  i_graphs <- input$ars_animation_graph
  i_table <- input$ars_animation_table
  n <- ParGraph$n
  
  if(is.null(i_graphs)) i_graphs <- 1
  if(is.null(i_table)) i_table <- 1
  
  zs <- ParGraph$zsList[[i_graphs]]
  Points <- ParGraph$PointsList[[i_graphs]]
  sample <- ParGraph$obs[1:i_graphs, c(1, 3)]
  
  if(i_graphs == 1){
    nSamp <- 0
    pos <- 1
    sample <- c(NA, NA)
    notSample <- NULL
  } else{
    duplic <- duplicated(sample[, 2])
    notSample <- sample[duplic, 1]
    sample <- sample[!duplic, 1]
  }
  
  fx <- ParGraph$fx
  log_fx <- ParGraph$log_fx
  log_fxD <- ParGraph$log_fxD
  limInf <- ParGraph$limInf
  limSup <- ParGraph$limSup
  rateAcc_graphs <- ParGraph$rateAcc[1:i_graphs]
  const <- ParGraph$const
  
  log <- LogFunGraph(log_fx = log_fx, log_fxD = log_fxD,
                     limInf = limInf, limSup = limSup,
                     Points = Points, Intercepts = zs)
  fun <- FunGraph(fx = fx, log_fx = log_fx, log_fxD = log_fxD,
                  limInf = limInf, limSup = limSup,
                  Points = Points, Intercepts = zs)
  hist <- HistARS(fx = fx, sample = sample,
                  limInf = limInf, limSup = limSup,
                  const = const)
  
  Cont <- ParGraph$Cont
  rateAcc_table <- ParGraph$rateAcc[1:i_table]
  x_table <- do.call(what = 'rbind', args = ParGraph$x)[, 1]
  obs_table <- ParGraph$obs[1:i_table, 2]
  sk <- cumsum(x = obs_table == 1)
  rj <- cumsum(x = obs_table == 2)
  
  if(i_table != 1){
    x_table <- round(x_table, 4)
  }
  
  table <- data.frame(x = x_table[1:i_table],
                      sk = sk,
                      rj = rj)
  
  table$size <- table$sk + table$rj
  table$rejected <-  1:i_table - table$size - 1
  table$total = table$size + table$rejected
  table$rateAcc = round(rateAcc_table, 4)
  
  if(i_table == 1) table$rateAcc <- NA
  
  names(table) <- c("x sampled",
                    "Squeezing test",
                    "Rejection test",
                    "Accepted",
                    "Rejected",
                    "Simulated",
                    "Acceptance rate")
  row.names(table) <- NULL
  
  ts <- TSGraph(AcceptRate = rateAcc_graphs, nIterations = Cont)
  
  return(list(log = log, fun = fun, hist = hist, ts = ts, table = table))
})

output$ars_table <- renderDataTable({
  ars_plots_tables()$table
},
options = list(searching = TRUE, paging = TRUE, searchable = TRUE, filtering = TRUE))

output$ars_log <- renderPlot({
  print(ars_plots_tables()$log)
})

output$ars_fun <- renderPlot({
  print(ars_plots_tables()$fun)
})

output$ars_hist <- renderPlot({
  print(ars_plots_tables()$hist)
})

output$ars_ts <- renderPlot({
  print(ars_plots_tables()$ts)
})