##-- Help page ----
observeEvent(input$norm_help,{
  updateTabsetPanel(session = session, inputId = "apps", selected = "norm_help")
})

##-- Help page ----
observeEvent(input$norm_go_app,{
  updateTabsetPanel(session = session, inputId = "apps", selected = "norm")
})

##-- Original data ----
##-- + Getting the inputs ----
norm_inputs <- eventReactive(input$norm_run,{
  ##-- + Inputs ----
  norm_distribution <- input$norm_distribution
  norm_n_obs <- input$norm_n_obs
  norm_function <- input$norm_function
  
  if(norm_n_obs > 5000){
    sendSweetAlert(
      session = session,
      title = "N° of observations must be smaller than 5000",
      text = a("https://stackoverflow.com/questions/15427692/perform-a-shapiro-wilk-normality-test/15427746#15427746"),
      type = "error"
    )
  }
  
  return(list(norm_distribution = norm_distribution,
              norm_n_obs = norm_n_obs,
              norm_function = norm_function))
  
})

##-- + Creating original data ----
norm_data <- eventReactive(input$norm_run, {
  ##-- + Inputs ----
  inputs <- norm_inputs()
  norm_distribution <- inputs$norm_distribution
  norm_n_obs <- inputs$norm_n_obs
  
  ##-- + Sampling data ----
  if(norm_distribution == "Log-normal"){
    x <- rlnorm(n = norm_n_obs, meanlog = 0, sdlog = 1)
  } else{
    if(norm_distribution == "Normal"){
      x <- rnorm(n = norm_n_obs, mean = 15)
    } else{
      if(norm_distribution == "Chi-square"){
        x <- rchisq(n = norm_n_obs, df = 15)
      } else{
        if(norm_distribution == "t-student"){
          x <- rt(n = norm_n_obs, df = 5) + 15
        }
      }
    }
  }
  
  data <- data.frame(x = x)
  
  if(norm_n_obs > 5000){
    return(NULL)
  } else{
    return(data)  
  }
  
})

##-- + Plots and tables ----
output$norm_hist <- renderPlot({
  data <- norm_data()
  bins <- max(5, ceiling(length(data$x)/30))
  
  if(!is.null(data)){
    mean_x <- mean(data$x)
    sd_x <- sd(data$x)
    
    ggplot(data) +
      geom_histogram(mapping = aes(x = x, y = ..density..), 
                     color = "white", fill = "SteelBlue", bins = bins) +
      stat_function(fun = function(x) dnorm(x = x, mean = mean_x, sd = sd_x),
                    color = "IndianRed", size = 1.2) +
      xlab("Sampled data") + ylab("Density") +
      gg_tema
  }
})
output$norm_qqplot <- renderPlot({
  data <- norm_data()
  
  if(!is.null(data)){
    data <- data %>%
      mutate(y = qqnorm(x, plot = FALSE)$x)
    
    ggplot(data = data, mapping = aes(x = x, y = y)) + 
      geom_point(color = "SteelBlue", size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "IndianRed", size = 2) +
      xlab("Theoretical quantiles") + ylab("Empirical quantiles") +
      gg_tema
  }
})
output$norm_tab <- DT::renderDataTable({
  data <- norm_data()
  
  mean_x <- mean(data$x)
  sd_x <- sd(data$x)
  
  if(!is.null(data)){
    anderson <- ad.test(x = data$x)$p.value
    lillie <- lillie.test(x = data$x)$p.value
    ks <- ks.test(x = scale(data$x), y = "pnorm")$p.value
    shapiro <- shapiro.test(x = data$x)$p.value
    
    tab <- data.frame(`Test` = c("Anderson-darling", "Lilliefors", "Kolmogorov-smirnov", "Shapiro-wilk"),
                      `pvalor` = round(c(anderson, lillie, ks, shapiro), 2))
    
    tab <- rbind.data.frame(c("x", round(c(anderson, lillie, ks, shapiro), 2)))
    names(tab) <- c("fun", "anderson", "lilliefors", "ks", "sw")
    
    datatable(tab, escape = FALSE, selection = 'none',
              rownames = F, colnames = c("", "Anderson-darling", "Lilliefors", "Kolmogorov-smirnov", "Shapiro-wilk"), options = options_DT) %>%
      formatStyle(columns = c("anderson", "lilliefors", "ks", "sw"), 
                  backgroundColor = styleInterval(cuts = c(0.05), values = c("IndianRed", "SteelBlue"))) %>%
      formatStyle('fun',  color = 'black', backgroundColor = 'white', fontWeight = 'bold')
  }
})

##-- Transformed data ----
norm_data_trans <- eventReactive(input$norm_run, {
  data <- norm_data()
  trans <- norm_inputs()$norm_function
  
  if(!is.null(data)){
    if(trans == "Exponential"){
      data$x <- exp(data$x)
    } else{
      if(trans == "Identity"){
        data$x <- data$x
      } else{
        if(trans == "Logarithm"){
          data$x <- log(data$x)
        } else{
          if(trans == "Square"){
            data$x <- data$x^2
          } else{
            if(trans == "Square root"){
              data$x <- sqrt(data$x)
            } else{
              if(trans == "Reciprocal"){
                data$x <- 1/data$x
              }
            }
          }
        }
      }
    }
  }
  
  return(data)
})

##-- + Plots and tables ----
output$norm_hist_trans <- renderPlot({
  data <- norm_data_trans()
  bins <- max(5, ceiling(length(data$x)/30))
  
  if(!is.null(data)){
    mean_x <- mean(data$x)
    sd_x <- sd(data$x)
    
    ggplot(data) +
      geom_histogram(mapping = aes(x = x, y = ..density..), 
                     color = "white", fill = "SteelBlue", bins = bins) +
      stat_function(fun = function(x) dnorm(x = x, mean = mean_x, sd = sd_x),
                    color = "IndianRed", size = 1.2) +
      xlab("Sampled data") + ylab("Density") +
      gg_tema
  }
  
})
output$norm_qqplot_trans <- renderPlot({
  data <- norm_data_trans()
  
  if(!is.null(data)){
    data <- data %>%
      mutate(y = qqnorm(x, plot = FALSE)$x)
    
    ggplot(data = data, mapping = aes(x = x, y = y)) + 
      geom_point(color = "SteelBlue", size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "IndianRed", size = 2) +
      xlab("Theoretical quantiles") + ylab("Empirical quantiles") +
      gg_tema
  }
})
output$norm_tab_trans <- DT::renderDataTable({
  data <- norm_data_trans()
  
  mean_x <- mean(data$x)
  sd_x <- sd(data$x)
  
  if(!is.null(data)){
    anderson <- ad.test(x = data$x)$p.value
    lillie <- lillie.test(x = data$x)$p.value
    ks <- ks.test(x = scale(data$x), y = "pnorm")$p.value
    shapiro <- shapiro.test(x = data$x)$p.value
    
    tab <- data.frame(`Test` = c("Anderson-darling", "Lilliefors", "Kolmogorov-smirnov", "Shapiro-wilk"),
                      `pvalor` = round(c(anderson, lillie, ks, shapiro), 2))
    
    tab <- rbind.data.frame(c("x", round(c(anderson, lillie, ks, shapiro), 2)))
    names(tab) <- c("fun", "anderson", "lilliefors", "ks", "sw")
    
    datatable(tab, escape = FALSE, selection = 'none',
              rownames = F, colnames = c("", "Anderson-darling", "Lilliefors", "Kolmogorov-smirnov", "Shapiro-wilk"), options = options_DT) %>%
      formatStyle(columns = c("anderson", "lilliefors", "ks", "sw"), 
                  backgroundColor = styleInterval(cuts = c(0.05), values = c("IndianRed", "SteelBlue"))) %>%
      formatStyle('fun',  color = 'black', backgroundColor = 'white', fontWeight = 'bold')
  }
})