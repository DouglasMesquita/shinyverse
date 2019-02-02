##-- Observe runButton to update other buttons
observeEvent(input$gibbs_run, {
  updatePrettyToggle(session = session, inputId = "gibbs_show_true", value = FALSE)
  x_user <<- NULL
  updatePrettyToggle(session = session, inputId = "gibbs_show_gibbs", value = FALSE)
  
  hide(id = "gibbs_show_gibbs")
  hide(id = "gibbs_show_true")
  
  hide(id = "gibbs_user_rate")
  hide(id = "gibbs_rate")
  hide(id = "gibbs_user_point")
  hide(id = "gibbs_point")
  hide(id = "gibbs_true_point")
  hide(id = "gibbs_n_user")
  hide(id = "gibbs_n_gibbs")
  
  disable(id = "gibbs_run")
  
  gibbs_update <<- TRUE
  gibbs_user_point <<- FALSE
})

##-- Poisson model with change point
gibbs_mcmc <- eventReactive(input$gibbs_run, {
  
  n <- input$gibbs_n_obs                    # Serie length
  m <- sample(2:(n-1), 1)                   # Change point
  taxa1 <- 10                               # First mean
  taxa2 <- 12                               # Second mean
  y1 <- rpois(n = m, lambda = taxa1)
  y2 <- rpois(n = (n-m), lambda = taxa2)
  y <- data.frame(y = c(y1, y2), x = 1:n)
  
  ##-- Gibbs
  tamCadeia <- input$gibbs_n
  burnin <- input$gibbs_burnin
  
  if(tamCadeia < burnin){
    showModal(modalDialog(
      span('The number of MCMC iteration must be greater than burn-in.'),
      footer = tagList(
        modalButton(label = "Ok")
      ))
    )
  } else{
    ##-- To store the chains
    MCMC_taxa_1 <- rep(0, tamCadeia)
    MCMC_taxa_2 <- rep(0, tamCadeia)
    MCMC_m <- rep(0, tamCadeia)
    
    ##-- Priors
    a1 <- 0.001
    b1 <- 0.001
    a2 <- 0.001
    b2 <- 0.001
    
    ##-- Initial points
    MCMC_taxa_1[1] <- 10
    MCMC_taxa_2[1] <- 10
    MCMC_m[1] <- n/2
    
    y_acum <- cumsum(y$y)
    y_total <- sum(y$y)
    m_seq <- 1:n
    
    ##-- Loop
    for(i in 2:tamCadeia){
      soma_y <- y_acum[MCMC_m[i-1]]
      soma_y_compl <- y_total - soma_y
      
      a1_new <- a1 + soma_y
      b1_new <- b1 + MCMC_m[i-1]
      a2_new <- a2 + soma_y_compl
      b2_new <- b2 + n - MCMC_m[i-1]
      
      MCMC_taxa_1[i] <- rgamma(1, a1_new, rate = b1_new)
      MCMC_taxa_1[i] <- ifelse(MCMC_taxa_1[i] == 0, 0.1, MCMC_taxa_1[i])
      MCMC_taxa_2[i] <- rgamma(1, a2_new, rate = b2_new)
      MCMC_taxa_2[i] <- ifelse(MCMC_taxa_2[i] == 0, 0.1, MCMC_taxa_2[i])
      
      aux <- y_acum[m_seq]*log(MCMC_taxa_1[i]) + (y_total - y_acum[m_seq])*log(MCMC_taxa_2[i])
      log_probs <- -m_seq*MCMC_taxa_1[i]-(n-m_seq)*MCMC_taxa_2[i] + aux
      
      ## To do: better solution
      maxLogProb <- max(abs(log_probs))
      sinal <- ifelse(abs(min(log_probs)) > abs(max(log_probs)), 1, -1)
      log_probs <- log_probs + sinal*maxLogProb
      log_probs <- ifelse(log_probs > 709, 709, log_probs)
      
      MCMC_m[i] <- sample(x = m_seq, size = 1, prob = exp(log_probs))
    }
    
    MCMC_dados <- data.frame(taxa1 = MCMC_taxa_1[-c(1:burnin)],
                             taxa2 = MCMC_taxa_2[-c(1:burnin)],
                             m = MCMC_m[-c(1:burnin)],
                             iteracao = 1:(tamCadeia-burnin))
    
    return(list(MCMC = MCMC_dados, y = y, n = n, m = m))
  }
})

##-- Plots
gibbs_graph <-  eventReactive(c(input$gibbs_show_true, input$gibbs_show_gibbs),{
  gibbs <- gibbs_mcmc()
  
  if(!is.null(gibbs)){
    
    serie <- ggplot(data = gibbs$y) + geom_line(aes(y = y, x = x), color = gibbs_ts_col) +
      ylab("y") + xlab("time") +
      gg_tema
    
    if(!gibbs_user_point) x_user <- NULL
    x_gibbs <- mean(gibbs$MCMC$m)
    x_true <- gibbs$m
    
    if(gibbs_user_point){
      x_user <- x_user ## Global value
      serie <- serie + geom_vline(xintercept = x_user, 
                                  col = gibbs_user_col, size = 1.5)
    }
    
    if(input$gibbs_show_gibbs){
      serie <- serie + geom_vline(xintercept = x_gibbs, 
                                  col = gibbs_gibbs_col, size = 1.5)
    }
    
    if(input$gibbs_show_true){
      serie <- serie + geom_vline(xintercept = x_true, 
                                  col = gibbs_true_col, size = 1.5)
    }
    return(list(serie = serie, x_gibbs = x_gibbs, x_true = x_true))
  }
})
output$gibbs_serie_graph <- renderPlot({
  gibbs_graph()$serie
})

##-- Other outputs
observeEvent(input$gibbs_moment_click,{
  
  if(gibbs_update){
    gibbs_user_point <<- TRUE
    
    x_user <<- round(input$gibbs_moment_click$x)
    x_gibbs <- gibbs_graph()$x_gibbs
    x_true <- gibbs_graph()$x_true
    updatePrettyToggle(session = session, inputId = "gibbs_show_gibbs", value = TRUE)
    updatePrettyToggle(session = session, inputId = "gibbs_show_true", value = TRUE)
    
    dist_user <- abs(x_user - x_true)
    dist_gibbs <- abs(x_gibbs - x_true)
    
    gibbs_error <<- ((dist_gibbs/x_true) + gibbs_error*(gibbs_won + user_won))/(gibbs_won + user_won + 1)
    user_error <<- ((dist_user/x_true) + user_error*(gibbs_won + user_won))/(gibbs_won + user_won + 1)
    
    if(dist_user >= dist_gibbs){
      sendSweetAlert(
        session = session,
        title = "You lost",
        text = "Oups!",
        type = "error"
      )
      gibbs_won <<- gibbs_won + 1
    } else{
      sendSweetAlert(
        session = session,
        title = "You won!",
        text = "Congratulations",
        type = "success"
      )
      user_won <<- user_won + 1
    }
    
    show(id = "gibbs_show_gibbs")
    show(id = "gibbs_show_true")
    
    show(id = "gibbs_user_rate")
    show(id = "gibbs_rate")
    show(id = "gibbs_user_point")
    show(id = "gibbs_point")
    show(id = "gibbs_true_point")
    show(id = "gibbs_n_user")
    show(id = "gibbs_n_gibbs")
    
    gibbs_widht <- "110%" 
    
    output$gibbs_user_point <- renderUI({
      tab_blog(texto = "User change point", numero = round(x_user), cor = gibbs_user_col, icon = "gibbs/user.png", width = gibbs_widht)
    })
    
    output$gibbs_point <- renderUI({
      tab_blog(texto = "Gibbs estimative", numero = round(x_gibbs), cor = gibbs_gibbs_col, icon = "gibbs/gibbs.png", width = gibbs_widht)
    })
    
    output$gibbs_true_point <- renderUI({
      tab_blog(texto = "True change point", numero = x_true, cor = gibbs_true_col, icon = "gibbs/true.png", width = gibbs_widht)
    })
    
    output$gibbs_rate <- renderUI({
      tab_blog(texto = "MAPE gibbs sampling", numero = paste(round(100*gibbs_error, 2), "%"), cor = gibbs_others_col, icon = "gibbs/perc.png", width = gibbs_widht)
    })
    
    output$gibbs_user_rate <- renderUI({
      tab_blog(texto = "MAPE user", numero = paste(round(100*user_error, 2), "%"), cor = gibbs_others_col, icon = "gibbs/perc.png", width = gibbs_widht)
    })
    
    output$gibbs_n_user <- renderUI({
      tab_blog(texto = "User wins", numero = user_won, cor = gibbs_others_col, icon = "gibbs/user.png", width = gibbs_widht)
    })
    
    output$gibbs_n_gibbs <- renderUI({
      tab_blog(texto = "Gibbs wins", numero = gibbs_won, cor = gibbs_others_col, icon = "gibbs/gibbs.png", width = gibbs_widht)
    }) 
    
    gibbs_update <<- !gibbs_update
    enable(id = "gibbs_run")
  }
  
})