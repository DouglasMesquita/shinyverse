gibbs <- tabPanel(title = "Poisson with change point", 
                  value = "gibbs", 
                  HTML("<h1><center>Poisson with change point</center></h1>"),
                  actionBttn(inputId = "gibbs_go_home", label = "Back", icon = icon("chevron-left"), 
                             style = "material-flat", color = "primary", size = "sm"),
                  useShinyjs(),
                  column(width = 12,
                         column(2, 
                                label_blog(text = "Select the size of time serie.", size = "h4", color = "black", center = TRUE),
                                label_blog(text = "Number of observations:", size = "h5", color = "black", center = FALSE),
                                numericInput('gibbs_n_obs', label = NULL, value = 2000),
                                div(style = "float: left; margin-top:20px",
                                    actionBttn(inputId = "gibbs_run", label = "Run!", icon = icon("retweet"),
                                               style = "material-flat", color = "danger", size = "sm")
                                ),
                                div(style = "float: right; margin-top:20px",
                                    actionBttn(inputId = "gibbs_help", label = "Help page", icon = icon("question"), 
                                               style = "material-flat", color = "danger", size = "sm")
                                )
                         ),
                         conditionalPanel(
                           condition = "input.gibbs_run == 0",
                           column(width = 8, 
                                  box_blog(cor = "#35a9bd", height = "250px", width = "100%",
                                           label_blog(text = "Set up you model and click Run!", 
                                                      color = "#fff", size = "h1", center = TRUE),
                                           div(style = "position: absolute; right:50px; bottom:50px", 
                                               actionBttn(inputId = "gibbs_help_ini", label = "Help page", icon = icon("question"), 
                                                          style = "material-flat", color = "danger", size = "sm")
                                           )
                                  )
                           )
                         ),
                         conditionalPanel(
                           condition = "input.gibbs_run > 0",
                           column(8,
                                  column(12,
                                         label_blog(text = "Click on the change point! ", size = "h1", color = "black", center = TRUE),
                                         plotOutput(outputId = 'gibbs_serie_graph',
                                                    click = "gibbs_moment_click")
                                  ),
                                  column(6, align = "center",
                                         hidden(
                                           prettyToggle(
                                             inputId = "gibbs_show_gibbs",
                                             label_on = "Hide Gibbs estimation", 
                                             label_off = "Show Gibbs estimation.",
                                             outline = TRUE,
                                             plain = TRUE,
                                             icon_on = icon("thumbs-up"), 
                                             icon_off = icon("thumbs-down")
                                           )
                                         )
                                  ),
                                  column(6, align = "center",
                                         hidden(
                                           prettyToggle(
                                             inputId = "gibbs_show_true",
                                             label_on = "Hide the true point", 
                                             label_off = "Show the true point",
                                             outline = TRUE,
                                             plain = TRUE,
                                             icon_on = icon("thumbs-up"), 
                                             icon_off = icon("thumbs-down")
                                           )
                                         )
                                  )
                           )
                         ),
                         column(2,
                                label_blog(text = "MCMC setup.", size = "h4", color = "black", center = TRUE),
                                label_blog(text = "Number of iterations:", size = "h5", color = "black", center = FALSE),
                                numericInput('gibbs_n', label = NULL, value = 2000),
                                label_blog(text = "Burn-in:", size = "h5", color = "black", center = FALSE),
                                numericInput('gibbs_burnin', label = NULL, value = 1000)
                         )
                  ),
                  column(width = 12,
                         column(width = 4,
                                column(width = 6,
                                       htmlOutput(outputId = "gibbs_user_rate")
                                ),
                                column(width = 6,
                                       htmlOutput(outputId = "gibbs_rate")
                                )
                         ),
                         column(width = 4, align = "center",
                                column(width = 6,
                                       htmlOutput(outputId = "gibbs_user_point")
                                ),
                                column(width = 6,
                                       htmlOutput(outputId = "gibbs_point")
                                ),
                                column(width = 6, offset = 3, align = "center",
                                       htmlOutput(outputId = "gibbs_true_point")
                                )
                         ),
                         column(width = 4, align = "center",
                                column(width = 6,
                                       htmlOutput(outputId = "gibbs_n_user")
                                ),
                                column(width = 6,
                                       htmlOutput(outputId = "gibbs_n_gibbs")
                                )
                         )
                  )
)
