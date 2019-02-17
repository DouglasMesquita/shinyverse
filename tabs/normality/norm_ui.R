norm <- tabPanel(title = "Normality test and variable transformation",
                 value = "norm", 
                 HTML("<h1><center>Normality test and variable transformation</center></h1>"),
                 actionBttn(inputId = "norm_go_home", label = "Back", icon = icon("chevron-left"), 
                            style = "material-flat", color = "primary", size = "sm"),
                 column(width = 12,
                        column(width = 6, offset = 3,
                               box_blog(cor = "#6b7494", height = "100%", width = "100%", 
                                        br(),
                                        column(width = 4, 
                                               pickerInput(inputId = "norm_distribution", 
                                                           label = "Distribution", 
                                                           choices = c("Log-normal", 
                                                                       "Normal",
                                                                       "Chi-square",
                                                                       "t-student"), 
                                                           selected = "Normal")
                                        ),
                                        column(width = 4, 
                                               numericInput(inputId = "norm_n_obs", 
                                                            label = "Sample size",
                                                            value = 1000, min = 0, max = 5000,
                                                            step = 100)
                                        ),
                                        column(width = 4, 
                                               pickerInput(inputId = "norm_function", 
                                                           label = "Transformation function", 
                                                           choices = c("Exponential",
                                                                       "Identity", 
                                                                       "Logarithm",
                                                                       "Square",
                                                                       "Square root",
                                                                       "Reciprocal"), 
                                                           selected = "Identity")
                                        ),
                                        div(style = "float: left; margin-bottom:20px; margin-left:20px", 
                                            actionBttn(inputId = "norm_run", label = "Run!", icon = icon("retweet"), 
                                                       style = "material-flat", color = "danger", size = "sm")
                                        ),
                                        div(style = "float: right; margin-bottom:20px; margin-right:20px", 
                                            actionBttn(inputId = "norm_help", label = "Help page", icon = icon("question"), 
                                                       style = "material-flat", color = "danger", size = "sm")
                                        )
                               )
                        )
                 ),
                 conditionalPanel(condition = "input.norm_run > 0",
                                  column(width = 6, align = "center",
                                         HTML("<h1><center>Original scale</center></h1>"),
                                         column(width = 8, offset = 2, align = "center",
                                                withSpinner(DT::dataTableOutput("norm_tab", width = "90%"), type = 5, color = "#ffffff", proxy.height = "80px")
                                         ),
                                         br(),
                                         ##-- ++ Histogram ----
                                         column(width = 6, align = "center", 
                                                box_blog(cor = "#6b7494", height = "400px", width = "100%", 
                                                         label_blog(text = "Histogram x", size = "h4", color = "#fff", center = TRUE),
                                                         withSpinner(plotOutput("norm_hist", width = "90%", height = "340px"), type = 5, color = "#ffffff")
                                                )
                                         ),
                                         ##-- ++ QQplot ----
                                         column(width = 6, align = "center", 
                                                box_blog(cor = "#6b7494", height = "400px", width = "100%", 
                                                         label_blog(text = "Qqplot x", size = "h4", color = "#fff", center = TRUE),
                                                         withSpinner(plotOutput("norm_qqplot", width = "90%", height = "340px"), type = 5, color = "#ffffff")
                                                )
                                         )
                                  ),
                                  column(width = 6, align = "center",
                                         HTML("<h1><center>Transformed scale</center></h1>"),
                                         column(width = 8, offset = 2, align = "center",
                                                withSpinner(DT::dataTableOutput("norm_tab_trans", width = "90%"), type = 5, color = "#ffffff", proxy.height = "80px")
                                         ),
                                         br(),
                                         ##-- ++ Histogram transformed ----
                                         column(width = 6, align = "center", 
                                                box_blog(cor = "#6b7494", height = "400px", width = "100%", 
                                                         label_blog(text = "Histogram g(x)", size = "h4", color = "#fff", center = TRUE),
                                                         withSpinner(plotOutput("norm_hist_trans", width = "90%", height = "340px"), type = 5, color = "#ffffff")
                                                )
                                         ),
                                         ##-- ++ QQplot transformed ----
                                         column(width = 6, align = "center", 
                                                box_blog(cor = "#6b7494", height = "400px", width = "100%", 
                                                         label_blog(text = "Qqplot g(x)", size = "h4", color = "#fff", center = TRUE),
                                                         withSpinner(plotOutput("norm_qqplot_trans", width = "90%", height = "340px"), type = 5, color = "#ffffff")
                                                )
                                         )
                                  )
                 )
)     
