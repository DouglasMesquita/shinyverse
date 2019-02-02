ars <- tabPanel(id = "teste",
                title = "Adaptive Rejection Sampling (ARS)",
                value = "ars", 
                br(),
                actionBttn(inputId = "ars_go_home", label = "Back", icon = icon("chevron-left"), 
                           style = "material-flat", color = "primary", size = "sm"),
                br(),
                column(width = 12, 
                       column(width = 10,
                              
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, 
                                            draggable = TRUE, 
                                            style = "z-index: 1000",
                                            top = 150, left = 60, right = "auto", bottom = "auto",
                                            width = 400, height = "auto",
                                            
                                            HTML('<button data-toggle="collapse" data-target = "#ars_absolute_panel"  
                                                         style = "margin-top: 20px; border:0; background:transparent";>
                                                            <h3 style = "color:white;">
                                                              <label class="control-label" style = "cursor:pointer;">
                                                                <i class = "fa fa-gear"></i> List of inputs
                                                              </label>
                                                            </h3>
                                                 </button>'),
                                            tags$div(id = 'ars_absolute_panel', class = "accordion-body collapse",
                                                     hr(),
                                                     br(),
                                                     conditionalPanel(
                                                       condition = "input.ars_run_button != 0",
                                                       label_blog(text = "Choose an iteration:", 
                                                                  size = "h4", color = "#fff", center = FALSE),
                                                       # uiOutput("IterationGraph_btn")
                                                       HTML("<center>"),
                                                       uiOutput("ars_animation_graph_btn"),
                                                       HTML("</center>")
                                                     ),
                                                     label_blog(text = "Distribution parameters", size = "h4", center = F),
                                                     ##-- Distribution ----
                                                     label_blog(text = "Distribution", size = "h5"),
                                                     HTML("<center>"),
                                                     pickerInput("ars_distribution", 
                                                                 label = NULL,
                                                                 choices = list("Beta" = 1,
                                                                                "Chi-square" = 2,
                                                                                "Exponential" = 3,
                                                                                "Gamma" = 4,
                                                                                "Logistic" = 5,
                                                                                "Normal" = 6,
                                                                                "Weibull" = 7),
                                                                 selected = 6
                                                     ),
                                                     HTML("</center>"),
                                                     ##-- + Parameters ----
                                                     label_blog(text = "Parameters", size = "h5"),
                                                     conditionalPanel(
                                                       condition = "input.ars_distribution == 1",
                                                       column(width = 12,
                                                              #--- beta: shape1
                                                              column(width = 6,
                                                                     label_blog(text = "Shape 1", size = "h6"),
                                                                     numericInput("ars_shape1", label = NULL,
                                                                                  value = 2, min = 1, max = 50)
                                                              ),
                                                              #--- beta: shape2
                                                              column(width = 6,
                                                                     label_blog(text = "Shape 2", size = "h6"),
                                                                     numericInput("ars_shape2", label = NULL, 
                                                                                  value = 2, min = 1, max = 50)
                                                              )
                                                       )
                                                     ),
                                                     conditionalPanel(
                                                       condition = "input.ars_distribution == 2",
                                                       #--- chi square: degrees of freedom
                                                       label_blog(text = "Degrees of freedom", size = "h6"),
                                                       numericInput("ars_gl_qui", label = NULL,
                                                                    value = 2, min = 2, max = 50)
                                                     ),
                                                     conditionalPanel(
                                                       condition = "input.ars_distribution == 3",
                                                       #--- exponential: rate
                                                       label_blog(text = "Rate", size = "h6"),
                                                       numericInput("ars_rate_exp", label = NULL, 
                                                                    value = 2, min = 1, max = 50)
                                                     ),   
                                                     conditionalPanel(
                                                       condition = "input.ars_distribution == 4",
                                                       column(width = 12,
                                                              #--- gamma: shape
                                                              column(width = 6,
                                                                     label_blog(text = "Shape", size = "h6"),
                                                                     numericInput("ars_shape_gama", label = NULL, 
                                                                                  value = 2, min = 1, max = 50)
                                                              ),
                                                              #--- gamma: rate
                                                              column(width = 6,
                                                                     label_blog(text = "Rate", size = "h6"),
                                                                     numericInput("ars_rate_gama", label = NULL, 
                                                                                  value = 1, min = 10e-10, max = 50)
                                                              )
                                                       )
                                                     ),  
                                                     conditionalPanel(
                                                       condition = "input.ars_distribution == 5",
                                                       column(width = 12,
                                                              #--- logistic: location
                                                              column(width = 6,
                                                                     label_blog(text = "Location", size = "h6"),
                                                                     numericInput("ars_location_logis", label = NULL, 
                                                                                  value = 0, min = -500, max = 500)
                                                              ),
                                                              #--- logistic: scale
                                                              column(width = 6,
                                                                     label_blog(text = "Scale", size = "h6"),
                                                                     numericInput("ars_scale_logis", label = NULL, 
                                                                                  value = 1, min = 10e-10, max = 50)
                                                              )
                                                       )
                                                     ),
                                                     conditionalPanel(
                                                       condition = "input.ars_distribution == 6",
                                                       column(width = 12,
                                                              #--- normal: mean
                                                              column(width = 6,
                                                                     label_blog(text = "Mean", size = "h6"),
                                                                     numericInput("ars_mean_normal", label = NULL, 
                                                                                  value = 0, min = -500, max = 500)
                                                              ),
                                                              #--- normal: variance
                                                              column(width = 6,
                                                                     label_blog(text = "Variance", size = "h6"),
                                                                     numericInput("ars_var_normal", label = NULL, 
                                                                                  value = 1, min = 1, max = 100)
                                                              )
                                                       )
                                                     ),
                                                     conditionalPanel(
                                                       condition = "input.ars_distribution == 7",
                                                       column(width = 12,
                                                              #--- weibull: shape
                                                              column(width = 6,
                                                                     label_blog(text = "Shape", size = "h6"),
                                                                     numericInput("ars_shape_w", label = NULL, 
                                                                                  value = 2, min = 1, max = 50)
                                                              ),
                                                              #--- weibull: scale
                                                              column(width = 6,
                                                                     label_blog(text = "Scale", size = "h6"),
                                                                     numericInput("ars_scale_w", label = NULL,
                                                                                  value = 1, min = 10e-10, max = 50)
                                                              )
                                                       )
                                                     ),
                                                     ##-- + Bounds ----
                                                     label_blog(text = "Bounds", size = "h5"),
                                                     column(width = 12,
                                                            column(width = 6,
                                                                   label_blog(text = "Lower", size = "h6"),
                                                                   numericInput(inputId = "ars_lim_inf", label = NULL, value = -2)
                                                            ),
                                                            column(width = 6,
                                                                   label_blog(text = "Upper", size = "h6"),
                                                                   numericInput(inputId = "ars_lim_sup", label = NULL, value = 2)
                                                            )
                                                     ),
                                                     ##-- ARS parameter ----
                                                     label_blog(text = "ARS parameters", size = "h4", center = F),
                                                     column(width = 12,
                                                            #--- ARS: number of observations
                                                            column(width = 6,
                                                                   label_blog(text = "Number of obs.", size = "h6"),
                                                                   numericInput("ars_n", label = "", value = 100)
                                                            ),
                                                            #--- ARS: number of initial points
                                                            column(width = 6,
                                                                   label_blog(text = "Initial points", size = "h6"),
                                                                   numericInput("ars_n_points", label = "", value = 2)
                                                            )
                                                     ),
                                                     div(style = "float: right; margin-top:20px", 
                                                         actionBttn(inputId = "ars_run_button", label = "Run!", icon = icon("retweet"), 
                                                                    style = "material-flat", color = "danger", size = "sm")
                                                         )
                                            )
                              )
                       )
                ),
                column(width = 12,
                       fluidRow(
                         conditionalPanel(
                           condition = "input.ars_run_button == 0",
                           column(width = 8, offset = 2,
                                  box_blog(cor = "#35a9bd", height = "250px", width = "100%",
                                           label_blog(text = "Use <i class = 'fa fa-gear'></i> to set some parameters and click Run!", 
                                                      color = "#fff", size = "h1", center = TRUE),
                                           div(style = "position: absolute; right:50px; bottom:50px", 
                                               actionBttn(inputId = "ars_help", label = "Help page", icon = icon("question"), 
                                                          style = "material-flat", color = "danger", size = "sm")
                                           )
                                  )
                           )
                         )
                       ),
                       fluidRow(
                         HTML("<center>"),
                         conditionalPanel(
                           condition = "input.ars_run_button > 0",
                           column(width = 4,
                                  box_blog(cor = "#6b7494", height = "125px", width = "80%", 
                                           label_blog(text = "Log-function", 
                                                      size = "h4", color = "#fff", center = TRUE),
                                           withSpinner(plotOutput("ars_log"), type = 5, color = "#ffffff")
                                  )
                           )
                         ),
                         conditionalPanel(
                           condition = "input.ars_run_button > 0",
                           column(width = 4,
                                  box_blog(cor = "#6b7494", height = "125px", width = "80%", 
                                           label_blog(text = "Histogram", 
                                                      size = "h4", color = "#fff", center = TRUE),
                                           withSpinner(plotOutput("ars_hist"), type = 5, color = "#ffffff")
                                  )
                           )
                         ),
                         conditionalPanel(
                           condition = "input.ars_run_button > 0",
                           column(width = 4,
                                  box_blog(cor = "#6b7494", height = "125px", width = "80%", 
                                           label_blog(text = "Function", 
                                                      size = "h4", color = "#fff", center = TRUE),
                                           withSpinner(plotOutput("ars_fun"), type = 5, color = "#ffffff")
                                  )
                           )
                         ),
                         conditionalPanel(
                           condition = "input.ars_run_button > 0",
                           column(width = 8, offset = 2,
                                  box_blog(cor = "#6b7494", height = "125px", width = "100%", 
                                           label_blog(text = "Aceptance Rate", 
                                                      size = "h3", color = "#fff", center = TRUE),
                                           withSpinner(plotOutput("ars_ts"), type = 5, color = "#ffffff")
                                  )
                           )
                         ),
                         HTML("</center>")
                       )
                )
)
