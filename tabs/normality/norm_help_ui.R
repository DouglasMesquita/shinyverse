norm_help <- tabPanel(title = "Normality test and variable transformation", 
                      value = "norm_help", 
                      HTML("<h1><center>Normality test and variable transformation</center></h1>"),
                      actionBttn(inputId = "norm_go_app", label = "Back", icon = icon("chevron-left"), 
                                 style = "material-flat", color = "primary", size = "sm"),
                      column(width = 12,
                             column(width = 6, offset = 3,
                                    includeMarkdown("tabs/normality/about.md")
                             )
                      )
)
