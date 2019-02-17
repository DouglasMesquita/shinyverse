gibbs_help <- tabPanel(title = "Poisson with change point", 
                       value = "gibbs_help", 
                       HTML("<h1><center>Poisson with change point</center></h1>"),
                       actionBttn(inputId = "gibbs_go_app", label = "Back", icon = icon("chevron-left"), 
                                  style = "material-flat", color = "primary", size = "sm"),
                       column(width = 12,
                              column(width = 6, offset = 3,
                                     includeMarkdown("tabs/gibbs/about.md")
                              )
                       )
)
