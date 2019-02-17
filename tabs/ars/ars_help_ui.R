ars_help <- tabPanel(title = "Adaptive Rejection Sampling (ARS)", 
                     value = "ars_help", 
                     HTML("<h1><center>Adaptive Rejection Sampling (ARS)</center></h1>"),
                     actionBttn(inputId = "ars_go_app", label = "Back", icon = icon("chevron-left"), 
                                    style = "material-flat", color = "primary", size = "sm"),
                     column(width = 12,
                            column(width = 6, offset = 3,
                                   includeMarkdown("tabs/ars/about.md")
                            )
                     )
)
