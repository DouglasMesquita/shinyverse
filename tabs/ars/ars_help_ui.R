ars_help <- tabPanel(title = "Adaptive Rejection Sampling (ARS)", 
                     value = "ars_help", 
                     br(),
                     actionBttn(inputId = "ars_go_app", label = "Back", icon = icon("chevron-left"), 
                                    style = "material-flat", color = "primary", size = "sm"),
                     br(),
                     column(width = 6, offset = 3,
                            includeMarkdown("tabs/ars/about.md")
                     )
)
