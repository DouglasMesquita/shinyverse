home <- tabPanel(title = "Home", 
                 value = "home", 
                 HTML("<h1><center>Shinyverse</center></h1>"),
                 br(), br(),
                 column(width = 12,
                        column(width = 2,
                               app_img(src = "img/banners/ars.png", link = "app_ars", width = "100%"),
                               app_title(text = "Adaptive Rejection Sampling", html_size = "h4", link = "app_ars_text")
                        ),
                        column(width = 2,
                               app_img(src = "img/banners/gibbs.png", link = "app_gibbs", width = "100%"),
                               app_title(text = "Poisson with change point", html_size = "h4", link = "app_gibbs_text")
                        )
                        # column(width = 2,
                        #        app_img(src = "img/banners/exams.png", link = "www.google.com", width = "100%"),
                        #        app_title(text = "Medical exams", html_size = "h4", link = "www.google.com")
                        # ),
                        # column(width = 2,
                        #        app_img(src = "img/banners/medical_report.png", link = "www.google.com", width = "100%"),
                        #        app_title(text = "Automatic Medical Reports", html_size = "h4", link = "www.google.com")
                        # )
                 ),
                 column(width = 10, offset = 1,
                        br(), br(),
                        HTML("<h3><center>Did you find a mistake? Send me a message :)</center></h3>")
                 )
)
