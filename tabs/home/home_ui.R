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
                        ),
                        column(width = 2,
                               app_img(src = "img/banners/normality.png", link = "app_norm", width = "100%"),
                               app_title(text = "Normality tests", html_size = "h4", link = "app_norm_text")
                        ),
                        column(width = 2,
                               app_img(src = "img/banners/glm.png", link = "app_glm", width = "100%"),
                               app_title(text = "GLM model", html_size = "h4", link = "app_glm_text")
                        )
                 ),
                 column(width = 10, offset = 1,
                        br(), br(),
                        HTML("<h3><center>Did you find a mistake? Send me a message :)</center></h3>")
                 )
)