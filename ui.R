shinyUI(
  fluidPage(
    ##-- Favicon ----
    tags$head(
      tags$link(rel = "shortcut icon", href = "img/logo.ico"),
      #-- biblio js ----
      tags$link(rel="stylesheet", type = "text/css",
                href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"),
      tags$link(rel="stylesheet", type = "text/css",
                href = "https://fonts.googleapis.com/css?family=Open+Sans|Source+Sans+Pro")
    ),
    ##-- Header and body ----
    navbarPage(title = "require(R)",
               id = "apps",
               selected = "home",
               theme = "styles.css", 
               fluid = T,
               ##-- Header ----
               div(includeHTML("html/header.html")),
               ##-- Abas ----
               home,
               ##-- ars ----
               ars,
               ars_help,
               ##-- gibbs ----
               gibbs
    ),
    ##-- Footer ----
    div(class = "footer", includeHTML("html/footer.html")),
    div(includeHTML("html/google_analytics.html"))
  )
)