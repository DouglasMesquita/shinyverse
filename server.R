shinyServer(function(input, output, session){
  ##-- HOME ----
  source("tabs/home/home_server.R", local = TRUE)
  ##-- ars ----
  source("tabs/ars/ars_server.R", local = TRUE)
  ##-- gibbs ----
  source("tabs/gibbs/gibbs_server.R", local = TRUE)
})