##-- Home ----
observeEvent(c(input$ars_go_home, input$gibbs_go_home, input$norm_go_home, input$glm_go_home),{
  updateTabsetPanel(session = session, inputId = "apps", selected = "home")
})

##-- ARS ----
observeEvent(input$app_ars,{
  updateTabsetPanel(session = session, inputId = "apps", selected = "ars")
})
observeEvent(input$app_ars_text,{
  updateTabsetPanel(session = session, inputId = "apps", selected = "ars")
})

##-- Gibbs ----
observeEvent(input$app_gibbs,{
  updateTabsetPanel(session = session, inputId = "apps", selected = "gibbs")
})
observeEvent(input$app_gibbs_text,{
  updateTabsetPanel(session = session, inputId = "apps", selected = "gibbs")
})

##-- Normality ----
observeEvent(input$app_norm,{
  updateTabsetPanel(session = session, inputId = "apps", selected = "norm")
})
observeEvent(input$app_norm_text,{
  updateTabsetPanel(session = session, inputId = "apps", selected = "norm")
})

##-- GLM ----
observeEvent(input$app_glm,{
  updateTabsetPanel(session = session, inputId = "apps", selected = "glm")
})
observeEvent(input$app_glm_text,{
  updateTabsetPanel(session = session, inputId = "apps", selected = "glm")
})