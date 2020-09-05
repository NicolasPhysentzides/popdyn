
seir.shiny <- function(){

  require(shiny)
  require(ggplot2)
  require(deSolve)







shinyApp(ui = fluidPage(

  # App title ----
  titlePanel("Sliders"),

  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "gr1", "Beta:",
                   min = 0, max = 3,
                   value = 1.42, step = 0.0005 ),
      numericInput(inputId = "gr2", "Gamma:",
                   min = 0, max = 3,
                   value = 0.142, step = 0.0005),
      numericInput(inputId = "gr7", "Mu:",
                   min = 0, max = 2,
                   value = 2e-06, step = 1e-05),
      numericInput(inputId = "gr8", "Sigma:",
                   min = 0, max = 3,
                   value = 0.1, step = 0.0005),
      numericInput(inputId = "gr3", "S proportion:",
                   min = 0, max = 1,
                   value = 0.8, step = 0.005),
      numericInput(inputId = "gr9", "E proportion:",
                   min = 0, max = 1,
                   value = 0.03, step = 0.005),
      numericInput(inputId = "gr4", "I proportion:",
                   min = 0, max = 1,
                   value = 1e-06, step = 0.005),
      numericInput(inputId = "gr5", "R proportion:",
                   min = 0, max = 1,
                   value = 2e-06, step = 0.005),
      sliderInput(inputId = "gr6", "Time sequence:",
                  min = 0, max = 365, value = c(0,70))
    ),
    mainPanel(
      plotOutput("plot", click = "plot_click"),
      verbatimTextOutput("info"),
      verbatimTextOutput("info1")
    )
  )
), server = function(input, output){

  parameters <- reactive(c(beta = input$gr1, gamma = input$gr2, mu = input$gr7, sigma = input$gr8))
  initials <- reactive(c(S = input$gr3, E= input$gr9, I = input$gr4, R = input$gr5))
  times <- reactive({seq(input$gr6[1],input$gr6[2])})
  # Solve and plot.

  val <- reactive(seir(pars = parameters(), init = initials(), time = times())$results)



  output$plot <- renderPlot({
    validate(
      need(length(val()[val() < 0] == 0), 'Check at least one letter!'))


    ggplot(data = val(), mapping = aes(x = time)) +
      geom_line(aes(y = S, colour = "susceptibles ratio"), lwd = 1.2) +
      geom_line(aes(y = E, colour = "exposed ratio"), lwd = 1.2) +
      geom_line(aes(y = I, colour = "infectious ratio"), lwd = 1.2) +
      geom_line(aes(y = R, colour = "recovered ratio"), lwd = 1.2) +
      labs(title = "SEIR model" , y = "Proportion of population", x = "Time sequence") +
      theme(plot.title = element_text(hjust = 0.5))
  })

  output$info <- renderText({
    paste0("Day=", input$plot_click$x, "\nRatio=", input$plot_click$y)
  })

})


}


