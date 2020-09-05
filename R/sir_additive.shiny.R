sir_additive.shiny <- function(){
  require(deSolve)
  require(shiny)
  require(ggplot2)

shinyApp(ui = fluidPage(

  # App title ----
  titlePanel("Sliders"),

  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "gr1", "Beta:",
                   min = 0, max = 4,
                   value = 1.42, step = 0.0005),
      numericInput(inputId = "gr2", "Gamma:",
                   min = 0, max = 4,
                   value = 0.142, step = 0.0005),
      numericInput(inputId = "gr8", "Psi:",
                   min = 0, max = 40,
                   value = 2, step = 1),
      numericInput(inputId = "gr9", "N:",
                   min = 0, max = 40,
                   value = 400000, step = 1),
      numericInput(inputId = "gr7", "Mu:",
                   min = 0, max = 10,
                   value = 2e-06, step = 1e-05),
      numericInput(inputId = "gr3", "Number of S:",
                   min = 0, max = 100000,
                   value = 20000, step = 1),
      numericInput(inputId = "gr4", "Number of I:",
                   min = 0, max = 100000,
                   value = 500, step = 1),
      numericInput(inputId = "gr10", "Number of R:",
                   min = 0, max = 100000,
                   value = 0, step = 1),
      numericInput(inputId = "gr5", "Step Size:",
                   min = 1, max = 10,
                   value = 1, step = 1),
      numericInput(inputId = "gr11", "Number of simulations:",
                   min = 1, max = 10,
                   value = 3, step = 1),
      sliderInput(inputId = "gr6", "Time sequence:",
                  min = 0, max = 365, value = c(0,70))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
), server = function(input, output){

  parameters <- reactive(c(beta = input$gr1, gamma = input$gr2, mu = input$gr7, psi = input$gr8, N = input$gr9))
  initials <- reactive(c(S = input$gr3, I = input$gr4, R = input$gr10))
  times <- reactive({seq(input$gr6[1],input$gr6[2])})

  # Solve and plot.

  #val <-

  require(plyr)
  simdat <- reactive(rdply(
    input$gr11,
    sir_additive(pars = parameters(), init = initials(), time = times(), step = input$gr5)$results
  ))

  output$plot <- renderPlot({
    validate(
      need(length(simdat()[simdat() < 0] == 0), 'Check at least one letter!'))

    par(mfrow=c(3,1),  mar=c(2,4,1,1)) #mar=c(4,4,1,2)

    plot(S~time,data=simdat(),type='n')
    d_ply(simdat(),".n",function(x)lines(S~time,data=x,col=.n))

    plot(I~time,data=simdat(),type='n')
    d_ply(simdat(),".n",function(x)lines(I~time,data=x,col=.n))

    plot(R~time,data=simdat(),type='n')
    d_ply(simdat(),".n",function(x)lines(R~time,data=x,col=.n))
  })



})




}
