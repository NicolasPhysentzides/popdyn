sirs_nsimul.shiny <- function(){
  require(dplyr)
  require(shiny)
  require(ggplot2)


shinyApp(ui = fluidPage(

  # App title ----
  titlePanel("Sliders"),

  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "gr1", "Beta:",
                   min = 0, max = 10000,
                   value = 20, step = 1 ),
      numericInput(inputId = "gr2", "Gamma:",
                   min = 0, max = 10000,
                   value = 13, step = 1),
      numericInput(inputId = "gr7", "Mu:",
                   min = 0, max = 10000,
                   value = 0, step = 1),
      numericInput(inputId = "gr11", "Xi:",
                   min = 0, max = 10000,
                   value = 2, step = 1),
      numericInput(inputId = "gr3", "Number of S:",
                   min = 0, max = 100000,
                   value = 595, step = 1),
      numericInput(inputId = "gr4", "Number of I:",
                   min = 0, max = 100000,
                   value = 5, step = 1),
      numericInput(inputId = "gr5", "Number of R:",
                   min = 0, max = 100000,
                   value = 0, step = 1),
      numericInput(inputId = "gr8", "maxstep:",
                   min = 10, max = 50000,
                   value = 1000, step = 10),
      numericInput(inputId = "gr9", "Number of simulations:",
                   min = 1, max = 100,
                   value = 5, step = 1)
      #numericInput(inputId = "gr10", "Time sequence:",
      #min = 0, max = 0)
    ),
    mainPanel(
      plotOutput("plot", height = "600px")

    )
  )
), server = function(input, output){

  params <- reactive(c(beta = input$gr1, gamma = input$gr2, mu = input$gr7, xi = input$gr11))
  x <- reactive(c(time = 0, S = input$gr3, I = input$gr4, R = input$gr5))
  #maxstep1 < - reactive((maxstep = input$gr8))
  #nsims1 < - reactive(nsims = input$gr9)
  #times <- reactive({seq(input$gr6[1],input$gr6[2])})
  # Solve and plot.

  val <- reactive(sir_nsimul(x = x(), params = params(), maxstep = input$gr8, nsims = input$gr9))



  output$plot <- renderPlot({

    par(mfrow=c(3,1),  mar=c(2,4,1,1)) #mar=c(4,4,1,2)

    plot(S~time,data=val(),type='n')
    d_ply(val(),".n",function(x)lines(S~time,data=x,col=.n))

    plot(I~time,data=val(),type='n')
    d_ply(val(),".n",function(x)lines(I~time,data=x,col=.n))

    plot(R~time,data=val(),type='n')
    d_ply(val(),".n",function(x)lines(R~time,data=x,col=.n))



  })



})

}
