
library(shiny)
library(shinyjs)

library(covidprobability)


ui <- fluidPage(


  titlePanel("COVID-19 Probability Calculator"),

  br(),

  "This calculator can be used to estimate the probability of an undetected
   COVID-19 case among a unit (e.g. hospital floor) following exposures to a
   number of individuals (e.g. by a staff member who later tested positive),
   given there are no symptomatic cases and given all exposed were tested with
  PCR and negative on specified day.",

  "Please see the ",
  tags$a(href = "https://eebrown.github.io/covidprobability/", "documentation"),
  "for parameter sources, assumptions, limitations and other important details.",

  br(),
  br(),

  useShinyjs(),
  actionButton("btn", "Advanced parameters"),

  br(),
  br(),
  fluidRow(
    column(6,
      sliderInput(inputId = "in_asymp", label = "Expected proportion of asymptomatic cases in setting", value = 0.278, min = 0, max = 1),
      sliderInput(inputId = "in_pre", label = "Pre-test probability given nature of exposure", value = 0.13, min = 0, max = 1),
      sliderInput(inputId = "in_mu", label = "Incubation period, mu (lognormal distribution)", value = 1.63, min = 0.5, max = 4),
    ),
    column(6,
      sliderInput(inputId = "in_testday", label = "Day testing negative (days since exposure)", value = 9, min = 2, max = 14),
      sliderInput(inputId = "in_n", label = "Number of exposed individuals", value = 10, min = 0, max = 100),
      sliderInput(inputId = "in_sigma", label = "Incubation period, sigma (lognormal distribution)", value = 0.5, min = 0.1, max = 2),
    ),

  ),

  plotOutput("main_plot"),

  br(),
  "Package version:",
  textOutput("version"),
  "Author:", br(),
  "Eric Brown"

)

server <- function(input, output) {

  hide("in_mu")
  hide("in_sigma")

  observeEvent(input$btn, {
    # Change the following line for more examples
    toggle("in_mu")
    toggle("in_sigma")
  })

  output$main_plot <- renderPlot({

    test_n <- unit_probability(test_day = input$in_testday,
                               pre0 =  input$in_pre,
                               sens = sens,
                               spec = 1,
                               mu = input$in_mu,
                               sigma = input$in_sigma,
                               days = 14,
                               asympt = input$in_asymp,
                               n = input$in_n)

    plot(1:14, test_n$point, type="b", ylim=c(0,1),
         main = "Unit-wide probabiltiy of undetected COVID-19",
         xlab = "Days since exposure(s)",
         ylab = "Probability of any undetected COVID-19 case")
    lines(test_n$lower, type="l", col="grey")
    lines(test_n$upper, type="l", col="grey")
    abline(v = input$in_testday)
    text(1:14, (test_n$point + 0.1), round(test_n$point, 2), cex = 1)

  })

  output$version <- renderText({as.character(packageVersion("covidprobability"))})

}

shinyApp(ui = ui, server = server)
