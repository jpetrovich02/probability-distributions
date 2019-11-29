# Define UI for random distribution app ----
ui <- fluidPage(
  withMathJax(),
  
  # App title ----
  titlePanel("The Exponential Distribution"),
  
  tabsetPanel(
    
    tabPanel("Explore", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 #Introductory Text
                 p("The exponential distribution is a continuous probability distribution often used to model the
                    amount of time until an event occurs, assuming that events occur at a rate of \\( \\lambda\\)."),
                 p("Change the value of \\( \\lambda \\) to see how the shape of the distribution is
                    affected by this parameter."),
                 
                 # Input: Slider for the rate parameter, lambda
                 sliderInput("lambda",
                             label = 'Rate parameter (\\( \\lambda \\))',
                             value = 3,
                             min = 0,
                             max = 10,
                             step = 0.1)
               ),
               mainPanel(
                 plotOutput("explore")
               )
             )

    ),
    tabPanel("Find Probability", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 numericInput("lambda2",label = "Rate parameter",value = 1,min = 0.001,max = 1e12,step = 0.1),
                 selectInput("prob",label = "Select Type of Probability",
                              choices = c("Lower Tail: P(X < x)" = "lower",
                                          "Upper Tail: P(X > x)" = "upper",
                                          "Interval: P(a < X < b)" = "interval")),
                 conditionalPanel(condition = "input.prob == 'interval'",
                                  numericInput("a",label = "Specify a:",value = 1,min = 0,max = 1e12,step = 0.1),
                                  numericInput("b",label = "Specify b:",value = 0,min = 0,max = 1e12,step = 0.1)
                 ),
                 conditionalPanel(condition = "input.prob != 'interval'",
                                  numericInput("x_quant",label = "Specify x:",value = 2.9957,min = 0,max = 1e12,step = 0.1)
                 )
               ),
               mainPanel(
                 plotOutput("probability")
               )
             )

    ),
    tabPanel("Find Percentile/Quantile", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 numericInput("lambda3",label = "Rate parameter",value = 1,min = 0.001,max = 1e12,step = 0.1),
                 selectInput("quant",label = "Select Type of Probability",
                             choices = c("Lower Tail" = "lower",
                                         "Upper Tail" = "upper")),
                 conditionalPanel(condition = "input.quant == 'lower'",
                                  numericInput("x_prob_l",label = "Probability in lower tail (in %):",
                                               value = 95,min = 0,max = 100,step = 0.1)
                 ),
                 conditionalPanel(condition = "input.quant == 'upper'",
                                  numericInput("x_prob_u",label = "Probability in upper tail (in %):",
                                               value = 5,min = 0,max = 100,step = 0.1)
                 )
               ),
               mainPanel(
                 plotOutput("percentile")
               )
             )
             
    )
  )
)