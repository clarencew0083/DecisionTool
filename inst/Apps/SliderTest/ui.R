ui <- fluidPage(title = 'Test Slider',
                theme = shinythemes::shinytheme('flatly'),
                sidebarLayout(
                  sidebarPanel(

                            selectInput("SelectIndicator", "Select Criteria", choices = criteria),

                            sliderInput('SelectIndicatorWeight', 'Select Criteria Weight',
                                        min=0, max=1, value= main.weights[1,1], # initial value
                                        step=0.05, round=0),
                            actionButton("reset", "Reset")


                  ),
                  mainPanel(tableOutput('mytable'),
                            br(),
                            plotlyOutput("Plot"))
                )
)
