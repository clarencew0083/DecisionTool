ui <- fluidPage(title = 'Test Slider',
                theme = shinythemes::shinytheme('flatly'),
                sidebarLayout(
                  sidebarPanel(

                            selectInput("SelectIndicator", "Select Criteria", choices = criteria),

                            sliderInput('SelectIndicatorWeight', 'Select Criteria Weight',
                                        min=0, max=4, value= main.weights[1,1], # initial value
                                        step=0.1, round=0)


                  ),
                  mainPanel(tableOutput('mytable'),
                            br(),
                            plotlyOutput("Plot"))
                )
)
