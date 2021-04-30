ui <- fluidPage(title = 'Test Slider',
                theme = shinythemes::shinytheme('flatly'),
                sidebarLayout(
                  sidebarPanel(

                            selectInput("SelectIndicatorMain", "Select Criteria", choices = criteria),

                            sliderInput('SelectIndicatorMainWeight', 'Select Criteria Weight',
                                        min=0, max=1, value= main.weights[1,1], # initial value
                                        step=0.05, round=0),
                            actionButton("reset", "Reset"),
                            br(),
                            br(),
                            br(),
                            selectInput("SelectIndicatorSub1", "Select Criteria", choices = subCriteria1.attributes),

                            sliderInput('SelectIndicatorSub1Weight', 'Select Sub Criteria 1 Weight',
                                        min=0, max=1, value= subcriteria1.weights[1,1], # initial value
                                        step=0.05, round=0),
                            actionButton("resetSub1", "Reset"),
                            br(),
                            br(),
                            br(),
                            selectInput("SelectIndicatorSub2", "Select Criteria", choices = subCriteria2.attributes),

                            sliderInput('SelectIndicatorSub2Weight', 'Select Sub Criteria 2 Weight',
                                        min=0, max=1, value= subCriteria2.weights[1,1], # initial value
                                        step=0.05, round=0),
                            actionButton("resetSub2", "Reset")

                  ),
                  mainPanel(tableOutput('mytable'),
                            br(),
                            plotlyOutput("Plot"))
                )
)
