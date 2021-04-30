server <- function(input, output, session) {

  # initialize the weights
  main_weights = reactiveValues(df = main.weights)
  subCriteria1_weights = reactiveValues(df = subcriteria1.weights)
  subCriteria2_weights = reactiveValues(df = subCriteria2.weights)
  #main_weights$df = main.weights


  # If selected element changes, then update the slider
  observeEvent(input$SelectIndicatorMain, {
    selected_mainWeight = main_weights$df[input$SelectIndicatorMain,1]
    updateSliderInput(session, "SelectIndicatorMainWeight", value = selected_mainWeight)
  })

  # If slider changes, update the weights reactiveValues
  observeEvent(input$SelectIndicatorMainWeight,
               {
                 main_weights$df[input$SelectIndicatorMain,1] <- input$SelectIndicatorMainWeight
               })

  # If selected element changes, then update the slider
  observeEvent(input$SelectIndicatorSub1, {
    selected_sub1Weight = subCriteria1_weights$df[input$SelectIndicatorSub1,1]
    updateSliderInput(session, "SelectIndicatorSub1Weight", value = selected_sub1Weight)
  })

  # If slider changes, update the weights reactiveValues
  observeEvent(input$SelectIndicatorSub1Weight,
               {
                 subCriteria1_weights$df[input$SelectIndicatorSub1,1] <- input$SelectIndicatorSub1Weight
               })

  # If selected element changes, then update the slider
  observeEvent(input$SelectIndicatorSub2, {
    selected_sub2Weight = subCriteria2_weights$df[input$SelectIndicatorSub2,1]
    updateSliderInput(session, "SelectIndicatorSub2Weight", value = selected_sub2Weight)
  })

  # If slider changes, update the weights reactiveValues
  observeEvent(input$SelectIndicatorSub2Weight,
               {
                 subCriteria2_weights$df[input$SelectIndicatorSub2,1] <- input$SelectIndicatorSub2Weight
               })

  # Output the table
  output$mytable <- renderTable(bordered = TRUE, {

    weights = main_weights$df # get weights
    colnames(weights) <- "New Weight"
    #norm.weights = sapply(names(my_object), function(x) {main_weights[[x]]/Reduce(`+`, main_weights)})
    norm.weights <- main_weights$df/sum(main_weights$df)
    colnames(norm.weights) <- "New Normalized Weight"

    cbind(criteria, weights, norm.weights)

    #rename(Company = company.unduplicated, Sum = lump.sum.2, Count = n)
  })

  output$Plot <- renderPlotly({
    main.weights <- data.frame(main_weights$df)
    subcriteria1.weights <- data.frame(subCriteria1_weights$df)
    subcriteria2.weights <- data.frame(subCriteria2_weights$df)
    score1 <- as.data.frame((as.matrix(subcriteria1.altScore) %*% t(as.matrix(subcriteria1.weights))) * main.weights[1,1])
    score2 <- as.data.frame((as.matrix(subCriteria2.altScore) %*% t(as.matrix(subcriteria2.weights))) * main.weights[2,1])
    scoreMatrix <- cbind(score1, score2)
    row.names(scoreMatrix) <- alternatives



    scoreMatrix <- scoreMatrix %>% mutate(id = alternatives)

    mydf.molten <- melt(scoreMatrix, value.name="Count", variable.name="Variable", na.rm=TRUE)


    p1 <- ggplot(mydf.molten, aes(fill=Variable, y=Count, x=reorder(id, Count))) +
      geom_bar(position="stack", stat="identity", width = 0.3) +
      ggtitle("Ranking of Alternatives") +  xlab("Alternative") + ylab("Score") +
      coord_flip() +
      theme(axis.text.x = element_text(face="bold", color="#008000",
                                       size=8, angle=0),
            axis.text.y = element_text(face="bold", color="#008000",
                                       size=8, angle=0))

    p1 + theme_bw()
    print(p1)

  })

}
