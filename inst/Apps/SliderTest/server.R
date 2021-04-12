server <- function(input, output, session) {

  # initialize the weights
  my_weights = reactiveValues(df = main.weights)
  #my_weights$df = main.weights


  # If selected element changes, then update the slider
  observeEvent(input$SelectIndicator, {
    selected_weight = my_weights$df[input$SelectIndicator,1]
    updateSliderInput(session, "SelectIndicatorWeight", value = selected_weight)
  })

  # If slider changes, update the weights reactiveValues
  observeEvent(input$SelectIndicatorWeight,
               {
                 my_weights$df[input$SelectIndicator,1] <- input$SelectIndicatorWeight
               })

  # Output the table
  output$mytable <- renderTable(bordered = TRUE, {

    weights = my_weights$df # get weights
    colnames(weights) <- "New Weight"
    #norm.weights = sapply(names(my_object), function(x) {my_weights[[x]]/Reduce(`+`, my_weights)})
    norm.weights <- my_weights$df/sum(my_weights$df)
    colnames(norm.weights) <- "New Normalized Weight"

    cbind(criteria, weights, norm.weights)

    #rename(Company = company.unduplicated, Sum = lump.sum.2, Count = n)
  })

  output$Plot <- renderPlotly({
    main.weights <- data.frame(my_weights$df)

    ahpGlobalWeight <- as.data.frame(as.matrix(ahpMatrix) %*% as.matrix(main.weights))
    row.names(ahpGlobalWeight) <- alternatives

    scoreMatrix <- as.data.frame(as.matrix(ahpMatrix[1,]) * t(as.matrix(main.weights)))
    score2<- rbind(scoreMatrix, as.data.frame(as.matrix(ahpMatrix[2,]) * t(as.matrix(main.weights))))
    score3 <- rbind(score2, as.data.frame(as.matrix(ahpMatrix[3,]) * t(as.matrix(main.weights))))

    row.names(score3) <- alternatives
    colnames(score3) <- criteria
    score3 <- score3 %>% mutate(id = alternatives)
    mydf.molten <- melt(score3, value.name="Count", variable.name="Variable", na.rm=TRUE)


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
