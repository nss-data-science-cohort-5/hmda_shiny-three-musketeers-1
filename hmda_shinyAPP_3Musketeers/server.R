library(shiny)
library(DT)

# Define server logic required to generate plots and tables
shinyServer(function(input, output) {
  
  # observeEvent(input$debug, {
  #   browser()
  # })
  
  # filter 1st LEI data based on the selections from the input
  
  # data_filtered <- function(lei, county){
  #   reactive({
  #     
  #     # filter1
  #     if ("All" %in% lei  ){
  #       data = hmda_lei_census
  #     }
  #     else {
  #       data = hmda_lei_census %>% 
  #         filter(`Entity Name` %in% lei)
  #     }
  #     
  #     # filter2
  #     if("All" %in% county  ){
  #       data = data
  #     }
  #     else{
  #       data = data %>% 
  #         filter(Name %in% county)
  #     }
  #   })
  #   
  # }
  # 
  # data_filtered1 <- data_filtered(input$lei1, input$county1)
  
  data_filtered1 <- reactive({

    # filter1 lei
    if ("All" %in% input$lei1  ){
      data = hmda_lei_census
    }
    else {
      data = hmda_lei_census %>%
        filter(`Entity Name` %in% input$lei1)
    }

    # filter2 county
    if("All" %in% input$county1  ){
      data = data
    }
    else{
      data = data %>%
        filter(Name %in% input$county1)
    }
    
    # filter3 loan_type
    if("All" %in% input$loan_type  ){
      data = data
    }
    else{
      data = data %>%
        filter(loan_type %in% input$loan_type)
    }
    
    
    # filter4 purchaser_type
    if("All" %in% input$purchaser_type  ){
      data = data
    }
    else{
      data = data %>%
        filter(purchaser_type %in% input$purchaser_type)
    }
    
    
    # filter5 derived_dwelling_category
    if("All" %in% input$derived_dwelling_category  ){
      data = data
    }
    else{
      data = data %>%
        filter(derived_dwelling_category %in% input$derived_dwelling_category)
    }
    
    # filter6 activity_year
    if("All" %in% input$activity_year  ){
      data = data
    }
    else{
      data = data %>%
        filter(activity_year %in% input$activity_year)
    }
    
  })
  
  # filter 2nd LEI data based on the selections from the input
  data_filtered2 <- reactive({
    
    # filter1 lei
    if ("All" %in% input$lei2  ){
      data = hmda_lei_census
    }
    else {
      data = hmda_lei_census %>%
        filter(`Entity Name` %in% input$lei2)
    }
    
    # filter2 county
    if("All" %in% input$county2  ){
      data = data
    }
    else{
      data = data %>%
        filter(Name %in% input$county2)
    }
    
    # filter3 loan_type
    if("All" %in% input$loan_type  ){
      data = data
    }
    else{
      data = data %>%
        filter(loan_type %in% input$loan_type)
    }
    
    
    # filter4 purchaser_type
    if("All" %in% input$purchaser_type  ){
      data = data
    }
    else{
      data = data %>%
        filter(purchaser_type %in% input$purchaser_type)
    }
    
    
    # filter5 derived_dwelling_category
    if("All" %in% input$derived_dwelling_category  ){
      data = data
    }
    else{
      data = data %>%
        filter(derived_dwelling_category %in% input$derived_dwelling_category)
    }
    
    # filter6 activity_year
    if("All" %in% input$activity_year  ){
      data = data
    }
    else{
      data = data %>%
        filter(activity_year %in% input$activity_year)
    }
    
  })
  
  # separator function for big mark and decimal mark of numbers
  separator <- function(x){
    format(as.numeric(x), big.mark = ",", decimal.mark = ".")
  }
  
  #1 Race and race census plots/tables
  # output$racePlot1 <- renderPlot({source("plots_tables/race.R", local = TRUE)})
  
  #1 Plot of loans by race
  output$racePlot1 <- renderPlot({
    data_filtered1()%>%
      count(derived_race = factor(derived_race)) %>%
      mutate(pct = prop.table(n)) %>%
      ggplot(aes(x = reorder(derived_race, pct), y = pct, fill = derived_race, label = scales::percent(pct))) +
      geom_col() +
      # geom_text(position = position_dodge(width = .9),    # move to center of bars
      #           vjust = -0.5,    # nudge above top of bar
      #           size = 5) +
      scale_y_continuous(labels = scales::percent)+
      labs(y = "Percent", x= "")+
      theme(text = element_text(size = 20), legend.position = "none")+
      coord_flip()
  })

  output$racePlot2 <- renderPlot({
    data_filtered2() %>%
      count(derived_race = factor(derived_race)) %>%
      mutate(pct = prop.table(n)) %>%
      ggplot(aes(x = reorder(derived_race, pct), y = pct, fill = derived_race, label = scales::percent(pct))) +
      geom_col() +
      # geom_text(position = position_dodge(width = .9),    # move to center of bars
      #           vjust = -0.5,    # nudge above top of bar
      #           size = 5) +
      scale_y_continuous(labels = scales::percent)+
      labs(y = "Percent", x= "")+
      theme(text = element_text(size = 20), legend.position = "none")+
      coord_flip()
  })

  output$raceCensusPlot1 <- renderPlot({

  })

  output$raceCensusPlot2 <- renderPlot({

  })

  #1 Tables of loans by race
  output$raceTable1<- renderDataTable(rownames = FALSE,
                                      options = list(columnDefs = list(list(className = 'dt-right', targets = 0:1))),
                                      {
                                        data_filtered1() %>%
                                          count(derived_race) %>%
                                          mutate(Percent = n/sum(n)*100)%>%
                                          data.frame() %>%
                                          mutate_at(vars(Percent), funs(round(.,2))) %>%
                                          mutate_at(vars(n), separator) %>%
                                          arrange(desc(Percent)) %>%
                                          rename(., `Derived Race` = derived_race, Loans = n)
                                      })

  output$raceTable2<- renderDataTable(rownames = FALSE,
                                      options = list(columnDefs = list(list(className = 'dt-right', targets = 0:1))),
                                      {
                                        data_filtered2() %>%
                                          count(derived_race) %>%
                                          mutate(Percent = n/sum(n)*100)%>%
                                          data.frame() %>%
                                          mutate_at(vars(Percent), funs(round(.,2))) %>%
                                          mutate_at(vars(n), separator) %>%
                                          arrange(desc(Percent)) %>%
                                          rename(., `Derived Race` = derived_race, Loans = n)
                                      })
  
  #2 Sex and sex census plots/tables
  
  
  #3 Age and age census plots/tables
  
  
  #4 Distribution of Loan Amounts plots/tables

  #5 Applicants' Credit Scores plots/tables

  #6 Denial Reasons of Loan Applications plots/tables

  #7 Loan Applications by Action Taken plots/tables
  
  #8 Loan Applications by County map/tables
  
  
  
  
})
