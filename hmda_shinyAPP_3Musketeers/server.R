# Define server logic required to generate plots and tables
shinyServer(function(input, output) {
  
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
  
  map_data_filtered1 <- reactive({
    
    # filter1 lei
    if ("All" %in% input$lei1  ){
      data = hmda_lei_census
    }
    else {
      data = hmda_lei_census %>%
        filter(`Entity Name` %in% input$lei1)
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
  map_data_filtered2 <- reactive({
    
    # filter1 lei
    if ("All" %in% input$lei2  ){
      data = hmda_lei_census
    }
    else {
      data = hmda_lei_census %>%
        filter(`Entity Name` %in% input$lei2)
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
      labs(y = "Percentage", x= "")+
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
      labs(y = "Percentage", x= "")+
      theme(text = element_text(size = 20), legend.position = "none")+
      coord_flip()
  })
  
  output$raceCensusPlot1 <- renderPlot({

    df <- data_filtered1() %>%
      select(Name, White, `African-American`, `Native American`, Asian, `Multi-Racial`, Latino) %>%
      group_by(Name) %>%
      summarise(
        White = mean(White),
        `African-American` = mean(`African-American`),
        `Native American`= mean(`Native American`),
        Asian = mean(Asian),
        `Multi-Racial`= mean(`Multi-Racial`),
        Latino = mean(Latino)) %>%
      drop_na() %>%
      t() %>% 
      data.frame() %>% 
      slice_tail(n=6)
    
    race <- rownames(df)
    df <- data.frame(sapply(df, as.numeric))
    df %>% 
      mutate(total = rowSums(across(where(is.numeric)))) %>% 
      mutate(pct = total/sum(total)) %>%
      mutate(race = race) %>%
      ggplot(aes(x = reorder(race, pct), y = pct, fill = race, label = scales::percent(pct))) +
      geom_col() +
      scale_y_continuous(labels = scales::percent)+
      labs(y = "Percentage", x= "")+
      theme(text = element_text(size = 20), legend.position = "none")+
      coord_flip()
  })
  
  output$raceCensusPlot2 <- renderPlot({
    df <- data_filtered2() %>%
      select(Name, White, `African-American`, `Native American`, Asian, `Multi-Racial`, Latino) %>%
      group_by(Name) %>%
      summarise(
        White = mean(White),
        `African-American` = mean(`African-American`),
        `Native American`= mean(`Native American`),
        Asian = mean(Asian),
        `Multi-Racial`= mean(`Multi-Racial`),
        Latino = mean(Latino)) %>%
      drop_na() %>%
      t() %>% 
      data.frame() %>% 
      slice_tail(n=6)
    
    race <- rownames(df)
    df <- data.frame(sapply(df, as.numeric))
    df %>% 
      mutate(total = rowSums(across(where(is.numeric)))) %>% 
      mutate(pct = total/sum(total)) %>%
      mutate(race = race) %>%
      ggplot(aes(x = reorder(race, pct), y = pct, fill = race, label = scales::percent(pct))) +
      geom_col() +
      scale_y_continuous(labels = scales::percent)+
      labs(y = "Percentage", x= "")+
      theme(text = element_text(size = 20), legend.position = "none")+
      coord_flip()
  })
 
  #1 Tables of loans by race
  output$raceTable1<- renderDataTable(rownames = FALSE,
                                      options = list(dom = 't',
                                                     columnDefs = list(list(className = 'dt-right', 
                                                                            targets = 0:1))),
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
                                      options = list(dom = 't',
                                                     columnDefs = list(list(className = 'dt-right', 
                                                                            targets = 0:1))),
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
  
  output$raceCensusTable1<- renderDataTable(rownames = FALSE,
                                            options = list(dom = 't',
                                                           columnDefs = list(list(className = 'dt-right', 
                                                                                  targets = 0:1))),
                                            {
                                              df <- data_filtered1() %>%
                                                select(Name, White, `African-American`, `Native American`, Asian, `Multi-Racial`, Latino) %>%
                                                group_by(Name) %>%
                                                summarise(
                                                  White = mean(White),
                                                  `African-American` = mean(`African-American`),
                                                  `Native American`= mean(`Native American`),
                                                  Asian = mean(Asian),
                                                  `Multi-Racial`= mean(`Multi-Racial`),
                                                  Latino = mean(Latino)) %>%
                                                drop_na() %>%
                                                t() %>% 
                                                data.frame() %>% 
                                                slice_tail(n=6)
                                              
                                              race <- rownames(df)
                                              df <- data.frame(sapply(df, as.numeric))
                                              
                                              df %>%
                                                mutate(total = rowSums(across(where(is.numeric)))) %>%
                                                mutate(pct = total/sum(total)*100) %>%
                                                mutate(race = race) %>%
                                                mutate_at(vars(pct), funs(round(.,2))) %>%
                                                mutate_at(vars(total), separator) %>%
                                                arrange(desc(pct)) %>%
                                                rename(., Population = total, Percent = pct, Race = race) %>% 
                                                select(Race, Population, Percent)
                                            })
  
  output$raceCensusTable2<- renderDataTable(rownames = FALSE,
                                            options = list(dom = 't',
                                                           columnDefs = list(list(className = 'dt-right', 
                                                                                  targets = 0:1))),
                                            {
                                              df <- data_filtered2() %>%
                                                select(Name, White, `African-American`, `Native American`, Asian, `Multi-Racial`, Latino) %>%
                                                group_by(Name) %>%
                                                summarise(
                                                  White = mean(White),
                                                  `African-American` = mean(`African-American`),
                                                  `Native American`= mean(`Native American`),
                                                  Asian = mean(Asian),
                                                  `Multi-Racial`= mean(`Multi-Racial`),
                                                  Latino = mean(Latino)) %>%
                                                drop_na() %>%
                                                t() %>% 
                                                data.frame() %>% 
                                                slice_tail(n=6)
                                              
                                              race <- rownames(df)
                                              df <- data.frame(sapply(df, as.numeric))
                                              
                                              df %>%
                                                mutate(total = rowSums(across(where(is.numeric)))) %>%
                                                mutate(pct = total/sum(total)*100) %>%
                                                mutate(race = race) %>%
                                                mutate_at(vars(pct), funs(round(.,2))) %>%
                                                mutate_at(vars(total), separator) %>%
                                                arrange(desc(pct)) %>%
                                                rename(., Population = total, Percent = pct, Race = race) %>% 
                                                select(Race, Population, Percent)
                                            })
  
  #2 Sex and sex census plots/tables
  
 
  #3 Age plots/tables
  
  output$agePlot1_1 <- renderPlot({
    filter_age(data_filtered1()) %>% 
      fill(Value, 0) %>% 
      filter(grepl("Percentage", Category)) %>% 
      ggplot(aes(x = Group, 
                 y = Value, 
                 fill = Category, 
                 label = percent(Value))) +
      geom_col(position = position_dodge(width = 1),
               color = "black") +
      geom_text(position = position_dodge(width = 1),
                vjust = -0.5,
                size = 3) +
      scale_y_continuous(labels = percent) +
      labs(x = "Applicant Age Group",
           y = "Percentage of Applicants",
           title = "Applicant Age Group Percentage")
  })
  output$agePlot1_2 <- renderPlot({
    filter_age(data_filtered1()) %>% 
      fill(Value, 0) %>% 
      filter(grepl("Total", Category)) %>% 
      ggplot(aes(x = Group, 
                 y = Value, 
                 fill = Category)) +
      geom_col(position = position_dodge(width = 1),
               color = "black") +
      scale_y_continuous(labels = comma) +
      labs(x = "Applicant Age Group",
           y = "Number of Applicants",
           title = "Applicant Age Group Totals")
  })
  
  
  output$agePlot2_1 <- renderPlot({
    filter_age(data_filtered2()) %>% 
      fill(Value, 0) %>% 
      filter(grepl("Percentage", Category)) %>% 
      ggplot(aes(x = Group, 
                 y = Value, 
                 fill = Category, 
                 label = percent(Value))) +
      geom_col(position = position_dodge(width = 1),
               color = "black") +
      geom_text(position = position_dodge(width = 1),
                vjust = -0.5,
                size = 3) +
      scale_y_continuous(labels = percent) +
      labs(x = "Applicant Age Group",
           y = "Percentage of Applicants",
           title = "Applicant Age Group Percentage")
  })

  output$agePlot2_2 <- renderPlot({
    filter_age(data_filtered2()) %>% 
      fill(Value, 0) %>% 
      filter(grepl("Total", Category)) %>% 
      ggplot(aes(x = Group, 
                 y = Value, 
                 fill = Category)) +
      geom_col(position = position_dodge(width = 1),
               color = "black") +
      scale_y_continuous(labels = comma) +
      labs(x = "Applicant Age Group",
           y = "Number of Applicants",
           title = "Applicant Age Group Totals")
  })
  
  
  output$ageTable1 <- renderDataTable(
    rownames = FALSE,
    options = list(dom = 't',
                   columnDefs = list(list(className = 'dt-right', 
                                          targets = 0:1))),
    { 
      filter_age(data_filtered1()) %>%
        fill(0) %>% 
        pivot_wider(names_from = "Category", values_from = "Value") %>% 
        mutate("Applicant Percentage" = sapply(.[["Applicant Percentage"]],
                                               label_percent()),
               "Area Percentage" = sapply(.[["Area Percentage"]],
                                          label_percent())) %>%
        rename("Applicant Age Group" = "Group") %>% 
        mutate("Area Total" = prettyNum(.[["Area Total"]], big.mark = ","),
               "Applicant Total" = prettyNum(.[["Applicant Total"]], big.mark = ",")) %>% 
        relocate("Applicant Total", .before = "Applicant Percentage") %>% 
        relocate("Area Total", .before = "Area Percentage")
    })
  
  output$ageTable2 <- renderDataTable(
    rownames = FALSE,
    options = list(dom = 't',
                   columnDefs = list(list(className = 'dt-right', 
                                          targets = 0:1))),
    { 
      filter_age(data_filtered2()) %>%
        fill(0) %>% 
        pivot_wider(names_from = "Category", values_from = "Value") %>% 
        mutate("Applicant Percentage" = sapply(.[["Applicant Percentage"]],
                                               label_percent()),
               "Area Percentage" = sapply(.[["Area Percentage"]],
                                          label_percent())) %>%
        rename("Applicant Age Group" = "Group") %>% 
        mutate("Area Total" = prettyNum(.[["Area Total"]], big.mark = ","),
               "Applicant Total" = prettyNum(.[["Applicant Total"]], big.mark = ",")) %>% 
        relocate("Applicant Total", .before = "Applicant Percentage") %>% 
        relocate("Area Total", .before = "Area Percentage")
    })
  
  #4 Distribution of Loan Amounts plots/tables
  
 

  #6 Denial Reasons of Loan Applications plots/tables
  output$denialPlot1 <- renderPlot({
    data_filtered1()%>%
      filter(action_taken == "Application denied") %>%
      select(`denial_reason-1`) %>% 
      drop_na() %>%
      count(`denial_reason-1` = factor(`denial_reason-1`)) %>%
      mutate(pct = prop.table(n)) %>%
      ggplot(aes(x = reorder(`denial_reason-1`, pct), y = pct, fill = `denial_reason-1`, label = scales::percent(pct))) +
      geom_col() +
      # geom_text(position = position_dodge(width = .9),    # move to center of bars
      #           vjust = -0.5,    # nudge above top of bar
      #           size = 5) +
      scale_y_continuous(labels = scales::percent)+
      labs(y = "Percent", x= "")+
      theme(text = element_text(size = 20), legend.position = "none")+
      coord_flip()
  })
 
  output$denialPlot2 <- renderPlot({
    data_filtered2()%>%
      filter(action_taken == "Application denied") %>%
      select(`denial_reason-1`) %>% 
      drop_na() %>%
      count(`denial_reason-1` = factor(`denial_reason-1`)) %>%
      mutate(pct = prop.table(n)) %>%
      ggplot(aes(x = reorder(`denial_reason-1`, pct), y = pct, fill = `denial_reason-1`, label = scales::percent(pct))) +
      geom_col() +
      # geom_text(position = position_dodge(width = .9),    # move to center of bars
      #           vjust = -0.5,    # nudge above top of bar
      #           size = 5) +
      scale_y_continuous(labels = scales::percent)+
      labs(y = "Percent", x= "")+
      theme(text = element_text(size = 20), legend.position = "none")+
      coord_flip()
  })
  
  output$denialTable1<- renderDataTable(rownames = FALSE,
                                        options = list(dom = 't',
                                                       columnDefs = list(list(className = 'dt-right', 
                                                                              targets = 0:1))),
                                        {
                                          data_filtered1() %>%
                                            filter(action_taken == "Application denied") %>%
                                            select(`denial_reason-1`) %>% 
                                            drop_na() %>%
                                            count(`denial_reason-1`) %>%
                                            mutate(Percent = n/sum(n)*100)%>%
                                            data.frame() %>%
                                            mutate_at(vars(Percent), funs(round(.,2))) %>%
                                            mutate_at(vars(n), separator) %>%
                                            arrange(desc(Percent)) %>%
                                            rename(., `Denial Reasons` = `denial_reason.1`, Loans = n)
                                        })
    
    output$denialTable2<- renderDataTable(rownames = FALSE,
                                        options = list(dom = 't',
                                                       columnDefs = list(list(className = 'dt-right', 
                                                                              targets = 0:1))),
                                        {
                                          data_filtered2() %>%
                                            filter(action_taken == "Application denied") %>%
                                            select(`denial_reason-1`) %>% 
                                            drop_na() %>%
                                            count(`denial_reason-1`) %>%
                                            mutate(Percent = n/sum(n)*100)%>%
                                            data.frame() %>%
                                            mutate_at(vars(Percent), funs(round(.,2))) %>%
                                            mutate_at(vars(n), separator) %>%
                                            arrange(desc(Percent)) %>%
                                            rename(., `Denial Reasons` = `denial_reason.1`, Loans = n)
                                        })

    
  #7 Loan Applications by Action Taken plots/tables
    
  #8 Loan Applications by County map/tables
  output$mapPlot1 <- renderPlot({
    filter_map(map_data_filtered1()) %>% 
      fill(Aggregate_Number, 0) %>% 
      ggplot() + 
      geom_sf(aes(fill = Aggregate_Number),
              lwd = 0) +
      geom_text(aes(x = lat,
                    y = long,
                    label = scales::comma(Aggregate_Number)),
                size = 4) + 
      labs(title = "Applicant Number Per 1,000 by County") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5,
                                      size = 20),
            axis.line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "None") + 
      scale_fill_gradient(low = "#10bee8", 
                          high = "#A0522D",
                          trans = "log2")
  })
  
  output$mapPlot2 <- renderPlot({
    filter_map(map_data_filtered2()) %>% 
      fill(Aggregate_Number, 0) %>% 
      ggplot() + 
      geom_sf(aes(fill = Aggregate_Number),
              lwd = 0) +
      geom_text(aes(x = lat,
                    y = long,
                    label = scales::comma(Aggregate_Number)),
                size = 4) + 
      labs(title = "Applicant Number Per 1,000 by County") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5,
                                      size = 20),
            axis.line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "None") + 
      scale_fill_gradient(low = "#10bee8", 
                          high = "#A0522D",
                          trans = "log2")
  })
  
  output$mapPctPlot1 <- renderPlot({
    filter_map(map_data_filtered2()) %>% 
      fill(Pct_Aggregate_Number, 0) %>% 
      ggplot() + 
      geom_sf(aes(fill = Pct_Aggregate_Number),
              lwd = 0) +
      geom_text(aes(x = lat,
                    y = long,
                    label = scales::percent(Pct_Aggregate_Number)),
                size = 4) + 
      labs(title = "Applicant Percentage by County") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5,
                                      size = 20),
            axis.line=element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "None") + 
      scale_fill_gradient(low = "#10bee8", 
                          high = "#A0522D")
  })
  
  output$mapPctPlot2 <- renderPlot({
    filter_map(map_data_filtered2()) %>% 
      fill(Pct_Aggregate_Number, 0) %>%
      ggplot() + 
      geom_sf(aes(fill = Pct_Aggregate_Number),
              lwd = 0) +
      geom_text(aes(x = lat,
                    y = long,
                    label = scales::percent(Pct_Aggregate_Number)),
                size = 4) + 
      labs(title = "Applicant Percentage by County") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5,
                                      size = 20),
            axis.line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "None") + 
      scale_fill_gradient(low = "#10bee8", 
                          high = "#A0522D")
  })

  
  
  

  

  
  
})
