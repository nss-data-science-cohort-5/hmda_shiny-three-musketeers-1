#1 Plot of loans by race
output$racePlot1 <- renderPlot({
  data_filtered1() %>%
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