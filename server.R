function(input, output) {
 
  filtering <- reactive({
    startyear <- as.Date(input$date[1]) %>% as.character() %>% substr(1,4) %>% as.numeric()
    endyear <- input$date[2] %>% as.Date() %>% as.character() %>% substr(1,4) %>% as.numeric()
    years <- endyear - startyear + 1
    solar_europe_de_nuts %>%
      filter(Stadt == input$selected_country) %>%
      filter(utc_timestamp >= paste0(startyear, "-01-01 00:00:00"), utc_timestamp <= paste0(endyear, "-12-31 24:00:00"))
  })  
  
   m2 <- reactive({
       startyear <- as.Date(input$date[1]) %>% as.character() %>% substr(1,4) %>% as.numeric()
       endyear <- input$date[2] %>% as.Date() %>% as.character() %>% substr(1,4) %>% as.numeric()
       years <- endyear - startyear + 1
       solar_europe_de_nuts %>%
         filter(Stadt == input$selected_country) %>%
         filter(utc_timestamp >= paste0(startyear, "-01-01 00:00:00"), utc_timestamp <= paste0(endyear, "-12-31 24:00:00")) %>%
        summarise(
        yieldm2 = sum(solar_watt) / years * input$efficency /1000,
        m = input$kwhy/yieldm2
      )
  })
  
  output$m <- renderInfoBox({
    m2 <- m2()
    valueBox("Benötigte Fläche in m²", m2$m)
  })
  
  output$yieldm2 <- renderInfoBox({
      m2 <- m2()
      valueBox("Produziert kWh pro m²", m2$yieldm2)
  })
  
  output$radiation_chart <- renderPlot({
    data <- filtering()
    data %>%
      mutate(day = utc_timestamp %>% as.character() %>% substr(6,10)) %>%
      group_by(day) %>%
      summarize(avg = mean(solar_watt, na.rm = TRUE), std = sd(solar_watt, na.rm = TRUE) / sqrt(n())) %>%
      mutate(date = as.Date(paste0("2019-", day))) %>% 
      inner_join(slpc) %>%
      mutate(standardlast = standardlast / m2() %>% pull(m)) %>%
      ggplot() + 
      aes(x = date) +
      geom_ribbon(aes(ymin = avg - 1.96 * std, ymax = avg + 1.96 * std), fill = "grey70") +
      geom_line(aes(y = avg)) +
      geom_smooth(aes(y = standardlast, color = "red")) +
      xlab("") +
      ylab("") +
      scale_x_date(labels = date_format("%B"))
  })
  
}