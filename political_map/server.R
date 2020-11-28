shinyServer(function(input, output) {
   
# word frequency analysis ----------------------------------------------------------------------------------
    chamber_filter <- reactive({
       
        tmp <- party_congress %>% 
           filter(Chamber == input$chamber) %>% 
           mutate(Start = as.Date(Start),
                  End = as.Date(End)) 
        
        chamber_name <- names(chamber_names[chamber_names==input$chamber])
        
        # creates republican majority shading
        republican_shade <- tmp %>% 
            filter(Party == "Republican")
        
        # create democrat majority for shading
        democrat_shade <- tmp %>% 
            filter(Party == "Democrat")
       
        word_counts_party <- speech_df %>% 
            filter(chamber == input$chamber) %>% 
            group_by(month, party) %>% 
            summarise(ave_word = mean(word_count)) %>% 
            arrange(party, month) %>% 
            filter(party == "D" | party == "R") %>% 
            ungroup() %>% 
            mutate(month = as.Date(paste0(month,"-01")))
        
        word_counts_gender_timeseries <- speech_df %>% 
            filter(chamber == input$chamber) %>% 
            group_by(month, gender) %>% 
            summarise(ave_word = mean(word_count)) %>% 
            arrange(gender, month) %>% 
            ungroup() %>% 
            mutate(month = as.Date(paste0(month,"-01")))

       return(list(word_counts_party, republican_shade, democrat_shade, 
                   chamber_name, word_counts_gender_timeseries))
   }) 
   
   output$congress_plot <- renderPlot({

       chamber_filter()[[1]] %>%
           ggplot(aes(month, ave_word, color = party)) +
           geom_point() +
           geom_smooth(method = 'loess') +
           labs(y = "Average Word Count", x = "", 
                title = paste0("Average Montly Word Counts in ", chamber_filter()[[4]], " by Party"), 
                subtitle = "Jan. 2001 to Sept. 2016",
                caption = paste0("Shading represents party majority in ", chamber_filter()[[4]])) +
           scale_color_manual(values = c("blue", "red")) +
           theme_classic() +
           theme(text = element_text(size=16)) +
           scale_x_date(breaks = "3 years", date_labels = "%B %Y") +
           geom_rect(data=chamber_filter()[[2]], 
                     aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf), 
                     fill='red', alpha=0.1, inherit.aes = FALSE) +
           geom_rect(data=chamber_filter()[[3]], 
                     aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf), 
                     fill='blue', alpha=0.1, inherit.aes = FALSE )
       
   })
   
   congress_dates <- reactive({
       tmp <- congress_number_dates %>% 
           filter(congress_number == input$congress_number)
       return(tmp)
   })
   
   output$gender_plot_timeseries <- renderPlot({
       
       chamber_filter()[[5]] %>%
           ggplot(aes(month, ave_word, color = gender)) +
           geom_point() +
           geom_smooth(method = 'loess') +
           labs(y = "Average Word Count", x = "", 
                title = paste0("Average Montly Word Counts in ", chamber_filter()[[4]], " by Gender"), 
                subtitle = "Jan. 2001 to Sept. 2016") +
           scale_color_manual(values = c("blue", "red")) +
           theme_classic() +
           theme(text = element_text(size=16)) +
           scale_x_date(breaks = "3 years", date_labels = "%B %Y") +
           geom_rect(data=congress_dates(), 
                     aes(xmin=start, xmax=end, ymin=-Inf, ymax=+Inf), 
                     fill='grey', alpha=0.2, inherit.aes = FALSE )
   })
   
   output$gender_plot <- renderPlot({
       
       gender_percent %>% 
           filter(chamber == input$chamber) %>% 
           filter(congress_number == input$congress_number) %>% 
           ggplot(aes(x = gender, y = n, group = party))+
           geom_bar(aes(fill = party), stat = "identity", position = "dodge") +
           geom_label(aes(x = gender, y = n, label = n, group = party), position = position_dodge(width = 1)) +
           scale_fill_manual(values = c("blue", "red")) +
           labs(y = "", x = "",
                title = paste0("Number of Speakers in ", chamber_filter()[[4]], "\nby Gender and Party"),
                caption = "Counts number of speakers that speak in each chamber") +
           theme_bw()
   })
   
# sentiment analysis ------------------------------------------------------------------------------------------
   output$us_map <- renderLeaflet({
      
      states %>% 
      leaflet() %>% 
         addTiles() %>% 
         setView(lng = -98.32008, lat = 38.49878, zoom = 4) %>%
         addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                     layerId = ~STUSPS, 
                     label = ~lapply(paste0("<b>", NAME, "</b>",
                                            "<br/>",
                                            "Average Sentiment: ", round(ALAND, 2)), HTML), 
                     opacity = 1.0, fillOpacity = 0.5,
                     fillColor = ~colorQuantile(palette = "PRGn", n = 5, ALAND)(ALAND),
                     highlightOptions = highlightOptions(color = "white", weight = 2,
                                                         bringToFront = TRUE)) %>% 
         addLegend("bottomright", 
                   pal = pal, 
                   values = ~ALAND,
                   title = "Sentiment Percentiles",
                   opacity = 1)
   })
   
   # generate data in reactive
   ggplot_data <- reactive({

      site <- input$us_map_shape_click$id
      
      df <- sentiment_combine %>% 
         filter(state == site)
      
      return(df)
   })
   
   sentiment_boxplot <- reactiveVal(ggplot() + theme_classic())
   
   observeEvent(input$us_map_shape_click, {
      p <- ggplot_data() %>% 
         ggplot(aes(x = name, y = sentiment, fill = party)) +
         geom_boxplot() +
         geom_hline(yintercept = average_sentiment, color = "black", lty = "dashed") +
         coord_flip() +
         scale_fill_manual(values = c("D" = "blue", "I" = "purple", "R" = "red")) +
         labs(x = "", y = "Average Sentiment", 
              title = paste0("Distribution of Sentiment of Congressional Speech from ", input$us_map_shape_click)) +
         theme_classic()
      
      sentiment_boxplot(p)
   })
   
   output$plot <- renderPlot({
      sentiment_boxplot()
   }) 
   
   sentiment_president <- reactiveValues(p = NULL, txt = NULL)
   
   observeEvent(input$sentiment_click, {
   
      lvls <- sentiment_combine %>%
         filter(state == input$us_map_shape_click$id) %>% 
         pull(name) %>% unique() %>% factor() %>% levels()
      
      input_name <- lvls[round(input$sentiment_click$y)]
      
      p <- sentiment_combine %>%
         filter(name == input_name) %>%
         group_by(date, name) %>%
         summarise(daily_sentiment = sum(sentiment)) %>%
         ggplot(aes(x = date, y = daily_sentiment)) +
         geom_line(aes(group = 1), color = 'gray', linetype = 'dotted') +
         geom_point(aes(color = daily_sentiment >= 0)) +  # add conditional coloring 
         scale_color_manual(values = c("TRUE" = 'darkgreen', "FALSE" = 'darkorchid4')) +
         labs(x = "Total Daily Sentiment", y = "", title = paste0("Daily Sentiment for ", input_name)) +
         scale_x_date(breaks = "2 years", date_labels = "%B %Y") +
         theme_classic() +
         theme(legend.position = 'none')
      
      sentiment_president$p <- p

   })
   
   output$sentiment_timeseries <- renderPlotly({
      sentiment_president$p
   })
   
   textclassification <- eventReactive(input$run_text, {
      shiny_text <- tolower(input$speech_text)
      shiny_sequence <- texts_to_sequences(text_tokenizer, shiny_text)
      shiny_features <- pad_sequences(shiny_sequence, padding = "post", maxlen = 150) 
      text <- paste(unlist(text_tokenizer$index_word)[shiny_sequence[[1]]] , collapse = " ")
      return(list(shiny_text, shiny_sequence, shiny_features, text))
   })
   
   output$original_text <- renderText({
      textclassification()[[1]][[1]]
   })
   
   output$sequence <- renderText({
      textclassification()[[2]][[1]]
   })
   
   output$revised_text <- renderText({
      textclassification()[[4]]
   })
   
   output$encoded_text <- renderText({
      textclassification()[[3]]
   })
   
   fit_text_classification <- eventReactive(input$run_classification, {
      if(input$select_model == "new_model"){
         prediction <- predict_classes(object = new_model, x = textclassification()[[3]])
      } else if(input$select_model == "BERT_model") {
         prediction <- "this is a BERT model"
      } else {
         
      }
      
      return(prediction)
   })
   
   output$text_classification <- renderInfoBox({
      pred <- fit_text_classification()[[1]]
      if(pred == "1"){
         infoBox(
            "Classification:",
            "Democrat",
            color = "blue",
            icon = icon("democrat")
         )
      } else {
         infoBox(
            "Classification:",
            "Republican",
            color = "red",
            icon = icon("republican")
         )
      }
   })
   
   
})
