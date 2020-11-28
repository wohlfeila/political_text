shinyUI(fluidPage(
    navbarPage("Congressional Text Analysis",
               tabPanel("Word Frequency",
                        fluidRow(
                          column(3,
                                 selectInput("chamber",
                                             "Select Chamber: ",
                                              chamber_names
                                             ),
                                 selectInput("congress_number",
                                             "Select Congress: ",
                                              congress_number
                                             ),
                                 br(),
                                 br(),
                                 helpText("Help text: \n
                                          this is to explain selectors")
                        ), 
                        column(9, 
                               plotOutput("congress_plot")
                               )
                        ), 
                        
                        fluidRow(
                          column(3,
                                 plotOutput("gender_plot")
                                 ),
                          column(9, 
                                 plotOutput("gender_plot_timeseries")
                                 )
                          )
                        ),
               tabPanel("Sentiment",
                        fluidRow(
                          column(7,
                                 leafletOutput("us_map", height = "600px")
                                 ),
                          column(5,
                                 plotOutput("plot", height = "600px", click = "sentiment_click")
                                 )
                          ),
                        br(),
                        br(),
                        fluidRow(
                          column(12,
                                 plotlyOutput("sentiment_timeseries")
                          )
                        )
                        ),
               tabPanel("Text Classification",
                        fluidRow(
                          column(8,
                                 textInput("speech_text", "Enter Text for Classifiction:",
                                           value = ""),
                                 br(),
                                 actionButton("run_text", "Click to Update")
                          ),
                          column(4,
                                 radioButtons("select_model",
                                              "Select Classifier:",
                                              choices = c("GloVe" = "new_model", "BERT" = "BERT_model")),
                                 br(),
                                 actionButton("run_classification", "Classify Text")
                          )
                        ),
                        fluidRow(
                          column(8,
                                 h3("Original Text"),
                                 textOutput("original_text"),
                                 br(),
                                 h3("Sequences from Tokenizer"),
                                 textOutput("sequence"),
                                 br(),
                                 h3("Revised Text"),
                                 textOutput("revised_text"),
                                 br(),
                                 h3("Encoded Text"),
                                 textOutput("encoded_text")
                          ),
                          column(4,
                                 br(),
                                 infoBoxOutput("text_classification")
                          )
                        ),
                        )
               )
    )
)