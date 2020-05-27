###SHINY

library(shiny)
library(ggplot2)

bigdataset <- read.csv( file="shinyprojectdata.csv", header = TRUE, sep=",")


#make datasets for anova
sczanovadata <- subset(bigdataset, bigdataset$Group == "Schizophrenia")  
sibanovadata <- subset(bigdataset, bigdataset$Group == "Sibling")    
controlanovadata <- subset(bigdataset, bigdataset$Group == "Control")  


ui <- fluidPage(
  
  headerPanel("Month and Season of Birth Effects by Subgroup on Several Variables"),
  
  sidebarPanel(
    textInput("caption", "Caption:", "Results"),
    selectInput("dataset", "Choose a Dataset:",
                choices = c("Schizophrenia","Sibling","Control", "All")),
    selectInput("variable", "Choose a Variable:",
                choices= c("Cognition (Big_g)", "Genetic Risk (RPS p=.01)", "SES", "Symptoms (PANSS total)")),
    radioButtons("time", "By Season or Month?",
                 choices= c("Season", "Month"))
    
  ),
  
  mainPanel(
    h2(textOutput("caption")),
    h3(textOutput("groupname")),
    tabsetPanel(
    tabPanel("ANOVA", verbatimTextOutput("anova")),
    tabPanel("Box/Bar plot", plotOutput("barboxplot")),
    tabPanel("Jitter Plot",plotOutput("jitterboxplot")),
    tabPanel("Data Summary", verbatimTextOutput("datainfo"))
    )
  )
)

server <- function(input, output) {
  
  datasetInput <- reactive({
    
    switch(input$dataset,
           "Schizophrenia" = sczanovadata,
           "Sibling" = sibanovadata,
           "Control" = controlanovadata,
           "All" = bigdataset)
    
  })
  
  variableInput <- reactive({
    switch(input$variable,
           "Cognition (Big_g)" = datasetInput()$Big_g,
           "Genetic Risk (RPS p=.01)" = datasetInput()$zMG_0.01_ASPop_112216,
           "SES" = datasetInput()$FamilySES,
           "Symptoms (PANSS total)" = datasetInput()$PANSS_TotalScore)
  })
  
  timeInput <- reactive ({
    switch(input$time,
           "Season" = datasetInput()$SeasonOB,
           "Month" = datasetInput()$MonthOB)
  })
  
  
  output$groupname <- renderText({
    input$dataset
    
  })
  
  output$datainfo <-renderPrint({
    summary(datasetInput())
    
  })
  
  
  output$caption <- renderText({
    input$caption
  })  
  
  output$anova <- renderPrint({
    anova <- lm(variableInput()~timeInput(), data=datasetInput())
    summary(anova)
  })
  
  output$jitterboxplot <- renderPlot({
    
    if (input$dataset == "All") {
      
      jitterboxplot <- ggplot(datasetInput(), aes(x=factor(timeInput()), y=variableInput(), colour=Group)) + geom_jitter(stat='identity', position = 'dodge') + xlab(input$time) + ylab(input$variable)
    }
    
    else {
      
      jitterboxplot <- ggplot(datasetInput(), aes(x=factor(timeInput()), y=variableInput())) + geom_jitter() + xlab(input$time) + ylab(input$variable)
    }
    
    print(jitterboxplot)
    
  })
  
  output$barboxplot <- renderPlot({
    
    if (input$dataset == "All") {
      
      barboxplot <- ggplot(datasetInput(), aes(x=factor(timeInput()), y=variableInput(), fill=Group)) + geom_bar(stat='identity', position = 'dodge') + xlab(input$time) + ylab(input$variable)
    }
    
    else {
      
      barboxplot <- ggplot(datasetInput(), aes(x=factor(timeInput()), y=variableInput())) + geom_boxplot() + xlab(input$time) + ylab(input$variable)
    }
    
    print(barboxplot)
    
  })
}

shinyApp(ui=ui, server=server)









