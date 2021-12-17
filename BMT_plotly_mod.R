library(shiny)
library(shinyjs)
library(DT)
library(readxl)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinyalert)

values<-read_excel("online table v2.xlsx")

ref<-read_excel("otr v2.xlsx")

#ref<-read.csv(file="otr.csv")
# Define UI for slider demo app ----
ui <- fluidPage(
  useShinyalert(),
  # setBackgroundColor("AliceBlue"),
  #shinythemes::themeSelector(),
  
  theme = shinythemes::shinytheme("lumen"),  
  
  titlePanel("Artifact Removal Benchmarking Online Tool"),
  
  sidebarPanel(width=3,
    fluidRow(
      column(width=12,numericInput("num_n", "Training Examples (e.g., 500):", 10,min =1,max = 1000000)),
      column(width=12, tags$em("Tip: Number of examples used to train the algorithm.")),
      column(width=12,numericInput("num_f", "Features Extracted (e.g., 5):", 0,min = 0)),
      column(width=12,tags$em("Tip: Number of hand-crafted features extracted from the signal.")),
      column(width=12,numericInput("num_p", "Hyperparameters (e.g., 4):", 0,min = 0)),
      column(width=12,tags$em("Tip: Number of hyperparameters the algorithm has.")),
      radioButtons("radio", label = h5("Performance Metic"),
                   choices = list("Accuracy, AUROC, F1-score,  sensitivity,  specificity,  expert  agreement, R^2 and values that tend to 1 (range from 0 to 1)" = 1, 
                                  "MSE, RMSE, artifact residue and values that tend to 0 (usual range from 20 to 0)" = 2,
                                  "SNR, contrast-to-noise and values that tend to inf. (usual range from 1 to 20 DB)" = 3), 
                   selected = 1),
      
      column(width=12,numericInput("num_s", "Performance Score (e.g., 0.8):", 0,min = 0)),
      column(width=12,tags$em("Tip: Achieved performance."))
    ),
    hr(style = "border-top: 1px solid #000000;"),
    fluidRow(
      actionButton("send", HTML("Send Suggestions:<br/> DOI/Reference/Comments"), icon("paper-plane"), 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                   onclick ="window.open('https://forms.gle/ShK7L8tMJFQRrqQ38', '_blank')")
      , align = "center"
      , style = "margin-bottom: 10px;"
      , style = "margin-top: -10px;"
    )
  ),
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Comparison Plots",
                         fluidRow(
                           column(width=6,
                                  h5("Examples",align="center"),
                                  plotlyOutput("plot_ex")
                           ),
                           column(width=6,
                                  h5("Features",align="center"),
                                  plotlyOutput("plot_fe")
                           ),                          
                           
                           column(width=6,
                                  h5("Hyperparameters",align="center"),
                                  plotlyOutput("plot_hp")
                           ),                          
                           
                           column(width=6,
                                  h5("Normalised Performance",align="center"),
                                  plotlyOutput("plot_sc",)
                           )),  
                         
                         fluidRow(
                           column(width=6,
                                  selectInput("Signal", "Filter Signal Type",
                                              choices = c('All',unique(values[,3])), selected='All',multiple = TRUE)),
                           column(width=6,
                                  selectInput("Method", "Filter Method Type",
                                              choices = c('All',unique(values[,4])),selected='All', multiple = TRUE)),
                           column(width=6,
                                  selectInput("Artifact", "Filter Artifact Type",
                                              choices = c('All',unique(values[,5])),selected='All', multiple = TRUE)),
                           column(width=6),
                           column(width=6,
                                  numericRangeInput("Year", "Filter by Year",
                                                    value=c(min(values[,2], na.rm = TRUE),max(values[,2], na.rm = TRUE)))),
                           
                           column(width=6, 
                                  numericRangeInput("Examples", "Filter Training Examples",
                                                    value=c(min(values[,8], na.rm = TRUE), max(values[,8], na.rm = TRUE)))),
                           
                           column(width=6,
                                  sliderInput("Features", "Filter Features Extracted",
                                              min(values[,9], na.rm = TRUE), max(values[,9], na.rm = TRUE), c(min(values[,9], na.rm = TRUE),max(values[,9], na.rm = TRUE)) , step = 1)),
                           column(width=6,
                                  sliderInput("Hyperparameters", "Filter Hyperparameter",
                                              min(values[,10], na.rm = TRUE), max(values[,10], na.rm = TRUE), c(min(values[,10], na.rm = TRUE),max(values[,10], na.rm = TRUE)) , step = 1)),
                           column(width=6,
                                  sliderInput("Performance", "Filter Performance",
                                              min(values[,11], na.rm = TRUE), max(values[,11], na.rm = TRUE), c(min(values[,11], na.rm = TRUE),max(values[,11], na.rm = TRUE)) , step = 0.001))
                         )
                ),
                tabPanel("Comparison Table", 
                         fluidRow(
                           column(12,div(DT::dataTableOutput("table1"))
                           ))
                ),
                tabPanel("References Table", 
                         fluidRow(
                           column(12,div(DT::dataTableOutput("table2"))
                           ))
                )
    ),
    fluidRow(
      p(code("Developed by Marcos Fabietti")),
      p("Email: n0892706@my.ntu.ac.uk")
    ))
)


# Define server logic for slider examples ----
shinyserver <- 
  function(input, output) {
    #show intro modal
    observeEvent("", {
      showModal(modalDialog(
        includeHTML("intro_text.html"),
        easyClose = TRUE,
        footer = tagList(
          actionButton(inputId = "intro", label = "close", icon = icon("info-circle"))
        )
      ))
    })
    
    observeEvent(input$intro,{
      removeModal()
    })
    
    output$table1<-DT::renderDataTable(
      values,
      class="display nowrap compact",
      filter="top",
    )
    
    output$table2<-DT::renderDataTable(
      ref,
      class="display nowrap compact",
      filter="top",
    )
    
    #filter the data reactively according to inputs
    plottingdata<- reactive({  
      res<-values %>% filter( Year>=input$Year[1] & Year <= input$Year[2])
      res<-res %>% filter(`Training Examples` >= input$Examples[1] & `Training Examples` <= input$Examples[2])
      res<-res %>% filter(Features >= input$Features[1] & Features <= input$Features[2])
      res<-res %>% filter(Hyperparameters >= input$Hyperparameters[1] & Hyperparameters <= input$Hyperparameters[2])
      res<-res %>% filter(`Normalised Performance` >= input$Performance[1] & `Normalised Performance` <= input$Performance[2])
      
      if("All" %in% input$Artifact) {
        res<-res
      }
      else{
        res<-res %>% filter(Artifact %in% input$Artifact)
      }
      
      if("All" %in% input$Signal) {
        res<-res
      }
      else{
        res<-res %>% filter(Signal %in% input$Signal)
      }
      if("All" %in% input$Method) {
        res<-res
      }
      else{
        res<-res %>% filter(Method %in% input$Method)
      }
      
      if(nrow(res)<2){
        shinyalert("Oops!", "Less than two examples selected", type = "error")}
      res<-res
    })
    
    #generate the four plots
    output$plot_ex <- renderPlotly({
      
      req(nrow(plottingdata()) > 1)
      if (is.na(input$num_n)){num_n<-0}else{num_n<-input$num_n}
      gg<-qplot(y=plottingdata()$`Training Examples`, x="",
                     na.rm = TRUE, fill= I("orangered"), ylab="",
                     geom=c("violin"))+
                 scale_y_log10()+
                 geom_jitter(na.rm = TRUE,width=0.4, height=0,colour = 'orangered4', alpha = 0.5,cex =5,)+
                 geom_point(aes(x="",y=num_n), fill='white', shape=24, size=7)
      gg <- plotly_build(gg)
      gg$x$data[[1]]$hoverinfo <- "none"
      gg$x$data[[2]]$text <- paste( "Reference:",plottingdata()$Reference, "<br>",
                                    "Year:",plottingdata()$Year, "<br>",
                                    "Signal:",plottingdata()$Signal, "<br>",
                                    "Method:",plottingdata()$Method, "<br>",
                                    "Artifact:",plottingdata()$Artifact, "<br>",
                                    "Metric:",plottingdata()$Metric, "<br>",
                                    "Result:",plottingdata()$Result, "<br>",
                                    "Training Examples:",plottingdata()$`Training Examples`, "<br>",
                                    "Features:",plottingdata()$Features, "<br>",
                                    "Hyperparameters:",plottingdata()$Hyperparameters, "<br>",
                                    "Normalised Performance:",plottingdata()$`Normalised Performance`, "<br>")
      gg
    })
    
    output$plot_fe <- renderPlotly({
      req(nrow(plottingdata()) > 1)
      if (is.na(input$num_f)){num_f<-0}else{num_f<-input$num_f}
      gg<-qplot(y=plottingdata()$Features, x="",
                na.rm = TRUE, fill= I("palegreen"), ylab="",
                geom=c("violin"))+
        geom_jitter(na.rm = TRUE,width=0.4, height=0,colour = 'palegreen4', alpha = 0.5,cex =5,)+
        geom_point(aes(x="",y=num_f), fill='white', shape=24, size=7)
      gg <- plotly_build(gg)
      gg$x$data[[1]]$hoverinfo <- "none"
      gg$x$data[[2]]$text <- paste( "Reference:",plottingdata()$Reference, "<br>",
                                    "Year:",plottingdata()$Year, "<br>",
                                    "Signal:",plottingdata()$Signal, "<br>",
                                    "Method:",plottingdata()$Method, "<br>",
                                    "Artifact:",plottingdata()$Artifact, "<br>",
                                    "Metric:",plottingdata()$Metric, "<br>",
                                    "Result:",plottingdata()$Result, "<br>",
                                    "Training Examples:",plottingdata()$`Training Examples`, "<br>",
                                    "Features:",plottingdata()$Features, "<br>",
                                    "Hyperparameters:",plottingdata()$Hyperparameters, "<br>",
                                    "Normalised Performance:",plottingdata()$`Normalised Performance`, "<br>")
      gg
    })
    
    output$plot_hp <- renderPlotly({
      req(nrow(plottingdata()) > 1)
      if (is.na(input$num_p)){num_p<-0}else{num_p<-input$num_p}
      gg<-qplot(y=plottingdata()$Hyperparameters, x="",
                na.rm = TRUE, fill= I("royalblue"), ylab="",
                geom=c("violin"))+
        geom_jitter(na.rm = TRUE,width=0.4, height=0,colour = 'royalblue4', alpha = 0.5,cex =5,)+
        geom_point(aes(x="",y=num_p), fill='white', shape=24, size=7)
      gg <- plotly_build(gg)
      gg$x$data[[1]]$hoverinfo <- "none"
      gg$x$data[[2]]$text <- paste( "Reference:",plottingdata()$Reference, "<br>",
                                    "Year:",plottingdata()$Year, "<br>",
                                    "Signal:",plottingdata()$Signal, "<br>",
                                    "Method:",plottingdata()$Method, "<br>",
                                    "Artifact:",plottingdata()$Artifact, "<br>",
                                    "Metric:",plottingdata()$Metric, "<br>",
                                    "Result:",plottingdata()$Result, "<br>",
                                    "Training Examples:",plottingdata()$`Training Examples`, "<br>",
                                    "Features:",plottingdata()$Features, "<br>",
                                    "Hyperparameters:",plottingdata()$Hyperparameters, "<br>",
                                    "Normalised Performance:",plottingdata()$`Normalised Performance`, "<br>")
      gg
    })
    
    output$plot_sc<- renderPlotly({
      if (is.na(input$num_s)){num_s<-0}else{num_s<-input$num_s}
      if(input$radio==1)
      { s<-num_s}
      if(input$radio==2)
      {s<-1/(1+num_s) }
      if(input$radio==3)
      {s<-1*(1-1/(1+num_s))}
      req(nrow(plottingdata()) > 1)
      gg<-qplot(y=plottingdata()$`Normalised Performance`, x="",
                na.rm = TRUE, fill= I("goldenrod"), ylab="",
                geom=c("violin"))+
        geom_jitter(na.rm = TRUE,width=0.4, height=0,colour = 'goldenrod4', alpha = 0.5,cex =5,)+
        geom_point(aes(x="",y=s), fill='white', shape=24, size=7)
      gg <- plotly_build(gg)
      gg$x$data[[1]]$hoverinfo <- "none"
      gg$x$data[[2]]$text <- paste( "Reference:",plottingdata()$Reference, "<br>",
                                    "Year:",plottingdata()$Year, "<br>",
                                    "Signal:",plottingdata()$Signal, "<br>",
                                    "Method:",plottingdata()$Method, "<br>",
                                    "Artifact:",plottingdata()$Artifact, "<br>",
                                    "Metric:",plottingdata()$Metric, "<br>",
                                    "Result:",plottingdata()$Result, "<br>",
                                    "Training Examples:",plottingdata()$`Training Examples`, "<br>",
                                    "Features:",plottingdata()$Features, "<br>",
                                    "Hyperparameters:",plottingdata()$Hyperparameters, "<br>",
                                    "Normalised Performance:",plottingdata()$`Normalised Performance`, "<br>")
      gg
    })
  }    
# Create Shiny app ----
shinyApp(ui, shinyserver)
