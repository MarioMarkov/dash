library(shiny)
library(shinydashboard)
library(readr)
library(readxl)
library(tidyverse)
library(data.table)
library(ggrepel)
library(lubridate)
library(plotly)
htaSubmissions <- as.data.table(read_excel("Dashboard_data.xlsx",
                                           range = "A1:K551"))
#htaSubmissions[,Year:= as.date(Year)]
htaSubmissions[,Year:= as.factor(Year)]

htaSubmissions = htaSubmissions[`HTA body` != "N/A"]
htaSubmissions = htaSubmissions[`Year` != "NA"]

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Plot1", icon = icon("th"), tabName = "Plot1"),
      menuItem("Plot2", icon = icon("th"), tabName = "Plot2"),
      menuItem("Plot3", icon = icon("th"), tabName = "Plot3"),
      menuItem("Plot4", icon = icon("th"), tabName = "Plot4")
    )
  ),
  body <- dashboardBody(
    
    tabItems(
      tabItem(tabName = "Plot1",
              fluidRow(
                box(
                  title = "Inputs", status = "warning", solidHeader = TRUE,
                  numericInput(value = 0,"projectsCount", "Total Projects selected:"),
                  sliderInput("percentOfTotal",label="% of total experience", min = 0, 
                              max = 100, post  = " %", value = 50),
                  sliderInput("yearRange", label = h3("Year range"), min = 2012, 
                              max = 2021, value = c(2012, 2021),sep = "")
                  
                ),
                box(plotOutput("plot1", height = 600))
                
              )
      ),
      
      tabItem(tabName = "Plot2",
              fluidRow(
                box(
                  title = "Inputs", status = "warning", solidHeader = TRUE,
                  selectInput("body","Select Team:",choices =names(table(htaSubmissions$`HTA body`))),
                  numericInput(value = 0,"projectsCount2", "Total Projects selected:"),
                  sliderInput("percentOfTotal2",label="% of total experience", min = 0, 
                              max = 100, post  = " %", value = 0)
                  
                )
                
              ),
              fluidRow(
                box(plotOutput("plot2", height = 700,width = 900))
              )
      ),
      tabItem(tabName = "Plot3",
              fluidRow(
                box(
                  title = "Inputs", status = "warning", solidHeader = TRUE,
                  numericInput(value = 0,"projectsCount3", "Total Projects selected:"),
                  sliderInput("yearRange3", label = h3("Year range"), min = 2012, 
                              max = 2021, value = c(2012, 2021),sep = "")
                )
                
              ),
              fluidRow(
                box(plotOutput("plot3", height = 500,width = 800))
              )
      ),
      tabItem(tabName = "Plot4",
              fluidRow(
                box(
                  title = "Inputs", status = "warning", solidHeader = TRUE,
                  numericInput(value = 0,"projectsCount4", "Total Projects selected:"),
                  sliderInput("yearRange4", label = h3("Year range"), min = 2012, 
                              max = 2021, value = c(2012, 2021),sep = "")
                )
                
              ),
              fluidRow(
                box(plotOutput("plot4", height = 500,width = 800))
              )
      )
    )
  ),
  
  # Put them together into a dashboardPage
  dashboardPage(
    dashboardHeader(title = "Simple tabs"),
    sidebar,
    body
  )
)


server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    data = htaSubmissions[,.(n = sum(.N)),by =.(`Therapy area`,`Year`)]
    data$Year <- as.character(data$Year)
    data$Year <- as.numeric(data$Year)
    
    data <- data[Year>=input$yearRange[1] 
                 &Year<=input$yearRange[2]
                 &n>=input$projectsCount]
    data$Year <- as.factor(data$Year)
    
    
    oncologyData = data[`Therapy area`== "Oncology"]
    oncologyData$Year <- as.numeric(oncologyData$Year)
    
    plotTherapyareaAndYear <- ggplot(data = data, aes(x= Year,y= n, color = `Therapy area`))+
      geom_point(size =2)+
      geom_line(data = oncologyData, aes(x= Year,y= n),size=1)+
      theme_classic()+
      theme(legend.title = element_blank() 
            ,axis.title.x=element_blank()
            ,axis.title.y = element_blank())+
      guides(color = guide_legend(ncol = 1)  ) 
    
    plotTherapyareaAndYear
  })
  
  output$plot2 <- renderPlot({
    data = htaSubmissions[,.(n = sum(.N)),by =.(`HTA body`,`Therapy area`)]
    body = input$body
    data = data[`HTA body` == body]
    sum = sum(data$n)
    
    data <- data[n>input$projectsCount2]
    
    data <- data[,fraction:= as.numeric(format((n /sum)*100,digits =2))]
    data <- data[fraction>=input$percentOfTotal2]
    
    # Compute a good label
    data$label <- paste0(data$`Therapy area`, "\n",data$fraction,"%")
    
    df2 <- data %>% 
      mutate(csum = rev(cumsum(rev(n))), 
             pos = n/2 + lead(csum, 1),
             pos = if_else(is.na(pos), n/2, pos))
    
    
    ggplot(df2, aes(x = "" , y = n, fill = fct_inorder(`Therapy area`))) +
      geom_col(width = 1, color = 1) +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Pastel1") +
      geom_label_repel(data = df2,
                       aes(y = pos, label = label),
                       size = 4.5, nudge_x = 1, show.legend = FALSE) +
      guides(fill = guide_legend(title = "Group")) +
      theme_void()
  })
  
  output$plot3 <- renderPlot({
    dataByBody = htaSubmissions[,.(n = sum(.N)),by =.(`HTA body`,`Year`)]
    dataByYear = htaSubmissions[,.(n = sum(.N)),by =.(`Year`)]
    sum = sum(dataByYear$n)
    
    dataByYear = dataByYear[,fraction:= as.numeric(format((n /sum)*100,digits =2))]
    
    dataByYear <- dataByYear[n>=input$projectsCount3]
    dataByBody <- dataByBody[n>=input$projectsCount3]
    dataByBody$Year <- as.character(dataByBody$Year)
    dataByBody$Year <- as.numeric(dataByBody$Year)
    
    dataByYear$Year <- as.character(dataByYear$Year)
    dataByYear$Year <- as.numeric(dataByYear$Year)
    #filter dataBYBody
    dataByBody <- dataByBody[Year>=input$yearRange3[1] 
                             &Year<=input$yearRange3[2]
                             &n>=input$projectsCount3]
    
    dataByBody$Year <- as.factor(dataByBody$Year)
    
    dataByYear <- dataByYear[Year>=input$yearRange3[1] 
                             &Year<=input$yearRange3[2]
                             &n>=input$projectsCount3]
    
    dataByYear$Year <- as.factor(dataByYear$Year)
    
    
    
    dataSpline <- as.data.frame(spline(dataByYear$Year, dataByYear$n))
    
    #plot by Hta Body
    ggplot(data = dataByBody)+
      geom_col(aes(x= Year, y = n,fill = `HTA body`),position=position_dodge2(padding = 0.3,preserve = 'single'))+
      geom_smooth(data = dataSpline,aes(x= x, y = y,group=1),
                  se=F,
                  method = "loess",
                  color ="#05c880")+
      geom_text(data = dataByYear,aes(x=Year,
                                      y=n ,
                                      label = n,
                                      family = "sans",
                                      fontface ="bold"),
                vjust ="center",hjust ="center")+
      theme_classic()+
      theme(legend.title = element_blank() )
    
  })
  
  output$plot4 <- renderPlot({
    
    dataByTeam = htaSubmissions[,.(n = sum(.N)),by =.(`EMEA team`,`Year`)]
    
    dataByYear = htaSubmissions[,.(n = sum(.N)),by =.(`Year`)]
    sum = sum(dataByYear$n)
    
    dataByYear = dataByYear[,fraction:= as.numeric(format((n /sum)*100,digits =2))]
    
    dataByYear <- dataByYear[n>=input$projectsCount4]
    dataByTeam <- dataByTeam[n>=input$projectsCount4]
    dataByTeam$Year <- as.character(dataByTeam$Year)
    dataByTeam$Year <- as.numeric(dataByTeam$Year)
    
    dataByYear$Year <- as.character(dataByYear$Year)
    dataByYear$Year <- as.numeric(dataByYear$Year)
    #filter dataBYBody
    dataByTeam <- dataByTeam[Year>=input$yearRange4[1] 
                             &Year<=input$yearRange4[2]
                             &n>=input$projectsCount4]
    
    dataByTeam$Year <- as.factor(dataByTeam$Year)
    
    dataByYear <- dataByYear[Year>=input$yearRange4[1] 
                             &Year<=input$yearRange4[2]
                             &n>=input$projectsCount4]
    
    dataByYear$Year <- as.factor(dataByYear$Year)
    
    
    
    dataSpline <- as.data.frame(spline(dataByYear$Year, dataByYear$n))
    #plot by Country
    ggplot(data = dataByTeam)+
      geom_col(aes(x= Year, y = n,fill = `EMEA team`),position=position_dodge2(padding = 0.3,preserve = 'single'))+
      geom_smooth(data = dataSpline,aes(x= x, y = y,group=1),
                  se=F,
                  method = "loess",
                  color ="#05c880")+
      geom_text(data = dataByYear,aes(x=Year,
                                      y=n ,
                                      label = n,
                                      family = "sans",
                                      fontface ="bold"),
                vjust ="center",hjust ="center")+
      theme_classic()+
      theme(legend.position = "bottom",
            legend.title = element_blank() ) +
      guides(fill = guide_legend(nrow = 1)  ) 
    
  })
}
shinyApp(ui = ui, server = server)
