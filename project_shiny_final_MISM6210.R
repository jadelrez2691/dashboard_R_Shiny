#Final Project MISM 6210
#Ivan, Jad, Dante, Haripriya



#Loading Packages
library(shiny)
library(ggplot2)
library(readr)
library(plotly)
library(tidytext)
library(wordcloud)
library(plotly)
library(wordcloud)
library(dplyr)
library(lubridate)
library(shinythemes)

#Loading and Setting up Data

sample_data <- read_csv("final_dataset_project_MISM6210.csv")

tokenized_data <- sample_data %>%
    unnest_tokens(word,comments)
wc_dat <- tokenized_data %>%
    select(id, reviewer_id,word)%>%
    anti_join(get_stopwords())%>%
    count(word,sort = TRUE)




#User Interface

ui <- fluidPage(
    theme = shinytheme("cosmo"),
    navbarPage("Air BnB Boston Neighbourhood Safety Dashboard",
     tabPanel("Neighbourhood","Neighbourhood in Boston",
        selectInput("Neighbourhood","Choose a Neighbourhood", c("East Boston","Downtown","Charlestown","Roxbury","Mattapan","South Boston","Dorchester","South End","Brighton","West Roxbury","Jamaica Plain","Hyde Park"),multiple=TRUE),
        mainPanel(
            h1("Price , Crime, and Room"),
            plotlyOutput("price_hist"),
                plotlyOutput("room_hist"),
        )),
     tabPanel("Sentiment", h1("Sentiment of Air Bnb Listing"),
        sliderInput("words", "Number of Words",value=0,min=0,max=100),
        mainPanel(
            plotlyOutput("sent_hist"),
                plotOutput("word_cloud")
            )
    ),
     tabPanel("Analysis", h1("Additional Analysis"),
             mainPanel(
                 plotlyOutput("analysis_plot"),
             )
    )
    )
)
        
    
    


#Graphic Component

server <-function(input,output) {
    output$price_hist <- 
        renderPlotly( 
        ggplotly(
            ggplot(subset(sample_data,neighbourhood %in% input$Neighbourhood ),
                   aes(neighbourhood,price,fill=crime))+
                geom_bar(stat= "summary", fun="mean",
                         show.legend=TRUE)+
                labs(x = "Neighbourhood",
                     y="Average Price ",
                     title="Histogram of Price of AirBnB by Neighbourhood",
                     fill="Crime Rate")
            
        )
    )
    output$sent_hist <- 
        renderPlotly( 
            ggplotly(
                ggplot(sample_data,
                       aes(y=word_count,x=ave_sentiment))+
                    geom_point(
                        color="#37bae6",
                        show.legend=TRUE)+
                    labs(y = "#Word Count of Review",
                         x="Average Sentiment",
                         title="Average Sentiment by length of post")+
                    coord_cartesian(xlim =c(0,1.5))
            )
        )
    output$room_hist <- 
        renderPlotly( 
            ggplotly(
                ggplot(subset(sample_data,neighbourhood %in% input$Neighbourhood ),
                       aes(neighbourhood, fill = room_type))+
                    geom_bar(color = "black",
                             position = "dodge")+
                    labs(title = "Listing by room type",
                         x = "Neighbourhood district code",
                         y = "frequency")
            )
        )
    output$word_cloud <- renderPlot( 
            wordcloud(wc_dat$word,
                      wc_dat$n,
                      max.words=input$words,
                      colors=brewer.pal(6,"Dark2")
                
            )
        )
    output$analysis_plot <- 
        renderPlotly( 
            ggplotly(
                ggplot(sample_data,
                       aes(ave_sentiment))+
                    geom_density(color = 'lightslateblue',show.legend = FALSE)+
                    labs(title = "Airbnb review sentiment frequency distribution",
                         x = "Avg Sentiment",
                         y = "frequency")+
                    xlim(c(0,1))
            )
        )
} 






#Visuals set to variables

price_histogram <- ggplot(sample_data,
       aes(neighbourhood,price,fill=crime))+
    geom_bar(stat= "summary", fun="mean",
             show.legend=TRUE)+
    labs(x = "Neighbourhood",
         y="Average Price ",
         title="Histogram of Price of AirBnB by Neighbourhood",
         fill="Crime Rate")


ggplotly(price_histogram)

sent_scatterplot <- ggplot(sample_data,
       aes(y=word_count,x=ave_sentiment))+
    geom_point(
        color="#37bae6",
        show.legend=TRUE)+
    labs(y = "#Word Count of Review",
         x="Average Sentiment",
         title="Average Sentiment by length of post")+
    coord_cartesian(xlim =c(0,1.5))

ggplotly(sent_scatterplot)

room_barplot <- ggplot(sample_data,
                       aes(neighbourhood, fill = room_type))+
    geom_bar(color = "black",
             position = "dodge")+
    labs(title = "Listing by room type",
         x = "Neighbourhood district code",
         y = "frequency")

word_cloud <- wordcloud(wc_dat$word,
              wc_dat$n,
              max.words=50,
              colors=brewer.pal(6,"Dark2"))

word_cloud

sent_line <-ggplot(sample_data,
                   aes(ave_sentiment))+
    geom_density(color = 'lightslateblue',show.legend = FALSE)+
    labs(title = "Airbnb review sentiment frequency distribution",
         x = "Avg Sentiment",
         y = "frequency")+
    xlim(c(0,1))
ggplotly(sent_line)



#Running App
shinyApp(ui = ui, server = server)

