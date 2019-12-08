library(readr)
library(tidyverse)
library(scales)
library(sf)
library(here)
library(stringr)
library(shiny)
library(lubridate)

# load crime data 
#setwd("/Users/paulosoares/DataViz_Nadia")

c1 <- read_csv("./Chicago_Crimes_2001_to_2004.csv")
c2 <- read_csv("./Chicago_Crimes_2005_to_2007.csv")
c3 <- read_csv("./Chicago_Crimes_2008_to_2011.csv")
c4 <- read_csv("./Chicago_Crimes_2012_to_2017.csv")

df <- rbind(c1, c2, c3, c4)
df$X1 <- NULL

df <- df %>% distinct()
df <- df[complete.cases(df),]
df <- df %>% filter(Year != 2001 & Year != 2017) # Filtering out the incomplete years.

df$Date <- mdy_hms(df$Date)
df$Hour <- substring(df$Date, 12,13)
df$Date <- as.Date(df$Date, format="%m/%d/%Y")
df$Month = as.factor(months(df$Date))
df$Weekday = as.factor(weekdays(df$Date))

df <- sample_frac(df, 0.1)

ui <- fluidPage(
  titlePanel("Chicago Crime"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "num",
          label = "Choose a year",
          value = 2002, min = 2002, max = 2016),
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Time", plotOutput("year"), plotOutput("month"), plotOutput("weekday"), plotOutput("hour")),
                  tabPanel("Domestic", plotOutput("domestic"), plotOutput("domesticTypes")),
                  tabPanel("Types", plotOutput("types"), plotOutput("topTypes"), plotOutput("typeVsArrests"), plotOutput("typeVsDomestic"))
      )
    )
  )
)

server <- function(input, output) {
  output$year <- renderPlot({
    df %>% count(Year) %>% ggplot(aes(x=Year, y=n)) + 
      geom_bar(stat="identity", fill="darkturquoise") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(labels = comma, breaks = scales::pretty_breaks(n = 14)) +
      theme(axis.text.x = element_text(angle = 90, size=10),
            axis.text.y = element_text(size=10),
            axis.title = element_text(size=14)) +
      ggtitle("Number of crimes, per year, from 2002 to 2016") +
      labs(y="Count of occurrences", x = "Year") +
      theme(legend.position = "none")
  })

  output$month <- renderPlot({
    df %>% group_by(Year) %>% count(Month) %>%
      ungroup(Month) %>%
      mutate(Month = factor(Month, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) %>%
      ggplot(aes(x=as.factor(Month), y=n))+
      geom_boxplot()+
      ggtitle("Distribution crimes per year by month") +
      labs(y="Occurrences per year", x = "Month")
  })

  output$weekday <- renderPlot({
    df %>% group_by(Year) %>% count(Weekday) %>%
      ungroup(Weekday) %>%
      mutate(Weekday = factor(Weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
      ggplot(aes(x=as.factor(Weekday), y=n))+
      geom_boxplot()+
      ggtitle("Distribution crimes per year by weekday") +
      labs(y="Occurrences per year", x = "Weekday")
  })

  output$hour <- renderPlot({
    df %>% group_by(Year) %>% count(Hour) %>% ggplot(aes(x=as.factor(Hour), y=n))+
      geom_boxplot()+
      ggtitle("Distribution crimes per year by the Hours of the day") +
      labs(y="Occurrences per year", x = "Hour")
  })

  output$domestic <- renderPlot({
    df %>%
      dplyr::filter(Year == input$num) %>%
      count(Domestic) %>%
      ggplot(aes(x=reorder(as.factor(Domestic), n), y=n, fill=Domestic)) + 
        geom_bar(stat="identity") +
        scale_y_continuous(labels = comma, breaks = scales::pretty_breaks(n = 10)) +
        theme(axis.text.x = element_text(angle = 90, size=5)) +
        ggtitle("Counts of domestic and non-domestic crimes") +
        labs(y="Count of occurrences", x = "Domestic")
  })

  output$domesticTypes <- renderPlot({
    df %>%
      filter(Domestic == TRUE) %>%
      count(`Primary Type`, Domestic) %>%
      ggplot(aes(x=reorder(as.factor(`Primary Type`), n), y=n, fill=Domestic)) + 
      geom_bar(stat="identity") +
      coord_flip() +
      scale_y_continuous(labels = comma, breaks = scales::pretty_breaks(n = 20)) +
      theme(axis.text.x = element_text(angle = 90, size=5)) +
      ggtitle("Primary types of crimes in Chicago from 2002 to 2016") +
      labs(y="Count of occurrences", x = "Primary type of crime") +
      theme(legend.position = "none")
  })
  
  output$types <- renderPlot({
    df %>%  count(`Primary Type`, Year) %>%
      ggplot(aes(x=as.numeric(Year), y=as.numeric(n), colour=`Primary Type`, group=`Primary Type`, label = `Primary Type`)) + 
      geom_line() +
      facet_wrap(~`Primary Type`, ncol = 12) +
      theme_linedraw() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, size=5)) +
      ggtitle("Types of crimes, per year, from 2002 to 2016") +
      labs(y="Count of occurrences", x = "Year")
  })
  
  output$topTypes <- renderPlot({
    df %>% filter(`Primary Type` == "THEFT" | `Primary Type` == "BATTERY" |`Primary Type` == "CRIMINAL DAMAGE" | `Primary Type` == "NARCOTICS"|`Primary Type` == "OTHER OFFENSE" | `Primary Type` == "ASSAULT") %>%
      count(`Primary Type`, Year) %>% 
      ggplot(aes(x=as.numeric(Year), y=as.numeric(n), colour=`Primary Type`, group=`Primary Type`, label = `Primary Type`)) + 
      geom_line() +
      #geom_point() +
      #geom_text(aes(label = `Primary Type`), vjust = -0.5) +
      scale_y_continuous(labels = comma, breaks = scales::pretty_breaks(n = 10)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggtitle("Number of crimes, per year, from 2002 to 2016") +
      labs(y="Count of occurrences", x = "Year")+
      theme(axis.text.x = element_text(angle = 45, size=5))
  })
  
  output$typeVsArrests <- renderPlot({
    df %>% count(`Primary Type`, Arrest) %>% ggplot(aes(x=reorder(as.factor(`Primary Type`), n), y=n, fill=Arrest)) + 
      geom_bar(stat="identity") +
      coord_flip() +
      scale_y_continuous(labels = comma, breaks = scales::pretty_breaks(n = 20)) +
      theme(axis.text.x = element_text(angle = 90, size=5)) +
      ggtitle("Primary types of crimes in Chicago from 2002 to 2016") +
      labs(y="Count of occurrences", x = "Primary type of crime") # TODO: Fix titulos. Fix location_description. Aggregar os q tem pouco numa categoria Other.
  })
  
  output$typeVsDomestic <- renderPlot({
    df %>% count(`Primary Type`, Domestic) %>% ggplot(aes(x=reorder(as.factor(`Primary Type`), n), y=n, fill=Domestic)) + 
      geom_bar(stat="identity") +
      coord_flip() +
      scale_y_continuous(labels = comma, breaks = scales::pretty_breaks(n = 20)) +
      theme(axis.text.x = element_text(angle = 90, size=5)) +
      ggtitle("Primary types of crimes in Chicago from 2002 to 2016") +
      labs(y="Count of occurrences", x = "Primary type of crime")
  })
}

shinyApp(ui = ui, server = server)