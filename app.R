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

df <- rbind(c1, c2)#, c3, c4)
#df$X1 <- NULL

df <- df %>% distinct()
df <- df[complete.cases(df),]

df <- sample_frac(df, 0.1)

ui <- fluidPage(
  titlePanel("Chicago Crime"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "num",
          label = "Choose a year",
          value = 2001, min = 2001, max = 2017),
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Domestic",plotOutput("domestic")),
                  tabPanel("Types", plotOutput("types"))
      )
    )
  )
)

server <- function(input, output) {
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

  output$types <- renderPlot({
    df %>%  count(`Primary Type`, Year) %>%
      ggplot(aes(x=as.numeric(Year), y=as.numeric(n), colour=`Primary Type`, group=`Primary Type`, label = `Primary Type`)) + 
      geom_line() +
      facet_wrap(~`Primary Type`, ncol = 12) +
      theme_linedraw() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, size=5)) +
      ggtitle("Types of crimes, per year, from 2001 to 2017") +
      labs(y="Count of occurrences", x = "Year")
  })
}

shinyApp(ui = ui, server = server)