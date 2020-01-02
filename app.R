library(leaflet)
library(tidyverse)
library(shiny)
library(scales)
library(ggstatsplot)
library(treemapify)


df.map <- community_areas %>% filter(Year %in% c(2008,2009,2010,2011,2012)) %>% 
  select(-c(`MOST COMMON PRIMARY TYPE`, `Arrest`, `Domestic`))

df.map <- df.map %>%
  group_by_at(c(1:12,48:51)) %>%
  summarise_each(sum, -geometry) %>% ungroup()

df.map[,c(18:52)] <- as.data.frame(df.map)[,c(18:52)] / df.map[,"X2010"]$X2010

df.map <- st_transform(df.map, "+proj=longlat +datum=WGS84")

# Choices for drop-downs
color_vars <- c(
  "Population density" = "POPULATION DENSITY",
  "Per capita income" = "PER CAPITA INCOME",
  "Households below poverty (%)" = "PERCENT HOUSEHOLDS BELOW POVERTY",
  "Housing crowded (%)" = "PERCENT OF HOUSING CROWDED",
  "Aged 16+ unemployed (%)" = "PERCENT AGED 16+ UNEMPLOYED",
  "Aged 25+ w/o high school diploma (%)" = "PERCENT AGED 25+ WITHOUT HIGH SCHOOL DIPLOMA",
  "Per capita hispanics" = "PER CAPITA HISPANICS"
)

size_vars <- colnames(df.map)[c(52,18:51)]

ui <- navbarPage("Chicago Crime", id="nav",
           tabPanel("Interactive map",
                    div(class="outer",

                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),

                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),

                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 600, height = "auto",

                                      h2("Community Area explorer"),

                                      selectInput("color", "Socioeconomic Indicator (Color)", color_vars, selected = "Per capita income"),
                                      selectInput("size", "Per Capita Primary Type (Size)", size_vars, selected = "ALL"),

                                      plotOutput("histCentile", height = 400)
                                      #plotOutput("scatterCollegeIncome", height = 250)
                                      ),
                        tags$div(id="cite",
                                 'Data compiled from ', tags$em('Census Data - Selected socioeconomic indicators in Chicago, 2008 - 2012'), ' by City of Chicago.'
                          )
                        )
                    ),
           tabPanel("Choropleth maps",sidebarLayout(
             sidebarPanel(
                    sliderInput(inputId = "num1",
                                label = "Choose a year or click the play button below 2016. Disclaimer: Animation velocity may vary with maps' refresh time!",
                                value = 2002, min = 2002, max = 2016, sep = "",
                                step = 1, width = '100%',
                               dragRange = FALSE, animate = animationOptions(interval = 3500)), width = '100%', height = 100),
             mainPanel(
               splitLayout(
                        column(width = 12, leafletOutput("map40", height = 475)),
                        column(width = 12, leafletOutput("map10", height = 475)),
                        column(width = 12, leafletOutput("map20", height = 475)),
                        column(width = 12, leafletOutput("map30", height = 475)), cellWidths = c('37%','37%','37%','37%','37%'))
                    ))),
           tabPanel("Dashboard",
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput(inputId = "num",
                                    label = "Choose a year range",
                                    value = c(2002, 2016), min = 2002, max = 2016, sep = ""),
                        selectInput("Month",
                                    "Month:",
                                    c("All",
                                      "January", "February", "March", "April", "May", "June", "July",
                                      "August","September","October","November","December")),
                        selectInput("Weekday",
                                    "Weekday:",
                                    c("All",
                                      "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
                        selectInput("Primary Type",
                                    "Primary Type:",
                                    c("All",
                                      sort(unique(as.character(df$`Primary Type`))))),
                        selectInput("Arrest",
                                    "Arrest:",
                                    c("All",
                                      sort(unique(as.character(df$Arrest))))),
                        selectInput("Domestic",
                                    "Domestic:",
                                    c("All",
                                      sort(unique(as.character(df$Domestic))))),
                        selectInput("Location Description",
                                    "Location Description:",
                                    c("All",
                                      sort(unique(as.character(df$`Location Description`))))),
                        selectInput("Community Area",
                                    "Community Area:",
                                    c("All",
                                      sort(unique(as.character(df$`COMMUNITY AREA NAME`)))))
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Time - Tendency", plotOutput("year"), plotOutput("yearMonth")),
                                    tabPanel("Time - Cycles",
                                             column(width = 12, plotOutput("month", height = 260), hr()),
                                             column(width = 12, plotOutput("weekday", height = 260), hr()), 
                                             column(width = 12, plotOutput("hour", height = 260))),
                                    tabPanel("Crime Primary Types",
                                             fluidRow(
                                               column(width = 6, plotOutput("topTypes")),
                                               column(width = 6, plotOutput("descriptionCloud"))
                                             ), hr(),
                                             plotOutput("types", height=750),
                                             ),
                                    tabPanel("Arrests",
                                             plotOutput("arrestsByTypes"),
                                             hr(),
                                             plotOutput("typeVsArrests")
                                             ),
                                    tabPanel("Domestic",
                                             plotOutput("domesticByTypes"),
                                             hr(),
                                             plotOutput("domestic")
                                    )
                              )
                        )
                      )
                    )
           )

server <- function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(-87.4321032,41.8730856, zoom = 11)
  })
  
  output$histCentile <- renderPlot({
    
    
    ggstatsplot::ggscatterstats(
      data = as.data.frame(df.map),
      x = !!sym(input$size),
      y = !!sym(input$color),
      xlab = paste("PER CAPITA", as.character(input$size)),
      ylab = as.character(input$color),
      title = "Understanding crimes by socioeconomic indicator",
      messages = FALSE,
      ggstatsplot.layer = FALSE, # turn off ggstatsplot theme layer
      marginal.type = "density", # type of marginal distribution to be displayed
      xalpha = 0.6, # transparency for x-axis marginal distribution
      yalpha = 0.6, # transparency for y-axis marginal distribution
    )
    
  })
  
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size
    
    colorData <- df.map[[colorBy]]
    pal <- colorBin("viridis", colorData, pretty = TRUE)
    
    radius <- df.map[[sizeBy]] / max(df.map[[sizeBy]]) * 3000
    radius[is.na(radius)] <- 0
    
    leafletProxy("map", data = df.map) %>%
      clearShapes() %>%
      addCircles(~lat.centroid, ~lon.centroid, radius=radius, layerId=~area_num_1,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })
  
  showZipcodePopup <- function(area_num_1, lat, lng) {
    selectedZip <- df.map[df.map$area_num_1 == area_num_1,]
    content <- as.character(tagList(
      #tags$h4(paste(paste("PER CAPITA", as.character(input$size)),':',sep=''), round(selectedZip[[input$size]],3)),
      tags$strong(HTML(sprintf("%s, %s %s",
                               selectedZip$`COMMUNITY AREA NAME`, "population:", number(selectedZip$`X2010`)
      ))), tags$br(),
      sprintf("Population density: %s", round(selectedZip$`POPULATION DENSITY`,3)), tags$br(),
      sprintf("Per capita income: %s", dollar(selectedZip$`PER CAPITA INCOME`)), tags$br(),
      sprintf("Percent of households below poverty: %s%%", selectedZip$`PERCENT HOUSEHOLDS BELOW POVERTY`), tags$br(),
      sprintf("Percent of housing crowded: %s%%", selectedZip$`PERCENT OF HOUSING CROWDED`), tags$br(),
      sprintf("Percent of aged 16+ unemployed: %s%%", selectedZip$`PERCENT AGED 16+ UNEMPLOYED`), tags$br(),
      sprintf("Percent of aged 25+ w/o high school diploma: %s%%", selectedZip$`PERCENT AGED 25+ WITHOUT HIGH SCHOOL DIPLOMA`), tags$br(),
      sprintf("Per capita hispanics: %s", round(selectedZip$`PER CAPITA HISPANICS`,3))
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = area_num_1)
  }
  
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
  ## Choropleth maps ###########################################

  data1 <- reactiveValues(time_df1 = community_areas)
  
  observe({
    time_range1 <- input$num1
    data1$time_df1 <- community_areas %>% filter(Year == time_range1[1])
  })
  
  output$map10 <- renderLeaflet({
    pal <- colorBin("YlOrRd", domain = data1$time_df1[['ALL']], pretty = T, bins = 5)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s crimes",
      data1$time_df1$`COMMUNITY AREA NAME`, scales::number(data1$time_df1[['ALL']])
    ) %>% lapply(htmltools::HTML)
    
    leaflet("map10", data = st_transform(data1$time_df1, "+proj=longlat +datum=WGS84")) %>%
      clearShapes()%>%
      addPolygons(
        fillColor = ~pal(data1$time_df1[['ALL']]),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend("bottomleft", pal=pal, values=data1$time_df1[['ALL']], title='Number of crimes',
                layerId="colorLegend") %>% setView(-87.75,41.825, zoom = 10)
  })
  
  output$map20 <- renderLeaflet({
    pal <- colorBin("Greens", domain = data1$time_df1[['Arrest']], pretty = T, bins = 5)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s arrests",
      data1$time_df1$`COMMUNITY AREA NAME`, scales::number(data1$time_df1[['Arrest']])
    ) %>% lapply(htmltools::HTML)
    
    leaflet(st_transform(data1$time_df1, "+proj=longlat +datum=WGS84")) %>% 
      setView(-87.75,41.825, zoom = 10) %>% 
      addPolygons(
        fillColor = ~pal(data1$time_df1[['Arrest']]),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend("bottomleft", pal=pal, values=data1$time_df1[['Arrest']], title='Number of arrests',
                layerId="colorLegend")
    
  })
  
  output$map30 <- renderLeaflet({
    pal <- colorBin("Purples", domain = data1$time_df1[['Domestic']], pretty = T, bins = 5)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s domestic crimes",
      data1$time_df1$`COMMUNITY AREA NAME`, scales::number(data1$time_df1[['Domestic']])
    ) %>% lapply(htmltools::HTML)
    
    leaflet(st_transform(data1$time_df1, "+proj=longlat +datum=WGS84")) %>% 
      setView(-87.75,41.825, zoom = 10) %>% 
      addPolygons(
        fillColor = ~pal(data1$time_df1[['Domestic']]),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend("bottomleft", pal=pal, values=data1$time_df1[['Domestic']], title='Number of domestic crimes',
                layerId="colorLegend")
    
  })
  
  output$map40 <- renderLeaflet({
    pal <- colorFactor("Dark2", domain = data1$time_df1[['MOST COMMON PRIMARY TYPE']])
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Most common: %s",
      data1$time_df1$`COMMUNITY AREA NAME`, data1$time_df1[['MOST COMMON PRIMARY TYPE']]
    ) %>% lapply(htmltools::HTML)
    
    leaflet(st_transform(data1$time_df1, "+proj=longlat +datum=WGS84")) %>% 
      setView(-87.75,41.825, zoom = 10) %>% 
      addPolygons(
        fillColor = ~pal(data1$time_df1[['MOST COMMON PRIMARY TYPE']]),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend("bottomleft", pal=pal, values=data1$time_df1[['MOST COMMON PRIMARY TYPE']], title='Most common primary type',
                layerId="colorLegend")
    
  })
  

  
  ## Dashboard ###########################################

  data <- reactiveValues(time_df = df)

  observe({
    time_range <- input$num
    selected_month <- input$Month
    selected_weekday <- input$Weekday
    
    selected_type <- input$`Primary Type`
    selected_arrest <- input$Arrest
    selected_domestic <- input$Domestic
    selected_location <- input$`Location Description`
    selected_community <- input$`Community Area`
    
    temp_df <- df %>% dplyr::filter(Year >= time_range[1] & Year <= time_range[2])
    
    if (selected_month != "All") {
      temp_df <- temp_df %>% dplyr::filter(Month == selected_month)
    }
    if (selected_weekday != "All") {
      temp_df <- temp_df %>% dplyr::filter(Weekday == selected_weekday)
    }
    if (selected_type != "All") {
      temp_df <- temp_df %>% dplyr::filter(`Primary Type` == selected_type)
    }
    if (selected_arrest != "All") {
      temp_df <- temp_df %>% dplyr::filter(Arrest == selected_arrest)
    }
    if (selected_domestic != "All") {
      temp_df <- temp_df %>% dplyr::filter(Domestic == selected_domestic)
    }
    if (selected_location != "All") {
      temp_df <- temp_df %>% dplyr::filter(`Location Description` == selected_location)
    }
    if (selected_community != "All") {
      temp_df <- temp_df %>% dplyr::filter(`COMMUNITY AREA NAME` == selected_community)
    }
    
    data$time_df <- temp_df
  })

  #### Time - Tendency tab #####################
  output$year <- renderPlot({
    year_df <- data$time_df %>%
      dplyr::select(Year)
    a <- table(year_df)
    m <- ceiling(mean(a))
    year_df %>%
      count(Year) %>%
      ggplot(aes(x=Year, y=n)) +
      geom_bar(stat="identity", fill="steelblue") +
      geom_text(aes(label=scales::number(n)), vjust=1.6, color="white", size=3.5)+
      geom_hline(yintercept = m, linetype="dashed") +
      geom_label(aes(input$num[2], m, label = paste("mean = ", scales::number(m)), vjust = -0.2)) +
      #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_x_continuous(breaks=seq(2002, 2016, 1))+
      scale_y_continuous(labels = comma, breaks = scales::pretty_breaks(n = 14)) +
      ggtitle("Number of crimes per year") +
      labs(y="Count of occurrences", x = "Year") +
      theme_classic() +
      theme(axis.text.x = element_text(size = 12), title = element_text(size=16),
            legend.text=element_text(size=10), axis.text.y = element_text(size = 12)) 
  }, height = 400)

  output$yearMonth <- renderPlot({
    data$time_df %>%
      select(MonthYear) %>%
      count(MonthYear) %>%
      ggplot(aes(x=MonthYear, y=n)) + 
      geom_bar(stat="identity", fill="steelblue") +
      #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_x_continuous(breaks=seq(2002, 2016, 1))+
      scale_y_continuous(labels = comma, breaks = scales::pretty_breaks(n = 14)) +
      ggtitle("Number of crimes per month") +
      labs(y="Count of occurrences", x = "Month and Year") +
      theme_classic() +
      theme(axis.text.x = element_text(size = 12), title = element_text(size=16),
            legend.text=element_text(size=10), axis.text.y = element_text(size = 12))
  }, height = 400)

  #### Time - Cycle tab ########################
  output$month <- renderPlot({
    data$time_df %>%
      group_by(Year) %>%
      count(Month) %>%
      ungroup(Month) %>%
      mutate(Month = factor(Month, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) %>%
      ggplot(aes(x=as.factor(Month), y=n))+
      geom_boxplot(fill="steelblue") +
      ggtitle("Distribution of crimes per month") +
      labs(y="Count of occurrences", x = "Month") + 
      theme_classic() +
      theme(axis.text.x = element_text(size = 12), title = element_text(size=16),
            legend.text=element_text(size=10), axis.text.y = element_text(size = 12))
  }, height = 250)

  output$weekday <- renderPlot({
    data$time_df %>%
      group_by(Year) %>%
      count(Weekday) %>%
      ungroup(Weekday) %>%
      mutate(Weekday = factor(Weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
      ggplot(aes(x=as.factor(Weekday), y=n))+
      geom_boxplot(fill="steelblue") +
      ggtitle("Distribution of crimes per weekday") +
      labs(y="Count of occurrences", x = "Weekday") +
      theme_classic() +
      theme(axis.text.x = element_text(size = 12), title = element_text(size=16),
            legend.text=element_text(size=10), axis.text.y = element_text(size = 12))
  }, height = 250)

  output$hour <- renderPlot({
    data$time_df %>%
      group_by(Year) %>%
      count(Hour) %>%
      ggplot(aes(x=as.factor(Hour), y=n)) +
      geom_boxplot(fill="steelblue") +
      ggtitle("Distribution of crimes per hour") +
      labs(y="Count of occurrences", x = "Hour") +
      theme_classic() +
      theme(axis.text.x = element_text(size = 12), title = element_text(size=16),
            legend.text=element_text(size=10), axis.text.y = element_text(size = 12))
  }, height = 250)

  #### Primary Types ###########################
  output$types <- renderPlot({
    data$time_df %>%
      count(`Primary Type`, Year) %>%
      ggplot(aes(x=as.numeric(Year), y=as.numeric(n), colour=`Primary Type`, group=`Primary Type`, label = `Primary Type`)) + 
      geom_line() +
      facet_wrap(~`Primary Type`, ncol = 5) +
      theme_classic() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12), title = element_text(size=16),
             axis.text.y = element_text(size = 12), axis.title = element_text(size = 16)) +
      ggtitle("Types of crimes, per year") +
      labs(y="Count of occurrences", x = "Year")
  })
  
  output$topTypes <- renderPlot({
    data$time_df %>% filter(`Primary Type` == "THEFT" | `Primary Type` == "BATTERY" |`Primary Type` == "CRIMINAL DAMAGE" | `Primary Type` == "NARCOTICS" | `Primary Type` == "ASSAULT") %>%
      count(`Primary Type`, Year) %>% 
      ggplot(aes(x=as.numeric(Year), y=as.numeric(n), colour=`Primary Type`, group=`Primary Type`, label = `Primary Type`)) + 
      geom_line() + scale_colour_brewer(palette="Dark2") +
      #geom_point() +
      #geom_text(aes(label = `Primary Type`), vjust = -0.5) +
      scale_y_continuous(labels = comma, breaks = scales::pretty_breaks(n = 10)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggtitle("Evolution of top-5 crimes") +
      labs(y="Count of occurrences", x = "Year")+
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12), title = element_text(size=16),
            legend.text=element_text(size=10), axis.text.y = element_text(size = 12))
    
  })

  output$descriptionCloud <- renderPlot({
    ggplot(data$time_df %>% count(`Primary Type`), aes(area=n, label=`Primary Type`, fill=n))  +
     geom_treemap() +
      geom_treemap_text(fontface = "italic", colour = "white", place = "centre", grow = TRUE) + labs(fill = "Count of occurrences") + ggtitle('Most relevant Primary Type') +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12), title = element_text(size=16),
            legend.text=element_text(size=10), legend.title=element_text(size=10), axis.text.y = element_text(size = 12))
  })

  #### Arrests #################################

  output$arrestsByTypes <- renderPlot({
    data$time_df %>% count(Arrest, Year) %>% ggplot(aes(x=as.factor(Year), y=n/100, fill=Arrest)) + 
      geom_bar(stat="identity", position = "fill") + scale_fill_brewer(palette="Blues") +
      scale_y_continuous(labels = percent, breaks = scales::pretty_breaks(n = 10)) + theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12), title = element_text(size=16),
            legend.text=element_text(size=10), axis.text.y = element_text(size = 12))+
      ggtitle("Proportion of arrests per year") +
      labs(y="Percentage of arrests", x = "Year")
    
  })
  
  output$typeVsArrests <- renderPlot({
    data$time_df %>% count(`Primary Type`, Arrest) %>% ggplot(aes(x=reorder(as.factor(`Primary Type`), n/100), y=n, fill=Arrest)) + 
      geom_bar(stat="identity", position = "fill") +
      coord_flip() + scale_fill_brewer(palette="Blues") +
      scale_y_continuous(labels = percent, breaks = scales::pretty_breaks(n = 20)) +
      theme(axis.text.x = element_text(angle = 90, size=5)) +
      ggtitle("Proportion of arrests per primary type of crime") +
      labs(y="Percentage of arrests", x = "Primary type of crime") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12), title = element_text(size=16),
            legend.text=element_text(size=10), axis.text.y = element_text(size = 8))
  })



  #### Domestic #################################
  output$domesticByTypes <- renderPlot({
    data$time_df %>% count(Domestic, Year) %>% ggplot(aes(x=as.factor(Year), y=n/100, fill=Domestic)) + 
      geom_bar(stat="identity", position = "fill") + scale_fill_brewer(palette="Blues") +
      scale_y_continuous(labels = percent, breaks = scales::pretty_breaks(n = 10)) + theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12), title = element_text(size=16),
            legend.text=element_text(size=10), axis.text.y = element_text(size = 12)) +
      ggtitle("Proportion of domestic crimes per year") +
      labs(y="Percentage of domestic crimes", x = "Year")
  })
  
  output$domestic <- renderPlot({
    data$time_df %>% count(`Primary Type`, Domestic) %>% ggplot(aes(x=reorder(as.factor(`Primary Type`), n/100), y=n, fill=Domestic)) + 
      geom_bar(stat="identity", position = "fill") +
      coord_flip() + scale_fill_brewer(palette="Blues") +
      scale_y_continuous(labels = percent, breaks = scales::pretty_breaks(n = 20)) +
      theme(axis.text.x = element_text(angle = 90, size=5)) +
      ggtitle("Proportion of domestic crimes per primary type of crime") +
      labs(y="Percentage of domestic crimes", x = "Primary type of crime") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12), title = element_text(size=16),
            legend.text=element_text(size=10), axis.text.y = element_text(size = 8))
  })
  
}

shinyApp(ui = ui, server = server)








