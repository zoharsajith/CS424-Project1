library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(rsconnect)

#START CODE FOR FORMATTING DATA
#
#
#
#read in the file
litter <- read.csv(file = "litterati challenge-65.csv", sep = ",", header = TRUE)
#get rid of all the values that arent in america
litter[(litter$lon<42.5 & litter$lon > 41.5), ] <- NA

#change up some column variable types
litter$username <- as.character(litter$username)
litter$url <- as.character(litter$url)
litter$tags <- as.character(litter$tags)

#change all the empty tag to untagged
litter$tags[litter$tags == ""] <- "untagged"

#format the date and time to use in our graphs
litter$litterTimestamp <- as.POSIXct(x=litter$litterTimestamp,format = "%Y-%m-%d %H:%M")
#
#
#
#END CODE FOR FORMATTING DATA

ui <- dashboardPage(
  dashboardHeader(title = "Analysis of Litterati Data",
                  titleWidth = 300),
  dashboardSidebar(width = 300),
  dashboardBody(
    fluidRow(
      column(width = 10,
      box(width = NULL, solidHeader = TRUE, title = "Map of all Pieces of Data Picked Up",
          leafletOutput("coordMap", height = 500)),
      ),
      column(width = 2,
             box(width = NULL, solidHeader = TRUE, height = 500, title = "Total Number of Trash Picked Up",
                 textOutput("number"),
                 verbatimTextOutput("num")
                 )
             )
    ),
    fluidRow(
      column(width = 4,
             box(width = NULL, solidHeader = TRUE, title = "List of top 10 Users",
                 DT::dataTableOutput("totalUsers", height = 200)),
             ),
      column(width = 4,
              mainPanel(width = 12,
                tabsetPanel(
                  tabPanel("Plot",
                           box(width = NULL, solidHeader = TRUE, title = "Trash Picked up by Hour Graph",
                               plotOutput("hours", height = 500))),
                  tabPanel("Table",
                           box(width = NULL, solidHeader = TRUE, title = "Trash Picked up by Hour Table",
                               DT::dataTableOutput("totalHours", height = 500)
                           )
                  )
                ) 
              )
             ),
      column(width = 4,
             mainPanel(width = 12,
               tabsetPanel(
                 tabPanel("Plot",
                   box(width = NULL, solidHeader = TRUE, title = "Trash Picked up by Days Graph",
                      plotOutput("days", height = 500)
                   )
                 ),
                 tabPanel( "Table",
                   box(width = NULL, solidHeader = TRUE, title = "Trash Picked up by Days Table",
                       DT::dataTableOutput("daysTotal", height = 500)
                    
                   )
                 )
               )
             )
      )
    ),
    fluidRow(
      column(width = 6,
             mainPanel(width = 12,
                tabsetPanel(
                  tabPanel("Plot",
                      box(width = NULL, solidHeader = TRUE, title = "Trash picked up by Hour Graph",
                          plotOutput("dayOfWeek", height = 500))         
                  ),
                  tabPanel("Table",
                    box(width = NULL, solidHeader = TRUE, title = "Trash picked up by Hour Table",
                        DT::dataTableOutput("dayOfWeekTable", height = 500))
                  )
                )          
             )
      ),
      column(width = 6,
             mainPanel(width = 12,
               tabsetPanel(
                 tabPanel("Plot",
                   box(width = NULL, solidHeader = TRUE, title = "Trash Picked up Per Day Graph",
                       plotOutput("totalDays", height = 500))
                   ),
                 tabPanel("Table",
                   box(width = NULL, solidHeader = TRUE, title = "Trash Picked up Per Day Table",
                       DT::dataTableOutput("totalDaysTable", height = 500)
                       )
                 )
                 )
               )
             )
    )
    
  )
)

server <- function(input, output) {
  
  #function to work on the map
  output$coordMap <- renderLeaflet({
    lonTable = as.data.frame(litter$lon)
    latTable = as.data.frame(litter$lat)
    
    #rename the table columns
    names(lonTable)[1] = 'lon'
    names(latTable)[1] = 'lat'
    
    #replace all the 0 values with NA
    lonTable[lonTable == 0] <- NA
    latTable[latTable == 0] <- NA
    
    
    #take out all the NA values
    lon <- na.omit(lonTable)
    lat <- na.omit(latTable)
    
    #combine the tables together to use for the map
    totalTable <- cbind(lon, lat)
    omitTable <- na.omit(totalTable)
    
    #make table with lon/lat points on leaflet
    map <- leaflet() %>%
      addTiles() %>%
      addMarkers(data = omitTable , lng = ~lon ,lat = ~lat, 
                 clusterOptions = markerClusterOptions(), label=) %>%
      fitBounds(lng1 = -87,
                lat1 = 41,
                lng2 = -88,
                lat2 = 42) 
    map  
    })
  
  output$num <- renderText({
    "12646"
  })
  
  
  #function to make the total number of trash picked up show up
  output$totalUsers <- DT::renderDataTable({
    w = table(litter$username)
    #send that table into a dataframe so we can manipulate it
    #with a frequency
    t = as.data.frame(w)
    #give the column with usernames a name
    names(t)[1] = 'username'
    
    #send the ordered data into a new dataframe
    newdata <- head(t[order(-t$Freq),], n=10)
    newdata
  })
  
  output$totalHours <- DT::renderDataTable({
    hours <- as.data.frame(litter$litterTimestamp)
    
    names(hours)[1] = "time"
    
    hour <- format(as.POSIXct(x = hours$time, format="%Y-%m-%d %H:%M:%S"), "%H")
    
    hour <- table(hour)
    
    hourdf <- as.data.frame(hour)
    hourdf
  })
  
  output$hours <- renderPlot({
    
    hours <- as.data.frame(litter$litterTimestamp)
    
    names(hours)[1] = "time"
    
    hour <- format(as.POSIXct(x = hours$time, format="%Y-%m-%d %H:%M:%S"), "%H")
    
    hour <- table(hour)
    
    hourdf <- as.data.frame(hour)
    
    p <- ggplot(data = hourdf, aes(x =hourdf$hour, y = hourdf$Freq)) +
      geom_bar(stat="identity", width=.5,color="black", fill="white") +
      ggtitle("Amount of Trash Picked Up by Hour") +
      labs(x = "Hour of Day", y = "# of trash picked up") +
      theme(axis.text.x = element_text(face="bold", color="black", size=10)) +
      geom_text(aes(label = hourdf$Freq), position=position_dodge(width=.9), vjust=-.25)
    
    p
  })
  
  output$days <- renderPlot({
    dates <- as.data.frame(litter$litterTimestamp)
    
    names(dates)[1] = "date"
    
    day <- weekdays(as.Date(dates$date))
    
    date <- table(day)
    datedf <- as.data.frame(date)
    
    datedf$day <- factor(datedf$day, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday" ))
    
    
    p <- ggplot(data = datedf, aes(x = datedf$day, y=datedf$Freq)) +
      geom_bar(stat="identity", width=.5,color="black", fill="white") +
      ggtitle("Amount of Trash Picked Up by Day of the Week") +
      labs(x = "Day of the Week", y = "# of trash picked up per day") +
      theme(axis.text.x = element_text(face="bold", color="black", size=10)) +
      geom_text(aes(label = datedf$Freq), position=position_dodge(width=.9), vjust=-.25)
    p
  })
  
  output$daysTotal <- DT::renderDataTable({
    dates <- as.data.frame(litter$litterTimestamp)
    
    names(dates)[1] = "date"
    day <- weekdays(as.Date(dates$date))
    
    date <- table(day)
    datedf <- as.data.frame(date)
    
    datedf$day <- factor(datedf$day, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday" ))
    datedf
  })
  
  output$dayOfWeek <- renderPlot({
    w <- table(litter$tags)
    
    t <- as.data.frame(w)
    
    names(t)[1] = 'tags'
    
    newTable <- head(t[order(-t$Freq),], n = 29)
    
    
    newTable$tags[(grepl(',', newTable$tags)) == TRUE] <- NA
    
    
    omitTableTags <- na.omit(newTable)
    
    #got em bbyyyyyyyyy
    p <-ggplot(data = omitTableTags, aes(x=reorder(omitTableTags$tags, -omitTableTags$Freq), y=omitTableTags$Freq))+ 
      geom_bar(stat="identity", width=.5,color="black", fill="white") +
      ggtitle("Amount of Trash Picked Up by Tag") +
      labs(x = "Type of Trash", y = "# of trash") +
      theme(axis.text.x = element_text(face="bold", color="black", size=10, angle=90)) +
      geom_text(aes(label = omitTableTags$Freq), position=position_dodge(width=.9), vjust=-.25)
    p
  })
  
  output$dayOfWeekTable <- DT::renderDataTable({
    w <- table(litter$tags)
    
    t <- as.data.frame(w)
    
    names(t)[1] = 'tags'
    
    newTable <- head(t[order(-t$Freq),], n = 29)
    
    
    newTable$tags[(grepl(',', newTable$tags)) == TRUE] <- NA
    
    
    omitTableTags <- na.omit(newTable)
  })
  
  output$totalDays <- renderPlot({
    days <- as.data.frame(litter$litterTimestamp)
    
    names(days)[1] = "days"
    daystotal <- format(as.POSIXct(x = days$days, format = "%Y-%m-$d %H:%M:%S"), "%Y-%m-%d")
    
    md <- table(daystotal)
    
    daystotal <- as.data.frame(md)
    names(daystotal)[1] = "days"
    #daystotal
    
    p <- ggplot(data = daystotal, aes(x = daystotal$days, y=daystotal$Freq)) + 
      geom_bar(stat = "identity", width = .5, color = "black", fill = "white") +
      labs(x = "Days from Apr 4 2018 to Jan 7, 2020", y = "Number of trash picked up")
    p
  })
  
  output$totalDaysTable <- DT::renderDataTable({
    days <- as.data.frame(litter$litterTimestamp)
    
    names(days)[1] = "days"
    daystotal <- format(as.POSIXct(x = days$days, format = "%Y-%m-$d %H:%M:%S"), "%Y-%m-%d")
    
    md <- table(daystotal)
    
    daystotal <- as.data.frame(md)
    names(daystotal)[1] = "days"
    daystotal
  })
  
  
}


shinyApp(ui, server)




