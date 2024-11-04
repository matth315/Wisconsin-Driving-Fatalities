###########################
# Wisconsin Traffic Fatalites Shiny App
# Matthew Halberg
# 11/3/2024
# 
###########################

#General Packages
library(shiny)
library(readr)
library(ggplot2)
library(patchwork)
library(tidyverse)
library(ggiraph)
library(bslib)
library(dplyr)

#Tmap libraries
library(sf)
library(tmap)
library(tmaptools)


#Import the accident data
accident <- read_csv("www/accident.csv", col_types = cols(HOURNAME = col_character()))
accident$DAY_WEEKNAME <- factor(accident$DAY_WEEKNAME, levels=c("Sunday", "Monday",
                                                                "Tuesday", "Wednesday",
                                                                "Thursday", "Friday",
                                                                "Saturday"))

#Recode accident data
accident$ROUTENAME <- recode(accident$ROUTENAME,
       "State Highway" = "State Hwy",
       "County Road" = "Cty Road",
       "U.S. Highway" = "US Hwy",
       "Local Street - Municipality" = "Street",
       "Local Street - Frontage Road" = "Street",
       "Local Street - Township" = "Street",
       "Other" = "NA",
       "Unknown" = "NA")
accident$ROUTENAME <- factor(accident$ROUTENAME, levels=c("Street", "Cty Road",
                                                             "State Hwy", "US Hwy",
                                                             "Interstate", "NA"))
accident$MAN_COLLNAME <- recode(accident$MAN_COLLNAME,
                                "The First Harmful Event was Not a Collision with a Motor Vehicle in Transport" = "Non-vehicle",
                                "Sideswipe - Opposite Direction" = "Oncoming Sideswipe",
                                "Sideswipe - Same Direction" = "Same Sideswipe")
accident$HOURNAME <- recode(accident$HOURNAME,
                            "0:00am-0:59am" = "12am",
                            "1:00am-1:59am" = "1am",
                            "2:00am-2:59am" = "2am",
                            "3:00am-3:59am" = "3am",
                            "4:00am-4:59am" = "4am",
                            "5:00am-5:59am" = "5am",
                            "6:00am-6:59am" = "6am",
                            "7:00am-7:59am" = "7am",
                            "8:00am-8:59am" = "8am",
                            "9:00am-9:59am" = "9am",
                            "10:00am-10:59am" = "10am",
                            "11:00am-11:59am" = "11am",
                            "12:00pm-12:59pm" = "12pm",
                            "1:00pm-1:59pm" = "1pm",
                            "2:00pm-2:59pm" = "2pam",
                            "3:00pm-3:59pm" = "3pm",
                            "4:00pm-4:59pm" = "4pm",
                            "5:00pm-5:59pm" = "5pm",
                            "6:00pm-6:59pm" = "6pm",
                            "7:00pm-7:59pm" = "7pm",
                            "8:00pm-8:59pm" = "8pm",
                            "9:00pm-9:59pm" = "9pm",
                            "10:00pm-10:59pm" = "10pm",
                            "11:00pm-11:59pm" = "11pm",
                            "Unknown Hours" = "NA"
)

accident$HOURNAME <- factor(accident$HOURNAME, levels=c("12am", "1am", "2am", "3am",
                                                          "4am", "5am", "6am", "7am",
                                                          "8am", "9am", "10am", "11am",
                                                          "12pm", "1pm", "2pm", "3pm",
                                                          "4pm", "5pm", "6pm", "7pm",
                                                          "8pm", "9pm", "10pm", "11pm", "Unknown"))


# Subset the data to Only Wisconsin
wisconsin <- subset(accident, accident$STATENAME == "Wisconsin")

#Strip the extra number from the county name
wisconsin$COUNTYNAME <- sub(" \\(\\d+\\)$", "", wisconsin$COUNTYNAME)


#Import Wisconsin Map Data frame
mapData <- st_read("www/County_Boundaries_24K/County_Boundaries_24K.shp", stringsAsFactors = FALSE)
mapData$COUNTY_NAM <- toupper(mapData$COUNTY_NAM)
colnames(mapData)[colnames(mapData) == "COUNTY_NAM"] <- "COUNTYNAME"

# Merge the data frames
county_summary <- wisconsin %>%
  group_by(COUNTYNAME) %>%
  summarize(Fatalities = n())


# Add a new row to the data frame for Vilas County (because it has 0 fatalities)
new_row <- data.frame(COUNTYNAME = "VILAS", Fatalities = 0)
county_summary <- rbind(county_summary, new_row)
map_df <- merge(mapData, county_summary, by = "COUNTYNAME")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Wisconsin Traffic Fatalities in 2021"),
  #Right Justify the selection bar
  div(
    style = "display: flex; justify-content: right;",  # Centers the content inside the div
    selectInput("county",
                "Choose a County",
                choices = c("Whole State", sort(unique(wisconsin$COUNTYNAME))))
  ),
  
  # Divide the display into two "Cards"
  layout_column_wrap(
    # Display the state
    card(
      tmapOutput("StatePlot")
    ),
    
    # Display the plots
    card(
      girafeOutput("interactivePlot")
    )
  )
)


# Define server logic for output
server <- function(input, output) {
  
  # Display the State
  output$StatePlot <- renderTmap({
    tmap_mode("view")
    tm_shape(map_df) +
      tm_polygons("Fatalities", id = "COUNTYNAME",
                  style = "cont")
  })
    
  # Display the interactive Girafe Plots
  output$interactivePlot <- renderGirafe({
    # Condition on whether "Whole State" is selected
    if(input$county == "Whole State")
      filtered <- wisconsin
    else
      filtered <- subset(wisconsin, wisconsin$COUNTYNAME == input$county)
    
    # Repeatable tooltip text block 
    tooltip_text <- paste(
      "Hour:", filtered$HOURNAME, "<br>",
      "Road Type:", filtered$ROUTENAME, "<br>",
      "Accident Type:", filtered$MAN_COLLNAME, "<br>",
      "Day:", filtered$DAY_WEEKNAME
    )
    
    # Bar plot of time of day
    p1 <- ggplot(filtered) + 
      geom_bar_interactive(
        aes(
          x = HOURNAME, 
          tooltip = tooltip_text, 
          data_id = paste(ST_CASE, HOURNAME, ROUTENAME, MAN_COLLNAME)
        ), 
        fill = 'yellow', col = "gold"
      ) +
      labs(x = "Hour of the Day") +
      theme(axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Bar plot of type of highway
    p2 <- ggplot(filtered) + 
      geom_bar_interactive(
        aes(
          x = ROUTENAME, 
          tooltip = tooltip_text, 
          data_id = paste(ST_CASE, HOURNAME, ROUTENAME, MAN_COLLNAME)
        ), 
        fill = 'darkgray', col = "black"
      ) +
      labs(x = "Type of Road") +
      theme(axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Bar plot of type of accident
    p3 <- ggplot(filtered) + 
      geom_bar_interactive(
        aes(
          x = MAN_COLLNAME, 
          tooltip = tooltip_text, 
          data_id = paste(ST_CASE, HOURNAME, ROUTENAME, MAN_COLLNAME)
        ), 
        fill = 'coral', col = "darkred"
      ) +
      labs(x = "Type of Accident") +
      theme(axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Bar plot of day of the week
    p4 <- ggplot(filtered) + 
      geom_bar_interactive(
        aes(
          x = DAY_WEEKNAME, 
          tooltip = tooltip_text, 
          data_id = paste(ST_CASE, HOURNAME, ROUTENAME, MAN_COLLNAME)
        ), 
        fill = 'skyblue', col = "navy"
      ) +
      labs(x = "Day of the Week") +
      theme(axis.ticks.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Combine plots
    combined <- p1 / (p2 + p3) / p4
    girafe(
      ggobj = combined, 
      options = list(
        opts_hover(css = "fill:blue;stroke:blue;stroke-width:2;")
      )
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
