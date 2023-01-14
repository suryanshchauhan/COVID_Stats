library(shiny)
library(plotly)
library(tidyverse)
#install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")
library("rnaturalearth")
#install.packages("rnaturalearthhires",repos = "http://packages.ropensci.org",type = "source")
library("rnaturalearthdata")

my_map_theme <- function(){
    theme(panel.background=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank())
}

to_label <- function(theinput) {
    if (as.character(theinput) == "total_cases_per_million"){
        return("Total Cases per Million")
    }else if (as.character(theinput) == "new_cases_per_million"){
        return("New Cases per Million")
    }else if (as.character(theinput) == "total_deaths_per_million"){
        return("Total Deaths per Million")
    }else{
        return("Total Tests per Thousand")
    }
}

rate_switch <- function(theinput){
    if(as.character(theinput) == "total_cases_per_million" 
       || as.character(theinput) == "new_cases_per_million"
       || as.character(theinput) == "total_deaths_per_million"){
        
        return("per million")
        
    }else{
        
        return("per thousand")
        
    }
}
### ---------------- ###

theworld <- ne_countries(scale = "medium", returnclass = "sf")

statsforcovid <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv",
                       col_types = cols(date = col_date(format = "%Y-%m-%d"), 
                                        total_tests_per_thousand = 
                                            col_double()))

mapofcovid <- function(statistics, thedate){
    my_stat <- enquo(statistics)
    covid_eachday <- statsforcovid %>%
        filter(date==thedate) %>%
        select(location, iso_code, mystatistic = !!my_stat)
    mapdata <- left_join(theworld, covid_eachday, by=c("iso_a3"="iso_code"))
    m <- mapdata %>%
        mutate(text = paste("<b>",location,"</b>\nRate:", mystatistic, 
                            rate_switch(statistics))) %>%
        ggplot() +
        geom_sf(aes(fill=mystatistic, text=text), color="black")+
        scale_fill_continuous(to_label(statistics), low="white",high="red") +
        my_map_theme()
    ggplotly(m, tooltip="text") %>%
        style(hoveron="fill")
    
}  


ui <- fluidPage(
    
    
    titlePanel("COVID Stats by Country"),
    
     
    sidebarLayout(
        sidebarPanel(
            selectInput("mystatistic",
                        "Statistic you want to View:",
                        choices = list("Total Cases Per Million"=
                                           "total_cases_per_million",
                                       "New Cases Per Million"=
                                           "new_cases_per_million",
                                       "Total Deaths Per Million"=
                                           "total_deaths_per_million",
                                       "Total Tests per Thousand"=
                                           "total_tests_per_thousand"),
                        selected = "total_cases_per_million"),
            sliderInput("thedate",
                        "Select a date:",
                        min = as.Date("2019-12-31", "%Y-%m-%d"),
                        max = Sys.Date(),
                        value = as.Date("2021-07-15"),
                        timeFormat="%Y-%m-%d")
        ),
        
   
        mainPanel(
            plotlyOutput("map")
        )
    )
)


server <- function(input, output) {
    output$TitleText <- renderText(paste(input$mystatistic, "On", input$thedate))
    output$SubtitleText <- renderText(paste("The world map shows the rate of", 
                                            input$mystatistic,
                                            "on this given date."))
    output$map <- renderPlotly({
        mapofcovid(input$mystatistic, input$thedate)
    })
}

shinyApp(ui = ui, server = server)

