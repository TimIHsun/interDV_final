server <- function(input, output, session) {
  library(shiny)
  library(ggplot2)
  library(dplyr)
  library(shiny)
  library(htmltools)
  library(econR)
  library(ggplot2)
  library(dplyr)
  library(readxl)
  library(stringr)
  library(sysfonts)
  library(showtext)
  library(tidyselect)
  library(purrr)
  library(plotly)
  library(readxl)
  library(readr)
  library(patchwork)
  library(leaflet)
  load("support/data/finaldata.RData")
  reactive({
    choosen_school_data <- school_109 %>% filter(縣市學校名稱 == input$selectcity)

    county_name <- choosen_school_data %>%
      select(縣市名稱) %>%
      unlist() %>%
      unname()

    school_name <- choosen_school_data %>%
      select(學校名稱) %>%
      unlist() %>%
      unname()

    choosen_school_lng <- choosen_school_data %>%
      select(經度) %>%
      unlist() %>%
      unname()

    choosen_school_lat <- choosen_school_data %>%
      select(緯度) %>%
      unlist() %>%
      unname()

    city_county <- choosen_school_data %>%
      select(縣市鄉鎮) %>%
      unlist() %>%
      unname()

    sports %>% filter(縣市鄉鎮 == city_county) -> sports_data_leaflet

    library_data %>% filter(縣市鄉鎮 == city_county) -> library_data_leaflet
  })
  output$distPlot <-
    shiny::renderPlot({{ ggplot(data = faithful) +
      geom_histogram(
        aes(x = eruptions),
        bins = as.numeric(input$bins)
      ) }})
  output$leafletPlot <-
    renderLeaflet({{ leaflet() %>%
      addTiles() %>%
      setView(
        lng = school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(經度) %>% unlist() %>% unname(),
        lat = school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(緯度) %>% unlist() %>% unname(),
        zoom = 12
      ) %>%
      addMarkers(
        lng = school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(經度) %>% unlist() %>% unname(),
        lat = school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(緯度) %>% unlist() %>% unname(),
        popup = input$selectcity
      ) }})
}
