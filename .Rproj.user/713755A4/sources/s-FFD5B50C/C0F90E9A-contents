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
    sports_data_leaflet <- sports %>% filter(縣市鄉鎮 == city_county)
    library_data_leaflet <- library_data %>% filter(縣市鄉鎮 == city_county)
  })
  output$distPlot <-
    shiny::renderPlot({{ ggplot(data = faithful) +
      geom_histogram(aes(x = eruptions), bins = as.numeric(input$bins)) }})
  output$desctextcity <-
    renderText({
      paste0(
        input$selectcity1, "總計有", school_109_bycity %>% filter(縣市名稱 == input$selectcity1) %>% select(學校數總計) %>% unlist() %>% unname() %>% unique(), "間偏遠地區學校（偏校），且其偏校總數排名為全台縣市第",
        school_109_bycity %>% filter(縣市名稱 == input$selectcity1) %>% select(排名) %>% unlist() %>% unname() %>% unique(), "名。"
      )
    })
  output$desctextcity2 <-
    renderText({
      paste0("偏遠學校數：", if (length(school_109_bycity %>% filter(縣市名稱 == input$selectcity1 & 地區屬性 == "偏遠") %>% select(學校數) %>% unlist() %>% unname()) == 0) {
        0
      } else {
        school_109_bycity %>%
          filter(縣市名稱 == input$selectcity1 & 地區屬性 == "偏遠") %>%
          select(學校數) %>%
          unlist() %>%
          unname()
      }, "\n", "特偏學校數：", if (length(school_109_bycity %>% filter(縣市名稱 == input$selectcity1 & 地區屬性 == "特偏") %>% select(學校數) %>% unlist() %>% unname()) == 0) {
        0
      } else {
        school_109_bycity %>%
          filter(縣市名稱 == input$selectcity1 & 地區屬性 == "特偏") %>%
          select(學校數) %>%
          unlist() %>%
          unname()
      }, "\n", "極偏學校數：", if (length(school_109_bycity %>% filter(縣市名稱 == input$selectcity1 & 地區屬性 == "極偏") %>% select(學校數) %>% unlist() %>% unname()) == 0) {
        0
      } else {
        school_109_bycity %>%
          filter(縣市名稱 == input$selectcity1 & 地區屬性 == "極偏") %>%
          select(學校數) %>%
          unlist() %>%
          unname()
      })
    })
  output$citymaptitle <-
    renderText({
      paste0(input$selectcity1, "偏遠地區學校位置")
    })
  output$plotlycity <-
    renderPlotly({
      school_109_bycity_plotly
    })
  icons1 <-
    reactive({
      awesomeIcons(icon = "ios-close", iconColor = "black", library = "ion")
    })
  icons2 <-
    reactive({
      awesomeIcons(icon = "ios-close", iconColor = "black", library = "ion", markerColor = "red")
    })
  icons3 <-
    reactive({
      awesomeIcons(icon = "ios-close", iconColor = "black", library = "ion", markerColor = "green")
    })
  output$leafletplotcity <-
    renderLeaflet({{ leaflet() %>%
      addTiles() %>%
      setView(lng = head(school_109 %>% filter(縣市名稱 == input$selectcity1) %>% select(經度), 1) %>% unlist() %>% unname(), lat = head(school_109 %>% filter(縣市名稱 ==
        input$selectcity1) %>% select(緯度), 1) %>% unlist() %>% unname(), zoom = 9) %>%
      addAwesomeMarkers(lng = school_109 %>% filter(縣市名稱 == input$selectcity1 & 地區屬性 == "偏遠") %>% select(經度) %>% unlist() %>%
        unname(), lat = school_109 %>% filter(縣市名稱 == input$selectcity1 & 地區屬性 == "偏遠") %>% select(緯度) %>% unlist() %>% unname(), popup = paste0(school_109 %>% filter(縣市名稱 == input$selectcity1 & 地區屬性 ==
        "偏遠") %>% select(學校名稱) %>% unlist() %>% unname(), "：偏遠"), icon = icons1()) %>%
      addAwesomeMarkers(lng = school_109 %>% filter(縣市名稱 == input$selectcity1 & 地區屬性 == "極偏") %>% select(經度) %>% unlist() %>%
        unname(), lat = school_109 %>% filter(縣市名稱 == input$selectcity1 & 地區屬性 == "極偏") %>% select(緯度) %>% unlist() %>% unname(), popup = paste0(school_109 %>% filter(縣市名稱 == input$selectcity1 & 地區屬性 ==
        "極偏") %>% select(學校名稱) %>% unlist() %>% unname(), "：極偏"), icon = icons2()) %>%
      addAwesomeMarkers(lng = school_109 %>% filter(縣市名稱 == input$selectcity1 & 地區屬性 == "特偏") %>% select(經度) %>% unlist() %>%
        unname(), lat = school_109 %>% filter(縣市名稱 == input$selectcity1 & 地區屬性 == "特偏") %>% select(緯度) %>% unlist() %>% unname(), popup = paste0(school_109 %>% filter(縣市名稱 == input$selectcity1 & 地區屬性 ==
        "特偏") %>% select(學校名稱) %>% unlist() %>% unname(), "：特偏"), icon = icons3()) }})
  output$desctext <-
    renderText({
      paste0(input$selectcity, "為分級上的", school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(地區屬性) %>% unlist() %>% unname(), "學校，位處", school_109 %>% filter(縣市學校名稱 ==
        input$selectcity) %>% select(縣市鄉鎮) %>% unlist() %>% unname(), if (is.na(school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(原住民鄉分類) %>% unlist() %>% unname())) {
        "。"
      } else {
        paste0("，且其為原住民鄉分類上的", school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(原住民鄉分類) %>% unlist() %>% unname(), "。")
      })
    })
  output$desctext2 <-
    renderText({
      paste0("學生總計：", school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(學生數總計) %>% unlist() %>% unname(), "\n", "班級數總計：", school_109 %>% filter(縣市學校名稱 == input$selectcity) %>%
        select(班級數) %>% unlist() %>% unname(), if (is.na(school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(原住民學生比率) %>% unlist() %>% unname())) {
        "\n"
      } else {
        paste0("\n", "原住民學生比率：", school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(原住民學生比率) %>% unlist() %>% unname(), "\n")
      }, "學校網址：", school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(網址) %>% unlist() %>% unname())
    })
  output$maptitle <-
    renderText({
      paste0(input$selectcity, "—", school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(地區屬性) %>% unlist() %>% unname(), "學校位置")
    })
  output$allplotlytitle <-
    renderText({
      paste0(input$selectcity, "—", "所屬地區指標")
    })
  output$leafletPlot <-
    renderLeaflet({{ leaflet() %>%
      addTiles() %>%
      setView(lng = school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(經度) %>% unlist() %>% unname(), lat = school_109 %>% filter(縣市學校名稱 == input$selectcity) %>%
        select(緯度) %>% unlist() %>% unname(), zoom = 11) %>%
      addMarkers(lng = school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(經度) %>% unlist() %>% unname(), lat = school_109 %>% filter(
        縣市學校名稱 == input$selectcity
      ) %>% select(緯度) %>% unlist() %>% unname(), popup = paste0(input$selectcity, ":", school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(地區屬性) %>% unlist() %>%
        unname())) %>%
      addCircleMarkers(
        data = sports %>% filter(縣市鄉鎮 == school_109 %>%
          filter(縣市學校名稱 == input$selectcity) %>%
          select(縣市鄉鎮) %>%
          unlist() %>%
          unname()), radius = 8, stroke = T,
        fillOpacity = 0.7, color = "#4d9221", popup = sports %>% filter(縣市鄉鎮 == school_109 %>%
          filter(縣市學校名稱 == input$selectcity) %>%
          select(縣市鄉鎮) %>%
          unlist() %>%
          unname()) %>% select(場館名稱) %>% unlist() %>%
          unname()
      ) %>%
      addCircleMarkers(
        data = library_data %>% filter(縣市鄉鎮 == school_109 %>%
          filter(縣市學校名稱 == input$selectcity) %>%
          select(縣市鄉鎮) %>%
          unlist() %>%
          unname()), radius = 8, stroke = T,
        fillOpacity = 0.7, color = "#fdae61", popup = library_data %>% filter(縣市鄉鎮 == school_109 %>%
          filter(縣市學校名稱 == input$selectcity) %>%
          select(縣市鄉鎮) %>%
          unlist() %>%
          unname()) %>% select(圖書館名稱) %>%
          unlist() %>% unname()
      ) }})
  output$plotly1 <-
    renderPlotly({
      culture_lib_spt_plot_1_plotly %>%
        add_trace(
          x = 1, y = school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(鄉鎮圖書館體育館總和) %>% unlist() %>% unname(), type = "scatter", mode = "markers",
          name = "該校所屬鄉鎮市區", marker = list(color = "red")
        ) %>%
        add_trace(
          x = 2, y = school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(鄉鎮圖書館體育館總和) %>% unlist() %>% unname(), type = "scatter", mode = "markers",
          name = "該校所屬鄉鎮市區", marker = list(color = "red")
        )
    })
  output$plotly2 <-
    renderPlotly({
      life_density10909_plot_2_plotly %>%
        add_trace(
          x = 1, y = school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(村里人口密度) %>% unlist() %>% unname(), type = "scatter", mode = "markers",
          name = "該校所屬村里", marker = list(color = "red")
        ) %>%
        add_trace(
          x = 2, y = school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(村里人口密度) %>% unlist() %>% unname(), type = "scatter", mode = "markers",
          name = "該校所屬村里", marker = list(color = "red")
        )
    })
  output$plotly3 <-
    renderPlotly({
      life_conv_finance_plot_3_plotly %>%
        add_trace(
          x = 1, y = school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(鄉鎮便利金融) %>% unlist() %>% unname(), type = "scatter", mode = "markers",
          name = "該校所屬鄉鎮", marker = list(color = "red")
        ) %>%
        add_trace(
          x = 2, y = school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(鄉鎮便利金融) %>% unlist() %>% unname(), type = "scatter", mode = "markers",
          name = "該校所屬鄉鎮", marker = list(color = "red")
        )
    })
  output$plotly4 <-
    renderPlotly({
      life_hospital_plot_4_plotly %>%
        add_trace(
          x = 1, y = school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(醫療院所家數) %>% unlist() %>% unname(), type = "scatter", mode = "markers",
          name = "該校所屬鄉鎮", marker = list(color = "red")
        ) %>%
        add_trace(
          x = 2, y = school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(醫療院所家數) %>% unlist() %>% unname(), type = "scatter", mode = "markers",
          name = "該校所屬鄉鎮", marker = list(color = "red")
        )
    })
  output$plotly5 <-
    renderPlotly({
      digital_plot_5_plotly %>%
        add_trace(
          x = 1, y = school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(鄉鎮基地台) %>% unlist() %>% unname(), type = "scatter", mode = "markers",
          name = "該校所屬鄉鎮", marker = list(color = "red")
        ) %>%
        add_trace(
          x = 2, y = school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(鄉鎮基地台) %>% unlist() %>% unname(), type = "scatter", mode = "markers",
          name = "該校所屬鄉鎮", marker = list(color = "red")
        )
    })
  output$plotly6 <-
    renderPlotly({
      econ_aging_plot_6_plotly %>%
        add_trace(
          x = 1, y = school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(村里老化指數) %>% unlist() %>% unname(), type = "scatter", mode = "markers",
          name = "該校所屬村里", marker = list(color = "red")
        ) %>%
        add_trace(
          x = 2, y = school_109 %>% filter(縣市學校名稱 == input$selectcity) %>% select(村里老化指數) %>% unlist() %>% unname(), type = "scatter", mode = "markers",
          name = "該校所屬村里", marker = list(color = "red")
        )
    })
}

shinyApp(
  ui = shiny::htmlTemplate("www/index.html"),
  server
)
