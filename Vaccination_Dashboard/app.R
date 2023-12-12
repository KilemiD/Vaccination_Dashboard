library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)
library(purrr)
library(highcharter)
library(dplyr)
library(scales)
library(tidyverse)
library(countrycode)
library(ggimage)
library(tidyr)
library(DBI)
library(RMySQL)
library(plotly)

PARS <- list(
  debug = FALSE,
  classcol = "col-lg-offset-1 col-lg-10 col-md-offset-0 col-md-12 col-sm-offset-0 col-sm-12",
  sparkline_color = "#333333",
  font = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"'
) 

options(
  highcharter.google_fonts = FALSE,
  highcharter.debug = PARS$debug,
  # shiny.launch.browser = PARS$debug,
  highcharter.theme = 
    hc_theme_smpl(
      title = list(style = list(fontSize = "1.2em", fontFamily = PARS$font)),
      subtitle = list(style = list(fontFamily = PARS$font, fontSize = "0.95em")),
      chart = list(
        backgroundColor = "transparent",
        style = list(fontFamily = PARS$font, fontSize = "1.0em")
      ),
      plotOptions = list(
        series = list(
          dataLabels = list(color = "#222d32", style = list(fontWeight = "normal", textShadow = FALSE, textOutline = FALSE)),
          animation = list(duration = 3000)
        )
      ),
      legend = list(
        itemStyle =  list(
          fontWeight = "normal"
        )
      )
    )
)

dropdownButtonp <- purrr::partial(
  dropdownButton,
  status = "customstatus",
  size = "sm",
  right = TRUE,
  status = "info",
  width = "400px",
  inline = TRUE,
)
#spark

#theme
hc_theme_sparkline_vb <- function(...) {
  
  theme <- list(
    chart = list(
      backgroundColor = NULL,
      margins = c(0, 0, 0, 0),
      spacingTop = 0,
      spacingRight = 0,
      spacingBottom = 0,
      spacingLeft = 0,
      plotBorderWidth = 0,
      borderWidth = 0,
      style = list(overflow = "visible")
    ),
    xAxis = list(
      visible = F, 
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    yAxis = list(
      visible = F,
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    tooltip = list(
      outside = FALSE,
      shadow = FALSE,
      borderColor = "transparent",
      botderWidth = 0,
      backgroundColor = "transparent",
      style = list(textOutline = "5px white")
    ),
    plotOptions = list(
      series = list(
        marker = list(enabled = FALSE),
        lineWidth = 2,
        shadow = FALSE,
        fillOpacity = 0.0,
        color = "#FFFFFFBF",
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
          stops = list(
            list(0.00, "#FFFFFF00"),
            list(0.50, "#FFFFFF7F"),
            list(1.00, "#FFFFFFFF")
          )
        )
      )
    ),
    credits = list(
      enabled = FALSE,
      text = ""
    )
  )
  theme <- structure(theme, class = "hc_theme")
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  
  theme
}

valueBoxSpark <- function(value, subtitle, icon = NULL, color = "aqua", 
                          width = 4, href = NULL, spark = NULL, height_spark = "150px",minititle = NULL) {
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon)) 
    shinydashboard:::tagAssert(icon, type = "i")
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      if(!is.null(minititle)) tags$small(minititle),
      h3(value),
      # tags$span(style = paste0("height:", height_spark), hc_size(spark, height = "100vh")),
      tags$span(hc_size(spark, height = height_spark)),
      if (!is.null(subtitle)) p(subtitle)
    ),
    if (!is.null(icon)) div(class = "icon-large", icon)
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(class = if (!is.null(width)) 
    paste0("col-sm-", width), boxContent)
}

#reading the data
vaccine_data <- readRDS("HCWDashboard.rds")

# ui
ui <- navbarPage(
  title = tags$div(HTML('HCW VACCINATIONS DASHBOARD')),
  #title = "KE: INSURANCE SECTOR",
  theme = "cerulean",
  #inverse = TRUE,
  tags$style(HTML('.navbar { background-color: #00b9e3; }')),
  selected = "Overview",
  ###### Here : insert shinydashboard dependencies ######
  header = tagList(
    useShinydashboard()
  ),
  #######################################################
  tabPanel("Overview",
           fluidRow(    
             column(width=12,
                    # Metrics Filters
                    fluidRow(tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; border-top: none; -moz-box-shadow: none;box-shadow: none;}'))),
                             column(width =4,
                                    plotlyOutput("target",height = "150px")
                             ),
                             column(
                               width=8,
                               tags$head(tags$style(HTML(".small-box {height: 150px, width: 50px}"))),
                               valueBoxOutput("total_vaccinations"),
                               valueBoxOutput("today_vaccinations"),
                               valueBoxOutput("female_vaccinations")
                               )
                               
                             )
                    )
                    
             )
           )
  )



#server
server <- function(input, output) {
  
  #output for total vaccinations against target
  output$target=renderPlotly({
    
    lbl <- vaccine_data %>% 
      nrow()
    
    target=7000
    
    # Calculate percentage
    percentage <- (lbl / target) * 100
    # Create a gauge chart
    fig <- plot_ly(
      type = "indicator",
      #mode = "gauge+number",
      mode = "gauge+number", #+delta
      #delta = list(reference = 4500),
      value = ~lbl,
      title = "",
      gauge = list(
        axis = list(range = list(0, target)),
        bar = list(color = "darkgreen"),
        steps = list(
          list(range = c(0, target), color = "lightgray"),
          list(range = c(0, lbl), color = "darkgreen")
        ),
        threshold = list(
          line = list(color = "red", width = 6),
          thickness = 0.75,
          value = ~lbl
        )
      )
    )
    
    # Add annotations for percentage and numeric value
    fig <- fig %>% layout(
      annotations = list(
        text = paste(round(percentage, 1), "%"),
        x = 0.5,
        y = -0.05,  # Adjust the y coordinate to move the text below the value
        showarrow = F
      )
    )
    
    # Customize layout
    fig <- fig %>% layout(margin = list(b = 5, t = 15))
    
    # Show the chart
    fig
    
  })
  
  #output for total vaccinations
  output$total_vaccinations=renderValueBox({
    #rendering value box
    lbl <- vaccine_data %>% 
      nrow() %>%
      format(big.mark = ",", digits = 0,scientific=FALSE)
    
    valueBox(lbl,
             "Total Vaccinations",icon("hourglass-half"),color="olive"
    )
  })
  
  
  #output for today vaccinations
  output$today_vaccinations=renderValueBox({
    #rendering value box
    lbl <- vaccine_data %>%
      group_by(date)%>%
      count() %>% 
      pull(n) %>% 
      last() %>% 
      format(big.mark = ",", digits = 0,scientific=FALSE)
    
    valueBox(lbl,
             "Today Vaccinations",icon("hourglass-half"),color="olive"
    )
  })
  
  #output for ladies vaccinations
  output$female_vaccinations=renderValueBox({
    #rendering value box
    lbl <- vaccine_data %>%
      filter(sex=="Female") %>% 
      nrow() %>% 
      format(big.mark = ",", digits = 0,scientific=FALSE)
    
    valueBox(lbl,
             "Female Vaccinations",icon("hourglass-half"),color="olive"
    )
  })
  
}

shinyApp(ui = ui, server = server)