getwd()
vaccine_data <- readRDS("C:/Users/John/Documents/GitHub/Vaccination_Dashboard/HCWDashboard.rds")
str(vaccine_data)
head(vaccine_data)
sum(is.na(vaccine_data))
View(vaccine_data)
table(vaccine_data$team) #25 teams
table(vaccine_data$subcounty) #13 subcounties #a map of kakamega here will be best #use the map with Kinyanjui
table(vaccine_data$sex) #use donut chart in dashboard
summary(vaccine_data$age) #mean age of 37
table(vaccine_data$cadre) #for this include a button that can search the people
table(vaccine_data$risk_level) #model this in terms of risk level as colors (red,orange,green) #piechart/donut
summary(vaccine_data$date) #model date as daily trend #preferably a plotly output
#here as well include inputs, that allow for comparisons/or trend by each team, double filters.
#use observe events to change subcounty fills in the map of kakamega.

library(dplyr)
lbl <- vaccine_data %>% 
  nrow()

lbl <- vaccine_data %>% 
  nrow() %>%
  format(big.mark = ",", digits = 0,scientific=FALSE)


nrow(vaccine_data)
library(fontawesome)

lbl <- vaccine_data %>%
  group_by(date)%>%
  count() %>% 
  pull(n) %>% 
  last()

#But what are the Questions to be answered.
#Insight 1
#Filters to select specific teams & subcounties for data visualization
#I select team or subcounty, I see gender, date(trend), risk, cadre

#Insight 2
#Visualization of daily and cumulative counts of vaccinated HCWs.
#use value boxes here (for daily & cumulative)

#Insight 3
#Number and proportion of female HCWs
#here use the wiper chart

#Insight 4
#Risk level of vaccinated HCW.
#model risk levels as colors codes here.
#you can even introduce a table with colors representing subcounties/teams
#with their risk levels. use a donut chart as well here.

#Insight 5
#Number and percentage of vaccinated HCWs, categorized by cadre and ordered
#from the most common cadre.
#here you can include a chart that goes downwards (ggpplot/plotly),
#and outputs percentages in brackets and numbers. 
#use dplyr functions to summarize.

#Insight 6
#A login feature requiring a username and password for dashboard access.
#How do you implement this?

#Insight 7
#Design the dashboard to be user-friendly and effectively present the data.
#color blind colors. Readability. Visibility. Fonts. Interactivity.

#Insight 8
#Opportunity to incorporate and display additional relevant metrics from
# the data
#Here add maps, add analysis, add tabs?

#Insight 9
#Mapping of vaccinated HCWs by Subcounty in Kakamega County
#Add the map here, include counts inside, use a gradient.

#Insight 10
#The target for vaccination in the county is 7500. Include a chart
#that shows progress (number and percentage) to this goal.
#Here add a counter like that download button.

####
#Everytime data is refreshed that counter should change, that means
#it has to show the new readings (implement it as (n))


#draw a gauge chart
# Install and load the required package (if not already installed)
# install.packages("plotly")
library(plotly)

# Example data
target_value <- 7000
current_value <- 4600

# Create a sample dataset
data <- data.frame(
  Category = c("Achievement"),
  Value = c(current_value)
)


# Calculate percentage
percentage <- (current_value / target_value) * 100
# Create a gauge chart
fig <- plot_ly(
  data,
  type = "indicator",
  #mode = "gauge+number",
  mode = "gauge+number+delta",
  delta = list(reference = 4500),
  value = ~Value,
  title = "Vaccination Level",
  gauge = list(
    axis = list(range = list(0, target_value)),
    bar = list(color = "darkgreen"),
    steps = list(
      list(range = c(0, target_value), color = "lightgray"),
      list(range = c(0, current_value), color = "darkgreen")
    ),
    threshold = list(
      line = list(color = "red", width = 8),
      thickness = 0.75,
      value = ~Value
    )
  )
)


# Add annotations for percentage and numeric value
fig <- fig %>% layout(
  annotations = list(
    text = paste("Achieved ", current_value, " vaccinations representing ", round(percentage, 2), "%"),
    x = 0.5,
    y = 0.28,  # Adjust the y coordinate to move the text below the value
    showarrow = FALSE
  )
)

# Customize layout
fig <- fig %>% layout(margin = list(l = 50, r = 50, b = 50, t = 80))

# Show the chart
fig



library(plotly)

target_value <- 100
current_value <- 75

fig <- plot_ly(
  labels = c("Progress", "Remaining"),
  values = c(current_value, target_value - current_value),
  type = "pie",
  hole = 0.6,
  marker = list(colors = c("blue", "lightgray"))
)

fig

library(highcharter)
?hc_plotOptions()



#donut
data <- vaccine_data %>%
  filter(team=="Team1") %>% 
  group_by(risk_level) %>% 
  count() %>% 
  select(risk_level,n) #%>%
#group_by(risk_level) %>% 
#mutate(total=sum(n))

# Create a donut chart
donut <- plot_ly(data, labels = data$risk_level, values = data$n, 
                 type = "pie", hole = .4)

# Update the donut chart layout
donut <- donut %>%
  layout(
    title = "Risk Levels",
    xaxis = list(
      title = "Variable"
    ),
    yaxis = list(
      title = "Value"
    )
  )

# Return the donut chart
donut

sum(is.na(vaccine_data$risk_level))

#trying some maps
library(leaflet)
library(rgdal)
library(raster)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(htmltools)
library(DT)
library(RColorBrewer)
library(readr)
library(reshape2)
library("tidyverse")
library(ggfittext)

county_shp2<-readOGR("Vaccination_Dashboard/Kenya_Counties_(080719).shp")
plot(county_shp2)

voter2=read_csv("E:/2022/Shiny Dashboards/2022 Election Dashboard/voter2.csv")
#keep this for merging the two files
county_shp2@data = data.frame(county_shp2@data, 
                              voter2[match(county_shp2@data$NAME_1, 
                                         voter2$County),])

#drawing a sample map
leaflet(county_shp2) %>%
  setView(lng=37.9083,lat=0.1769,zoom = 6) %>%
  addPolygons(
    color = ~pal4(Total),
    smoothFactor = 0.5,
    weight = 2, opacity = 1.0,
    fillOpacity = 1.0,
    highlightOptions = highlightOptions(
      weight = 1,
      color = "blue",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = paste(
      "<strong>County:</strong>",county_shp2$NAME_1,
      "<br>",
      "<strong>Registered Voters:</strong>",county_shp2$Total
      
    ) %>% lapply(htmltools::HTML),
    labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                              padding = "3px 8px"), 
                                 textsize = "13px", direction = "auto"),
    
    popup = ~paste(
      "<strong>County:</strong>",NAME_1,
      "<br>",
      "<strong>Registered Voters:</strong>",Total
      
    )
    
  ) %>%
  addLegend(title = "Registered Voters",
            pal = pal4, values = county_shp2$Total, opacity = 1)


#color palettes
pal4<-colorBin("YlOrBr",subset_kakamega$vaccine_count)

#trying to subset a county
# Subset the shapefile to include only the desired county and its subcounties
subset_shp <- county_shp2[county_shp2$County == "Kakamega", ]

View(subset_shp)

#drawing a sample map
leaflet(subset_kakamega) %>%
  #setView(lng=38,lat=0.1769,zoom = 10) %>%
  addPolygons(
    color = ~pal4(vaccine_count),
    smoothFactor = 0.5,
    weight = 2, opacity = 1.0,
    fillOpacity = 1.0,
    highlightOptions = highlightOptions(
      weight = 1,
      color = "blue",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = paste(
      "<strong>Sub-County:</strong>",subset_kakamega$CONSTITUEN,
      "<br>",
      "<strong>Vaccinated:</strong>",subset_kakamega$vaccine_count
      
    ) %>% lapply(htmltools::HTML),
    labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                              padding = "3px 8px"), 
                                 textsize = "13px", direction = "auto"),
    
    popup = ~paste(
       "<strong>Sub-County:</strong>",CONSTITUEN,
       "<br>",
       "<strong>Vaccinated CHW:</strong>",vaccine_count
       
     )
    
  ) %>%
  addLegend(title = "Vaccinated CHW",
            pal = pal4, values = subset_kakamega$vaccine_count, opacity = 1)

#trying to load some constituency maps

constituency_shp=readOGR("E:/2022/Shiny Dashboards/Twitter R Shiny Dashboards/code for twitter analysis/health_maps/Constituency.shp")
plot(constituency_shp)                         
str(constituency_shp$COUNTY_NAM)  

#subset constituency
subset_kakamega <- constituency_shp[constituency_shp$COUNTY_NAM == "KAKAMEGA", ]
View(subset_kakamega)
plot(subset_kakamega)

?addPolygons

unique(subset_kakamega$CONSTITUEN)
unique(vaccine_data$subcounty_upper)

library(tidytext)
vaccine_data$subcounty_upper=toupper(vaccine_data$subcounty)

vaccine_data_subcounty=vaccine_data %>% 
  filter(!subcounty=="CHMT")
head(vaccine_data_subcounty)

#trying to merge the kakamega shape file witht the vaccine data
subset_kakamega@data = data.frame(subset_kakamega@data, 
                                  vaccine_data_subcounty[match(subset_kakamega@data$CONSTITUEN, 
                                                               vaccine_data_subcounty$subcounty_upper),])


View(subset_kakamega@data) %>% 
  group_by(team) %>% 
  count()

library(sf)
merged_data <- merge(vaccine_data_subcounty,subset_kakamega,
                     by.x="subcounty_upper",by.y="CONSTITUEN" )
st_write(merged_data, "kakamega.shp")

"C:/Users/John/Documents/GitHub/Vaccination_Dashboard"  


#summarizing by subcounty
library(dplyr)
df_sum <- vaccine_data_subcounty %>% 
  filter(subcounty_upper %in% subset_kakamega$CONSTITUEN) %>%
  group_by(subcounty_upper) %>%
  summarise(npscores = mean(age,na.rm=T))
df_sum


subset_kakamega$vaccine_count <- df_sum2$n[match(subset_kakamega$CONSTITUEN, df_sum2$subcounty_upper)]

#trying to count the people vaccinated
df_sum2 <- vaccine_data_subcounty %>%
  filter(subcounty_upper %in% subset_kakamega$CONSTITUEN) %>%
  group_by(subcounty_upper) %>%
  count()

View(df_sum2)

?addLegend

?addProviderTiles

vaccine_data %>% 
  filter(team=="Team1") %>% 
  group_by(cadre) %>%
  count() %>% 
  arrange(desc(n)) %>%
  ungroup() %>% 
  slice_head(n = 5)

vaccine_data %>% 
  filter(team=="Team1") %>% 
  filter(!is.na(age)) %>% 
  group_by(age) %>%
  count()  

summary(vaccine_data$age)

# Define the age group breaks and labels
breaks <- c(17, 24, 34, 44, 54, 65,100)
labels <- c("18-24", "25-34", "35-44", 
            "45-54","55-65","Over 65")

# Use dplyr functions to create age groups
vaccine_data <- vaccine_data %>%
  mutate(age_group = cut(age, breaks = breaks, 
                         labels = labels, include.lowest = TRUE))
table(age_breaks$age_group)
View(age_breaks)
age_breaks %>% 
  filter(age_group=="18-24")


?plot_ly
??tickmode


vaccine_data %>% 
  nrow()


vaccine_data <- vaccine_data %>%
  filter(age>17) %>% 
  filter(!is.na(age)) %>% 
  mutate(age_group = cut(age, breaks = breaks, 
                         labels = labels, include.lowest = TRUE))
sum(is.na(vaccine_data$age_group))

vaccine_data %>%
  filter(is.na(age_group))

?colorBin

vaccine_data %>% 
  select(date) %>% 
  summarise(count = n())


library(plotly)

# Sample data
vaccine_data <- data.frame(count = 4344)

# Target value
target <- 7000

# Calculate percentage
percentage <- (vaccine_data$count / target) * 100

# Create a gauge chart
fig <- plot_ly(
  type = "indicator",
  mode = "gauge+number",
  value = vaccine_data$count,
  title = "",
  gauge = list(
    axis = list(
      visible = FALSE  # Hide tick labels
    ),
    bar = list(color = "darkgreen"),
    steps = list(
      list(range = c(0, target), color = "lightgray"),
      list(range = c(0, vaccine_data$count), color = "darkgreen")
    )
  )
)

# Add annotations for percentage and numeric value
fig <- fig %>% layout(
  annotations = list(
    text = paste(round(percentage, 1), "%", "out of", target),
    x = 0.5,
    y = -0.05,  # Adjust the y coordinate to move the text below the value
    showarrow = FALSE
  )
)

fig


library(plotly)

# Sample data
vaccine_data <- data.frame(count = 4344)

# Target value
target <- 7000

# Calculate percentage
percentage <- (vaccine_data$count / target) * 100

# Create a gauge chart
fig <- plot_ly(
  type = "indicator",
  mode = "gauge+number",
  value = 4344,
  title = "",
  gauge = list(
    axis = list(
      visible = FALSE  # Hide tick labels
    ),
    bar = list(color = "darkgreen"),
    steps = list(
      list(range = c(0, target), color = "lightgray"),
      list(range = c(0, vaccine_data$count), color = "darkgreen")
    )
  )
)

# Add annotations for percentage and numeric value
fig <- fig %>% layout(
  annotations = list(
    text = paste(vaccine_data$count, "out of", target),
    x = 0.5,
    y = -0.05,  # Adjust the y coordinate to move the text below the value
    showarrow = FALSE
  )
)

fig



make_gauge <- function(x, n, color =  "#ef8d49", title = NULL) {
  data.frame(x = c(5 * cos(seq(-pi, 0, len = 100)), 
                   3 * cos(seq(0, -pi, len = 100)),
                   5 * cos(seq(-pi, -pi + pi * x/n, len = 100)),
                   3 * cos(seq( -pi + pi * x/n, -pi, len = 100))),
             y = c(5 * -sin(seq(-pi, 0, len = 100)), 
                   3 * -sin(seq(0, -pi, len = 100)),
                   5 * -sin(seq(-pi, -pi + pi * x/n, len = 100)),
                   3 * -sin(seq( -pi + pi * x/n, -pi, len = 100))),
             group = rep(c("off", "on"), each = 200)) |>
    ggplot(aes(x, y, fill = group)) +
    geom_polygon() +
    scale_fill_manual(values = c("#eceaea", color), guide = "none") +
    annotate("text", x = c(-4, 0, 4), y = c(-0.5, 0.6, -0.5),
             label = c(0, round(43, 1), 7000), size = c(12, 30, 12),
             colour = c("#c8c8c8", "#808080", "#c8c8c8"), 
             fontface = 2) +
    ggtitle(title) +
    coord_equal() +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 40, color = "#808080",
                                    vjust = 2, face = 2))
}

make_gauge(4344, 7000)
