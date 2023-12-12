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




