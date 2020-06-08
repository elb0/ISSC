### Shiny Dashboard: COVID-19 Rolling Averages ###
# Created by: Alex Stringer
# For: Statistical Sciences summer school
# Inspired by: https://art-bd.shinyapps.io/covid19canada/

# First, install shiny and shiny dashboards
# install.packages(c("shiny","shinydashboard"))

# I'm loading my secret functions. I'll share these after the workshop. They
# form "answers" to "exercises" I will assign throughout the workshop.
source("/Users/alexstringer/teaching/s20/summerschool/shinydashboardworkshop/secretfunctions.R")

# Load them into memory
library(shiny)
library(shinydashboard)
# Load the tidyverse, for data manipulation and plotting
library(tidyverse)


# Get the datasets for this project
# They are the ones underpinning the above-referenced dashboard
# They are in a git repo here: https://github.com/ishaberry/Covid19Canada
# (this person is at U of T!)
# You could clone the repo to your computer and load them that way, that's how I'd
# do it... but it's easier to show just downloading from the interweb
cases <- readr::read_csv(
  file = "https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/cases.csv",
  col_names = TRUE,
  col_types = c("cccccccccccccccc")
) %>%
  dplyr::select(
    case_id,
    province,
    date_report
  ) %>%
  mutate(date_report = lubridate::dmy(date_report))

# glimpse(cases)

# What's going on with those column types? Let's go see...
# Run this line by line to see what each step is doing...
# Here, I'll do another one:
testing <- readr::read_csv(
  file = "https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/testing_cumulative.csv",
  col_names = TRUE,
  col_types = c("ccnc")
) %>%
  dplyr::select(
    date_testing,
    province,
    cumulative_testing
  ) %>%
  mutate(date_testing = lubridate::dmy(date_testing))

# glimpse(testing)

# EXERCISE: get the other two we need:
# recovered: https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/recovered_cumulative.csv
# mortality: https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/mortality.csv

# glimpse(recovered)
# glimpse(mortality)

# We want daily counts. Currently we have four datasets summarized in two different ways
# The mortality and cases are raw, with one record per event. We want to count by day and province
# The recovered and testing datasets are summarized, with each record representing the cumulative
# sum to that day. We want to difference these.
# Let's do that then merge so we have one dataset with all the data in it that we need.

# I'll do one of each type then you do the other one.
# Here's the cases:
cases_daily <- cases %>%
  group_by(province,date_report) %>%
  summarize(cases = n())

# You create the dataset "mortality_daily" that looks like this:
# glimpse(mortality_daily)

# Here's the tested:
testing_daily <- testing %>%
  arrange(province,date_testing) %>%
  group_by(province) %>% # EXERCISE: figure out WHY you need this step (very subtle!)
  mutate(tested = cumulative_testing - lag(cumulative_testing,1)) %>%
  filter(!is.na(tested)) # Remove the first row, which does not have a value

# You create the dataset "recovered_daily" that looks like this:
# glimpse(recovered_daily)

# Now let's join them to get one dataset with the daily cases, deaths, recoveries and tests
dailycovidinfo <- cases_daily %>%
  inner_join(testing_daily,by = c("province" = "province","date_report" = "date_testing")) %>%
  inner_join(mortality_daily,by = c("province" = "province","date_report" = "date_death_report")) %>%
  inner_join(recovered_daily,by = c("province" = "province","date_report" = "date_recovered")) %>%
  dplyr::select(province,date_report,cases,tested,deaths,recovered)

## Actually build the app ##
# Now that our data exists we can build the dashboard
# As you learned last week, every shiny app consists of a UI and Server function
# The former controls what appears on the page. The latter computes stuff to be displayed.
# So, we'll build a server function that creates the plots we want, then a UI function
# that displays them on the page.
# The UI function will use slightly different functions than for a regular shiny app--
# which is what turns the thing into a dashboard.

# First let's do the UI. We will create the header, sidebar and body as R objects 
# using the dashboardHeader() dashboardSidebar() dashboardBody() functions.
# Then we'll combine them into a user interface.

body <- dashboardBody(
  # Have a row for the controls of how many days to average over and what provinces to see
  fluidRow(
    # Have a box for the user input, number of days to average over
    box(title = "How many days to average over",
        sliderInput("casesnumdays","Number of days",1,21,7)),
    box(title = "Provinces to show",
        checkboxGroupInput("provinces","Provinces",sort(unique(dailycovidinfo$province)),sort(unique(dailycovidinfo$province))))
  ),
  # Have a row for a plot of the daily cases
  fluidRow(
    # Have a box for the plot
    box(plotOutput("cases",height = 500,width = 600),width = 12,height = NULL)
  )
)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  body
)

server <- function(input, output) { 
  output$cases <- renderPlot({
    dailycovidinfo %>%
    filter(.data[["province"]] %in% input$provinces) %>%
    ggplot(aes(x = date_report,group = province)) +
    theme_minimal() +
    geom_point(aes(y = cases,fill = province),pch = 21,alpha = .5,colour = "black") +
    geom_line(aes(y = stats::filter(cases,rep(1/input$casesnumdays,input$casesnumdays),sides = 1),colour = province),size = 1) +
    labs(title = "Daily case counts by province",
         subtitle = paste0(input$casesnumdays,"-day rolling average"),
         x = "Date",
         y = "Counts (points) and rolling average (lines)") +
    coord_trans(y = "log2") +
    scale_y_continuous(breaks = 2^(1:11))
  })
  # Let's add one for daily deaths, together!
  # Then you'll do ones yourself for daily tested and recovered, and (I suggest)
  # daily "active cases" (cumulative [cases - deaths - recovered])
}

shinyApp(ui, server)

# EXERCISE: the daily number of cases isn't super informative. Add a plot
# to the app that shows the daily deaths. Include a slider for the width of
# the moving average window, 
