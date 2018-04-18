# library(tidyverse, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(stringr, quietly = TRUE)
library(ggplot2)

np2014 <- read.csv("np2014_d1.csv")
np2017 <- read.csv("np2017_d1.csv")

# unique(np2014$origin)
# unique(np2014$race)
# The key for RACE is as follows:
# 0 = All races (codes 1 through 6)
# 1 = White alone
# 2 = Black alone
# 3 = AIAN alone
# 4 = Asian alone
# 5 = NHPI alone
# 6 = Two or More Races
# 7 = White alone or in combination
# 8 = Black alone or in combination
# 9 = AIAN alone or in combination
# 10 = Asian alone or in combination
# 11 = NHPI alone or in combination


make_long <- function(data) {
  data %>% 
  select_all(tolower) %>%
  filter(race == 0 | race == 1 | race == 8) %>% 
  filter(origin != 0) %>%
  select(-total_pop) %>% 
  gather(age, pop, -year, -sex, -origin, -race) %>% # vars removed from gathering are equivalent to group vars %>%
  mutate(age = as.numeric(str_extract(age, "[\\d]+$"))) %>%
  mutate(raceeth = 10*origin + race) %>%
  select(-origin, -race) %>%
  spread(raceeth, pop) %>%
  rename(hispanic = `20`) %>% select(-`21`, -`28`) %>%
  rename(nonhispanic = `10`)  %>%
  rename(white_nonhispanic = `11`) %>%
  rename(black_nonhispanic = `18`) %>%
  mutate(other_nonhispanic = nonhispanic - white_nonhispanic - black_nonhispanic) %>%
  mutate(all = hispanic + nonhispanic) %>%
  mutate(age = ordered(age))
}

pop2017 <- make_long(np2017)
pop2014 <- make_long(np2014)

# filters by age before calculating shares
make_shares <- function(data, min_age = 0, max_age = 100) {
  data %>% 
    filter(age >= min_age, age <= max_age) %>%
    group_by(year, sex) %>% 
    summarize_at(vars(nonhispanic:all), sum) %>% 
    mutate_at(vars(nonhispanic:all), funs(. * 100/all)) %>% 
    select(-nonhispanic, -white_nonhispanic, -all) %>% 
    gather(raceeth, share, -year, -sex)
}

ui <- fluidPage(
    
    titlePanel("Race-Ethnicty Shares in the New Census Population Projections"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        selectInput("sex",
                    "Choose sexes to display:",
                     choices = c("Both" = 0, "Men" = 1, "Women" = 2),
                     selected = c(0)),
        sliderInput("age",
                    "Choose age range to display:",
                    min = 0, max = 100, value = c(0, 100))
        ),
      
      mainPanel(
        plotOutput("plot")
      )
    )
  )
  
server <- function(input, output) {
  
  shares <- reactive({
    bind_rows(make_shares(pop2017, input$age[1], input$age[2]) %>% mutate(proj = "2017"),
              make_shares(pop2014, input$age[1], input$age[2]) %>% mutate(proj = "2014")) %>%
    mutate(proj = factor(proj, levels = c("2017", "2014")))
  })
  
  output$plot <- renderPlot({
    
    ggplot() +
      geom_line(data = shares() %>% filter(sex == input$sex),
                aes(x = year, y = share, color = raceeth, linetype = proj)) +
      scale_y_continuous(limits = c(0,35)) +
      labs(x = "Year", y = "Percentage of the population",
           color = "Race-Ethnicity", linetype = "Projection year",
           title = "Census Population Projections",
           subtitle = "2014 vs. 2017",
           caption = "Source: https://www.census.gov/data/datasets/2017/demo/popproj/2017-popproj.html") +
      theme_minimal() +
      theme(plot.caption = element_text(hjust = 0))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)





  