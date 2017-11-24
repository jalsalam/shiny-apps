# app.R - a simple one-fil app
library("tidyverse")

ui <- bootstrapPage(
  selectInput("group_var", "Group variable", choices = c("manufacturer", "model", "displ", "class") ),
  plotOutput('plot')
)


# Define the server code
server <- function(input, output) {
  
  output$plot <- renderPlot({
    group_var <- input$group_var %>% sym()
    
    df <- mpg %>%
      group_by(!!group_var) %>%
      summarize(cty = mean(cty),
                hwy = mean(hwy)) 
    
    ggplot(df, aes_string(x = "cty", y = "hwy", label = input$group_var)) +
      geom_text()
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)