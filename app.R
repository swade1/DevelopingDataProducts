#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
if(!require(ggthemes)) install.packages("ggthemes",repos = "http://cran.us.r-project.org")
library(ggthemes)

cel <- drop_na(read_csv("cel_volden_wiseman-_coursera.csv"))
lot <- cel %>% select(congress,dem)
Party <- recode(cel$dem, `0`="Republican",`1`="Democrat")
lot <- add_column(lot, Party)
fig_data <- lot %>% group_by(congress,Party) %>% summarize(count=n())

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Party Representation by Congress, 1973-2013"),
    #hr(),
    h5("To view the number of Democrats and Republicans per session of Congress, choose the Congress number from the drop down list."),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("congress", "Select Congress", choices = unique(fig_data$congress), selected=NULL)
        ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)


server <- function(input, output) {
    output$distPlot <- renderPlot({
        data = fig_data %>% filter(congress %in% input$congress)
        req(input$congress)
        ggplot(data=data, aes(x=Party, y=count, fill=Party)) + 
            geom_bar(stat="identity") +
            labs(x="Party",y="Count") +  
            scale_fill_manual(values=c("steelblue","darkred")) + 
            guides(fill=FALSE) + 
            theme_wsj()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
