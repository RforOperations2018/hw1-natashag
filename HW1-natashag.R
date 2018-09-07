#I am going to create the UI of a NavBar for the Esoph package

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(DT)

cancer<-esoph


pdf(NULL)

ui <- fluidPage(
  navbarPage("Smoking, Alcohol and Esophageal Cancer NavBar", 
             theme = shinytheme("united"),
             tabPanel("Plot",
                      sidebarLayout(
                        sidebarPanel(
                         selectInput("agegp_select",
                                      "Age:",
                                      choices = levels(cancer$agegp),
                                      multiple = TRUE,
                                      selectize = TRUE,
                                      selected = c("25-34", "35-44", "45-54", "55-64","65-74","75+")),
                          sliderInput("ncasesselect",
                                      "Number of cancer cases:",
                                    min= min(cancer$ncontrols, na.rm = T),
                                    max= max(cancer$ncontrols, na.rm = T),
                                    value= c(min(cancer$ncontrols, na.rm = T),max(cancer$ncontrols.na.rm = T)),
                                    step =1)
                                  ),
                        # Output plot
                        mainPanel(
                          plotlyOutput("plot")
                        )
                      )
                    ),
             # Data Table
             tabPanel("Table",
                      fluidPage(DT::dataTableOutput("table"))
             )
       )     
  )



# I will define the server 
server <- function(input, output) {
  output$plot <- renderPlotly({
    #dat <- subset(meltcancer, agegp %in% input$agegp_select)
    ggplot(data = cancer, aes(x = alcgp, y= ncases, fill = agegp)) + geom_bar(stat="identity") 
  }) 
  output$table <- DT::renderDataTable({
    subset(cancer, agegp %in% input$agegp_select, select = c(agegp, alcgp, tobgp, ncases, ncontrols))
  })
}

#  I will run the application 
shinyApp(ui = ui, server = server)
    
