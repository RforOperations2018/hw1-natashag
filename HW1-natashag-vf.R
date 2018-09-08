#I am going to create the UI of a NavBar for the Esoph package

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(DT)

cancer <- esoph

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
                          sliderInput("ncontrolsSelect",
                                      "Number of Controls:",
                                      min = min(cancer$ncontrols, na.rm = T),
                                      max = max(cancer$ncontrols, na.rm = T),
                                      value = c(min(cancer$ncontrols, na.rm = T), max(cancer$ncontrols, na.rm = T)),
                                      step = 1)
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
    ggplotly(
      ggplot(data = cancer, aes(x = alcgp, y= ncases, fill = tobgp)) + geom_bar(stat="identity")+ 
        facet_grid(.~agegp) +
        labs(x = "", y="Number of Cases of Cancer", title= "Smoking, Alcohol and Esophageal Cancer by Age",  fill= "Smoking Level")+ 
        theme(text= element_text(size=7.5), axis.text.x=element_text(angle=45,vjust=1,hjust=1), axis.title.x=element_text(line=2.2, margin = margin(t = 20, r = 0, b = 0, l = 0))) # The margin call here works just in ggplot2
    ) %>% layout(xaxis=list(title = "<br><br>Alcohol level")) # Closest I could get.
  }) 
  output$table <- DT::renderDataTable({
    subset(cancer, agegp %in% input$agegp_select, select = c(agegp, alcgp, tobgp, ncases, ncontrols))
  })
}

#  I will run the application 
shinyApp(ui = ui, server = server)