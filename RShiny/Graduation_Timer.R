library(shiny)

# Define UI ----
ui <- fluidPage(
  
  h2(textOutput("today")),
  dateInput('Graduation', 'Graduation'),
  h3(textOutput("timeleft")),
  selectInput('Unit', 'Unit', c("days" = "days", "Weeks" = "weeks", "months" = "months", "years" = "years"), selected = "months"),
  h3(textOutput("time"))
  
)

# Define server ----
server <- function(input, output, session) {
  
  output$today <- renderText({
    invalidateLater(1000, session)
    paste("Today's Date is:", Sys.Date())
  })
  
  output$timeleft <- renderText({
    paste(as.Date(input$Graduation) - Sys.Date(), "Days left to Graduation")
  })
 
  
  output$time <- renderText({
    if (input$Unit == "days") {
      paste(as.Date(input$Graduation) - Sys.Date(), "Days left to Graduation")
    } else if (input$Unit == "weeks") {
      paste((round(difftime(as.Date(input$Graduation), Sys.Date(), units = "weeks"))), "Weeks left to Graduation")
    } else if (input$Unit == "months") {
      paste((length(seq(from=Sys.Date(), to=as.Date(input$Graduation), by='month')) - 1), "Months to Graduation")
    } else {
      paste((round(difftime(as.Date(input$Graduation), Sys.Date(), units = "weeks")/52.25,2)), "Years left to Graduation")
    }
    
    })
  
}

# Create Shiny app ----
shinyApp(ui, server)

## Deploy
library(rsconnect)
rsconnect::deployApp('C:/Users/Ritwik/OneDrive/BZAN552/HW5')



