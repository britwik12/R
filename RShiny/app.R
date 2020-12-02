########################### Baltimore Crime Dataset #########################
library(chron)
library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)
library(rsconnect)

data <- read.csv("data.csv", header = T, na.strings = c("?", "NA", "-", ""))

###### Take a Sample of the Above data for Shiny App ##########
head(data)
colnames(data)
str(data)

##### Change Column tyepes #######
data$CrimeDate <- as.Date(data$CrimeDate , format = "%m/%d/%Y")
data$CrimeTime <- chron(times=data$CrimeTime)
data$CrimeCode <- as.character(as.factor(data$CrimeCode))
data$Location <- as.character(as.factor(data$Location))
data$Description <- as.character(as.factor(data$Description))
data$Inside.Outside <- as.character(as.factor(data$Inside.Outside))
data$Weapon <- as.character(as.factor(data$Weapon))
data$District <- as.character(as.factor(data$District))
data$Neighborhood <- as.character(as.factor(data$Neighborhood))
data$Premise <- as.character(as.factor(data$Premise))

## Missingness for Each Column
colSums(is.na(data))

## Chnage NA;s to meaningful values
data$Premise <- ifelse(is.na(data$Premise), "UNKNOWN", data$Premise)
data$Weapon <- ifelse(is.na(data$Weapon), "UNKNOWN", data$Weapon)
data$Location <- ifelse(is.na(data$Location), "UNKNOWN", data$Location)
data$District <- ifelse(is.na(data$District), "UNKNOWN", data$District)
data$Inside.Outside <- ifelse(is.na(data$Inside.Outside), "UNKNOWN", data$Inside.Outside)
data$Inside.Outside <- ifelse(data$Inside.Outside == "I", "Inside", data$Inside.Outside)
data$Inside.Outside <- ifelse(data$Inside.Outside == "O", "Outside", data$Inside.Outside)

## Remove all Rows with Missing Columns (Remove Noise)
data = data[complete.cases(data), ]

############## Number of Incidents per Date #######################
incidents_per_date <- data %>%
                   group_by(CrimeDate) %>%
                   summarise("Count" = n())
print(incidents_per_date)

################## Type of Incident ################################
type_of_incident <- data %>%
                   group_by(Description) %>%
                   summarise("Count" = n()) %>%
                   arrange(desc(Count))
print(type_of_incident)

################## Inside or Outside Event ########################
place_of_incident <- data %>%
                  group_by(Inside.Outside) %>%
                  summarise("Count" = n())

################# Weapon Used in Event ############################
type_of_weapon <- data %>%
                  group_by(Weapon) %>%
                  summarise("Count" = n()) %>%
                  arrange(desc(Count))

###################### Type of District #############################
district <- data %>%
            group_by(District) %>%
            summarise("Percentage" = n()/nrow(data)*100) %>%
            arrange(desc(Percentage))

##################### Premise of Incident ##############################
premise <- data %>%
            group_by(Premise) %>%
            summarise("Percentage" = n()/nrow(data)*100) %>%
            arrange(desc(Percentage))

########################################################################################
############################# Add a Column for Year in the Dataset ####################
data["Year"] <- format(as.Date(data$CrimeDate, format="%d/%m/%Y"),"%Y")

###################### Do Year by Year Analysis of Cases ##########################
year_by_year <- data %>%
              group_by(Year) %>%
              summarise("Number of Cases" = n())

################## Add a Column for 'Hour' in the Dataset ############################
################# Use this Column for the Slider  ###################################
data["Hour"] <- format(strptime(data$CrimeTime,"%H:%M:%S"),'%H')
data$Hour <- as.numeric(as.character(data$Hour))

####################################### R SHINY ############################################

# Define UI ----
ui <- fluidPage(
  
  h1(textOutput("title")),
  h3(textOutput("author")),
  splitLayout(dateRangeInput('dateRange',
                 label = 'Filter crimes by date',
                 start = "2012-01-01",
                 end   = "2017-09-02",
                 min = "2012-01-01",
                 max = "2017-09-02"),
  sliderInput("slider", "Time of the Day (24-Hour Period)",
              min = min(data$Hour), max = max(data$Hour), value = c(min(data$Hour),max(data$Hour)))),
  plotOutput('plot1'),
  splitLayout(plotOutput('plot2'),
  plotOutput('plot3'),
  plotOutput('plot4')),
  splitLayout(plotOutput('plot5'),
  plotlyOutput('plot6')),
  dataTableOutput('my_table')
  
)

# Define server ----
server <- function(input, output, session) {
  
  output$title <- renderText({
    invalidateLater(1000, session)
    paste("Baltimore Crime Analysis (2012-2017)")
  })
  
  output$author <- renderText({
    paste("Presented by R. Bhriguvanshi")
  })
  
######################### Plot for Type of Incident #########################
  output$plot1 <- renderPlot({
    # Filter the data
    ggplot(data %>% 
      select(CrimeDate, Description) %>%
      filter(data$CrimeDate >= input$dateRange[1] & data$CrimeDate <= input$dateRange[2]
             & data$Hour >= min(input$slider) & data$Hour <= max(input$slider)) %>%
      group_by(Description) %>%
      summarise("Count" = n()) %>%
      top_n(10),
      aes(x=Description, Count)) + 
      geom_bar(stat = "identity", width=0.7) + coord_flip() + theme_bw() +
      labs(title= "Type of Crime and Number of Incidents", y = "Number of Incidents",
           x = "Type of Crime") +
      theme(plot.title = element_text(color = "red", size = 18, face = "bold",hjust = 0.5),
            axis.title=element_text(size=14,face="italic"))
  })
  
######################### Plot for Weapon #########################
  output$plot2 <- renderPlot({
    # Filter the data
         ii = data %>% 
             select(CrimeDate, Weapon) %>%
             filter(data$CrimeDate >= input$dateRange[1] & data$CrimeDate <= input$dateRange[2]
                    & data$Hour >= min(input$slider) & data$Hour <= max(input$slider)) %>%
             group_by(Weapon) 
         
    ## Make the Plot     
         ggplot( ii %>%
           summarise("Percentage" = round((n()/nrow(ii)*100),2)) ,
           aes(x=Weapon, Percentage)) + 
      geom_bar(stat = "identity", width=0.7) + coord_flip()  + theme_bw() +
      labs(title= "Top Weapons used in Crime", y = "Overall Total Percentage",
           x = "Weapon") +
      theme(plot.title = element_text(color = "red", size = 15, face = "bold",hjust = 0.5),
            axis.title=element_text(size=14,face="italic")) +
      geom_text(aes(label= Percentage), col = "blue")
    
  })
  
######################### Plot for Event being Inside or Outside #########################
  output$plot3 <- renderPlot({
    # Filter the data
          jj = data %>% 
             select(CrimeDate, Inside.Outside) %>%
             filter(data$CrimeDate >= input$dateRange[1] & data$CrimeDate <= input$dateRange[2]
                    & data$Hour >= min(input$slider) & data$Hour <= max(input$slider)) %>%
             group_by(Inside.Outside) 
        
    ## Make the Plot   
        ggplot(jj %>%
           summarise("Percentage" = round((n()/nrow(jj)*100),2)) ,
           aes(x="", y=Percentage, fill = Inside.Outside)) + 
      geom_bar(stat = "identity", width=0.7) + coord_polar("y",start=0) + theme_bw()  +
      labs(title= "Inside or Outside Property") +
      theme(plot.title = element_text(color = "red", size = 15, face = "bold",hjust = 0.5))
    
  })
  
######################### Plot for Premise of Event #########################
  output$plot4 <- renderPlot({
    # Filter the data
          kk = data %>% 
             select(CrimeDate, Premise) %>%
             filter(data$CrimeDate >= input$dateRange[1] & data$CrimeDate <= input$dateRange[2]
                    & data$Hour >= min(input$slider) & data$Hour <= max(input$slider)) %>%
             group_by(Premise) 
          
    ## Make the Plot      
          ggplot(kk %>%
             summarise("Percentage" = round((n()/nrow(kk)*100),2)) %>%
             top_n(6),
           aes(x=Premise, Percentage)) + 
      geom_bar(stat = "identity", width=0.7) + coord_flip()  + theme_bw() +
      labs(title= "Common Premise Type", y = "Overall Total Percentage",
           x = "Type of Premise") +
      theme(plot.title = element_text(color = "red", size = 15, face = "bold",hjust = 0.5),
            axis.title=element_text(size=14,face="italic")) + 
      geom_text(aes(label= Percentage), col = "blue")
  })
  
################################# Year by Year Plot ######################################
  output$plot5 <- renderPlot({
    ggplot(data=year_by_year, aes(x=Year, y=`Number of Cases`, group=1)) +
    geom_line(size=1, col= "black") +
    geom_text(aes(label= `Number of Cases`), col = "blue") + theme_bw() +
      labs(title= "Total Number of Cases by Year", y = "Number of Cases",
           x = "Year") +
      theme(plot.title = element_text(color = "red", size = 18, face = "bold",hjust = 0.5),
            axis.title=element_text(size=14,face="italic"))
  
  })
  
######################### Plot for District of Occurance #########################
  output$plot6<- renderPlotly({
    # Filter the data
    a = data %>% 
      select(CrimeDate, District) %>%
      filter(data$CrimeDate >= input$dateRange[1] & data$CrimeDate <= input$dateRange[2]
             & data$Hour >= min(input$slider) & data$Hour <= max(input$slider)) %>%
      group_by(District) %>%
      summarise("Percentage" = n()/nrow(data)*100) %>%
      arrange(desc(Percentage))
    plot_ly(type = "funnelarea",
            text = a$District,
            values = a$Percentage,
            title="Percentages of Crime for Each District") 
  })

#########################  Table with Data Statistics #########################
  output$my_table  <- renderDataTable({
    # Filter the data
    data %>% 
      select(CrimeDate, CrimeTime, Location, Description, Weapon, District) %>%
      filter(data$CrimeDate >= input$dateRange[1] & data$CrimeDate <= input$dateRange[2]
             & data$Hour >= min(input$slider) & data$Hour <= max(input$slider))
  })
}

# Create Shiny app ----
shinyApp(ui, server)

####################################
#rsconnect::setAccountInfo(name='bhriguvanshi',
#                          token='39E8A91AE16A2599CC2FA11AB388896F',
#                          secret='KRjsdw9wCcaE5D2LO9Lv0tzNvQ15ICLKXz/H4Evn')




