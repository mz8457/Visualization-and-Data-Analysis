library(shiny)
library(data.table)
library(plotly)

#system.time(cdc_csv_output_infant_deaths <- fread("cdc_csv_output_infant_deaths.csv"))

setDT(cdc_csv_output_infant_deaths_1000)

#system.time(cdc_csv_output_live_births <- fread("live_birth_new.csv", nrows = 10000))

death_mann <- c("Accident", "Suicide", "Homicide", "Pending investigation", "Could not determine", "Self-inflicted", 
                "Natural", "Not specified")

ui <- navbarPage("CDC",
                 tabPanel("Plot_infant_deaths",
                          sidebarLayout(
                            sidebarPanel(
                              
                              selectInput("del",
                                          "Delivery year:",
                                          c("All",
                                            as.character(cdc_csv_output_infant_deaths_1000$delivery_year))),
                              selectInput("death_man",
                                          "Death manner:",
                                          c("All",
                                            death_mann)),
                              selectInput("del_month",
                                          "Delivery month:",
                                          c("All",
                                            as.character(cdc_csv_output_infant_deaths_1000$delivery_month)))
                            ),
                            
                            mainPanel(plotOutput('plot1'), br(), plotlyOutput("plot2"), br(), plotlyOutput("plot4"))
                          )
                 ),
                 tabPanel("Table_infant_deaths",
                          DT::dataTableOutput("table")
                 ),
                 tabPanel("Plot2_infant_deaths",
                          fluidPage( plotlyOutput("plot3")
                          )))
#########################################################################################################

server <- function(input, output, session)
{
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- cdc_csv_output_infant_deaths_1000
    
    if (input$del != "All" & input$death_man != "All" & input$del_month != "All") {
      
      deathMann = switch(input$death_man, "Accident" = "1", "Suicide" = "2", "Homicide" = "3", "Pending investigation" = "4", "Could not determine" = "5", "Self-inflicted" = "6", 
                         "Natural" = "7", "Not specified" = "NA")
      
      data <- data[delivery_year == input$del &  manner_of_death == deathMann & delivery_month == input$del_month, list(age_at_death_in_days, infant_age_recode_5)]
    }
    data
  })
  )
  output$plot1 <- renderPlot({
    
    data <- cdc_csv_output_infant_deaths_1000
    
    if (input$del != "All" & input$death_man != "All" & input$del_month != "All") {
      
      deathMann = switch(input$death_man, "Accident" = "1", "Suicide" = "2", "Homicide" = "3", "Pending investigation" = "4", "Could not determine" = "5", "Self-inflicted" = "6", 
                         "Natural" = "7", "Not specified" = "NA")
      data <- data[delivery_year == input$del &  manner_of_death == deathMann & delivery_month == input$del_month, list(age_at_death_in_days, infant_age_recode_22, gestation_in_weeks)]
    }
    data
    par(mar = c(5, 4, 3, 3))
    hist(data$infant_age_recode_22, main=" ", 
         xlab="infant_age_recode_22", 
         border="chocolate", 
         col="aquamarine3",
         las=1, 
         breaks = seq(0,23,1))
  })
  output$plot2 <- renderPlotly({
    
    data <- cdc_csv_output_infant_deaths_1000
    dataSub1 <- data[, .N, by = .(infant_age_recode_5, manner_of_death)]
    
    plot_ly(data, x = dataSub1$infant_age_recode_5, y = dataSub1$manner_of_death, text = dataSub1$N, type = 'scatter', mode = 'markers', size = dataSub1$N, color = dataSub1$infant_age_recode_5,
            colors = 'Paired',
            #Choosing the range of the bubbles' sizes:
            sizes = c(10, 50),
            marker = list(opacity = 0.75, sizemode = 'diameter')) %>%
      layout(title = 'Manner of death for neonatal death (2007-2016)',
             xaxis = list(title = "infant_age_recode_5", showgrid = FALSE),
             yaxis = list(title = "manner_of_death", showgrid = FALSE))
    
  })
  
  output$plot4 <- renderPlotly({
    
    data <- cdc_csv_output_infant_deaths_1000
    plot_ly(data, x = data$gestation_in_weeks, y = data$age_at_death_in_days, type = "scatter", mode = 'markers')%>%
    layout(title = 'Relationship between gestion_in_weeks and age_at_death_in_days',
           xaxis = list(title = "gestion_in_weeks", showgrid = FALSE),
           yaxis = list(title = "age_at_death_in_days", showgrid = FALSE))
  })
  
  output$plot3 <- renderPlotly({
    
    data <- cdc_csv_output_infant_deaths_1000
    birth_inj <- unique(as.character(data$abnormal_newborn_conditions_birth_injury))
    data[data$abnormal_newborn_conditions_birth_injury == "N", "abnormal_newborn_conditions_birth_injury"] <- "1"
    data[data$abnormal_newborn_conditions_birth_injury == "U", "abnormal_newborn_conditions_birth_injury"] <- "2"
    data[data$abnormal_newborn_conditions_birth_injury == "Y", "abnormal_newborn_conditions_birth_injury"] <- "3"
    data <- data[!is.na(data$manner_of_death) & !is.na(data$abnormal_newborn_conditions_birth_injury) & data$mothers_marital_status != 9,]%>%
      plot_ly(width = 1000, height = 700) %>%
      add_trace(type = 'parcoords',
                line = list(color = data$infant_age_recode_5,
                            colorscale = 'Jet',
                            showscale = TRUE,
                            reversescale = TRUE,
                            cmin = 1,
                            cmax = 5),
                dimensions = list(
                  list(tickvals = c(1, 2, 3, 4, 5),
                       ticktext = c(" < 1 h", "1-23 h", "1-6 d", "7-27 d", "> 28 d"),
                       #constraintrange = c(1, 5),
                       label = 'infant_age_recode_5', values = data$infant_age_recode_5),
                  
                  list(ticvals = c(1, 2, 3),
                       ticktext = c("No", "Unknown", "Yes"),
                       label = 'abnormal_newborn_conditions_birth_injury', values = data$abnormal_newborn_conditions_birth_injury),
                  
                  list(tickvals = c(1, 2, 3, 4, 5, 6, 7),
                       ticktext = c("Accident", "Suicide", "Homicide", "Pending investigation", "Could not determine", "Self-inflicted", 
                                    "Natural", "Not specified"),
                       label = 'manner_of_death', values = data$manner_of_death),
                  
                  list(tickvals = c(1, 2, 3),
                       ticktext = c("Yes", "No", "not living together"),
                       label = 'mothers_marital_status', values = data$mothers_marital_status),
                  
                  list(range = c(min(data$mothers_age_recode_41), max(data$mothers_age_recode_41)),
                       label = "mothers_age_recode_41", values = data$mothers_age_recode_41)
                  
                  
                  
                  # list(range = c(min(data$age_at_death_in_days), max(data$age_at_death_in_days)),
                  #      label = 'age_at_death_in_days', values = data$age_at_death_in_days),
                  
                  
                )
      )
  })
}
shinyApp(ui = ui, server = server)