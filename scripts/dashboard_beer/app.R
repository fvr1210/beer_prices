#
# Using preused stuff from the Markdown file

library(shiny)
library(lubridate)
library(plotly)
library(ggthemes)
library(tidyverse) # Version 1.3.0
library(shinydashboard)

df_bci <- read_delim("processed-data/wwbp_city_add_var.csv", delim = ";" , locale = locale(encoding = 'ISO-8859-2'))
df_bco <- read_delim("processed-data/wwbp_country_add_var.csv", delim = ";" , locale = locale(encoding = 'ISO-8859-2'))
df_br <- read_delim("processed-data/wwbp_region_add_var.csv", delim = ";" , locale = locale(encoding = 'ISO-8859-2'))


# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
        sidebarMenuOutput("menu"),     # Dynamic sideboard content; code from https://rstudio.github.io/shinydashboard/structure.html
        tagList(                       # Aligne the checkboxes left; code from https://stackoverflow.com/questions/29738975/how-to-align-a-group-of-checkboxgroupinput-in-r-shiny
          tags$head(
            tags$style(
              HTML(
                ".checkbox-inline { 
                    margin-left: 0px;
                    margin-right: 10px;
          }
         .checkbox-inline+.checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
        "
              )
            ) 
          )
        )         
        ),
    dashboardBody(
      # define pages for sidebar
      tabItems(
        # First tab content checkboxes for cities, countries and regions
  tabItem(tabName = "city_side",
    fluidRow(
    checkboxGroupInput(inputId = "d_cc",label = "Show following Cities:", inline = T,
                                      choices = sort(unique(df_bci$cc)),
                                      selected = sort(unique(df_bci$cc))),
                    
    checkboxGroupInput(inputId = "d_countries",label = "Show cities in the following countries:", inline = T,
                                      choices = sort(unique(df_bci$country)),
                                      selected = sort(unique(df_bci$country))),
    checkboxGroupInput(inputId = "d_regions",label = "Show cities in the following Regions:", inline = T,
                                      choices = sort(unique(df_bci$region)),
                                      selected = sort(unique(df_bci$region))),
    tabBox(id = "cp", width=12, height=5,  #width of max 12 colums, hight not sure...
           tabPanel("Bar Prices", plotlyOutput("plot1")),
           tabPanel("Supermarket Prices", plotlyOutput("plot2")),
           tabPanel("Overall Prices", plotlyOutput("plot3")),
           tabPanel("Markup", plotlyOutput("plot4"))
 ))),
  tabItem(tabName = "country_side",
         fluidRow(
           checkboxGroupInput(inputId = "c_countries",label = "Show cities in the following countries:", inline = T,
                              choices = sort(unique(df_bco$country)),
                              selected = sort(unique(df_bco$country))),
           checkboxGroupInput(inputId = "c_regions",label = "Show cities in the following Regions:", inline = T,
                              choices = sort(unique(df_bco$region)),
                              selected = sort(unique(df_bco$region))),
           tabBox(id = "cp", width=12, height=5,  #width of max 12 colums, hight not sure...
                  tabPanel("Bar Prices", plotlyOutput("plot5")),
                  tabPanel("Supermarket Prices", plotlyOutput("plot6")),
                  tabPanel("Overall Prices", plotlyOutput("plot7")),
                  tabPanel("Markup", plotlyOutput("plot8"))
           )))
  )))
                    

server <- function(input, output) {
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("City Level", tabName = "city_side", icon = icon("th")),
      menuItem("Country Level", tabName = "country_side", icon = icon("th")))
    })
  
  
  # Country Level
    df_bcir <- reactive({
        dplyr::filter(df_bci, cc %in% input$d_cc & country %in% input$d_countries & region %in% input$d_regions)
    })
    
    col_reg <- c( "Europe & Central Asia" = "green3", "East Asia & Pacific" = "chocolate1", "Latin America & Caribbean" = "gold2", "North America" = "darkgoldenrod3", "Middle East & North Africa" = "deepskyblue", "South Asia" = "orchid", "Sub-Saharan Africa" = "red") 
    
    output$plot1 <-  renderPlotly ({
    p_1 <- ggplot(df_bcir(), aes(x=reorder(cc, -`BAR PRICE $`), y = `BAR PRICE $`, fill = region, text = paste("City:", cc, "\nPrice:", `BAR PRICE $`, "$"))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        # scale_fill_manual(values = col_age) +
        scale_x_discrete() +
        xlab("City") +
        ylab("$") +
        ggtitle("Price for a Beer in a bar in USD in selected cities") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")
    
    ggplotly(p_1, tooltip = "text") 
    })
    
    output$plot2 <-  renderPlotly ({
      p_2 <- ggplot(df_bcir(), aes(x=reorder(cc, -`AVERAGE SUPERMARKET PRICE $`), y = `AVERAGE SUPERMARKET PRICE $`, fill = region, text = paste("City:", cc, "\nPrice:", `AVERAGE SUPERMARKET PRICE $`, "$"))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        # scale_fill_manual(values = col_age) +
        scale_x_discrete() +
        xlab("City") +
        ylab("$") +
        ggtitle("Price for a Beer in a supermarket in USD in selected cities") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")
      
      ggplotly(p_2, tooltip = "text") 
    })
    
    output$plot3 <-  renderPlotly ({
      p_3 <- ggplot(df_bcir(), aes(x=reorder(cc, -`OVERALL PRICE $`), y = `OVERALL PRICE $`, fill = region, text = paste("City:", cc, "\nPrice:", `OVERALL PRICE $`, "$"))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        # scale_fill_manual(values = col_age) +
        scale_x_discrete() +
        xlab("City") +
        ylab("$") +
        ggtitle("Overall price in USD in selected cities") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")
      
      ggplotly(p_3, tooltip = "text") 
    })
    
    output$plot4 <-  renderPlotly ({
      p_4 <- ggplot(df_bcir(), aes(x=reorder(cc, -markup), y = markup, fill = region, text = paste("City:", cc, "\nRatio:", round(markup,2)))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        # scale_fill_manual(values = col_age) +
        scale_x_discrete() +
        xlab("City") +
        ylab("Ratio") +
        ggtitle("Markup in selected cities") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")
      
      ggplotly(p_4, tooltip = "text") 
    })
    
    # City Level
    df_bcor <- reactive({
      dplyr::filter(df_bco, country %in% input$d_countries & region %in% input$d_regions)
    })
    
    col_reg <- c( "Europe & Central Asia" = "green3", "East Asia & Pacific" = "chocolate1", "Latin America & Caribbean" = "gold2", "North America" = "darkgoldenrod3", "Middle East & North Africa" = "deepskyblue", "South Asia" = "orchid", "Sub-Saharan Africa" = "red") 
    
    output$plot5 <-  renderPlotly ({
      p_5 <- ggplot(df_bcor(), aes(x=reorder(country, -`BAR PRICE $`), y = `BAR PRICE $`, fill = region, text = paste("City:", country, "\nPrice:", `BAR PRICE $`, "$"))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        # scale_fill_manual(values = col_age) +
        scale_x_discrete() +
        xlab("Country") +
        ylab("$") +
        ggtitle("Price for a Beer in a bar in USD in selected cities") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")
      
      ggplotly(p_5, tooltip = "text") 
    })
    
    output$plot6 <-  renderPlotly ({
      p_6 <- ggplot(df_bcor(), aes(x=reorder(country, -`AVERAGE SUPERMARKET PRICE $`), y = `AVERAGE SUPERMARKET PRICE $`, fill = region, text = paste("City:", country, "\nPrice:", `AVERAGE SUPERMARKET PRICE $`, "$"))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        # scale_fill_manual(values = col_age) +
        scale_x_discrete() +
        xlab("Country") +
        ylab("$") +
        ggtitle("Price for a Beer in a supermarket in USD in selected cities") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")
      
      ggplotly(p_6, tooltip = "text") 
    })
    
    output$plot7 <-  renderPlotly ({
      p_7 <- ggplot(df_bcor(), aes(x=reorder(country, -`OVERALL PRICE $`), y = `OVERALL PRICE $`, fill = region, text = paste("City:", country, "\nPrice:", `OVERALL PRICE $`, "$"))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        # scale_fill_manual(values = col_age) +
        scale_x_discrete() +
        xlab("Country") +
        ylab("$") +
        ggtitle("Overall price in USD in selected cities") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")
      
      ggplotly(p_7, tooltip = "text") 
    })
    
    output$plot8 <-  renderPlotly ({
      p_8 <- ggplot(df_bcor(), aes(x=reorder(country, -markup), y = markup, fill = region, text = paste("City:", country, "\nRatio:", round(markup,2)))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        # scale_fill_manual(values = col_age) +
        scale_x_discrete() +
        xlab("Country") +
        ylab("Ratio") +
        ggtitle("Markup in selected cities") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")
      
      ggplotly(p_8, tooltip = "text") 
    })
}

shinyApp(ui, server)
