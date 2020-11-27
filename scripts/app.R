
# Using preused stuff from the Markdown file

library(shiny)
library(lubridate)
library(plotly)
library(ggthemes)
library(tidyverse) # Version 1.3.0
library(shinydashboard)
library(emojifont) # https://mran.microsoft.com/snapshot/2016-07-03/web/packages/emojifont/vignettes/emojifont.html
library(shinyWidgets)
library(DT)

load.emojifont('OpenSansEmoji.ttf')

#read city level data ----

df_bci_wide <- read_delim("../processed-data/beer_city_add_wide.csv", delim = ";" , locale = locale(encoding = 'ISO-8859-2'))
df_bci_long <- read_delim("../processed-data/beer_city_add_long.csv", delim = ";" , locale = locale(encoding = 'ISO-8859-2'))

df_bco <- read_delim("../processed-data/beer_country_add_wide.csv", delim = ";" , locale = locale(encoding = 'ISO-8859-2')) %>% 
  mutate(label = c(emoji('beer')))


introduction_text <-  read_file("../processed-data/introduction.html", locale = locale(encoding = 'ISO-8859-2'))
data_source_text <-  read_file("../processed-data/data_source.html", locale = locale(encoding = 'ISO-8859-2'))
data_final_text <-  read_file("../processed-data/data_final.html", locale = locale(encoding = 'ISO-8859-2'))

# define theme ----
mytheme <- theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 7),
        legend.title = element_blank(),
        legend.position = "bottom")



# Define UI for application that draws a histogram ----
ui <- dashboardPage( skin = "yellow",
    dashboardHeader(title = "Beerboard"),
    dashboardSidebar(                                                                   # Dynamic sideboard content; code from https://rstudio.github.io/shinydashboard/structure.html
        sidebarMenuOutput("menu"),
        tagList(                       # Aligne the checkboxes left; code from https://stackoverflow.com/questions/29738975/how-to-align-a-group-of-checkboxgroupinput-in-r-shiny
          tags$head(
            tags$style(
              HTML(                   # Change position of different elements 
                ".checkbox-inline {    
                    margin-left: 10px;
                    margin-right: 10px;
                    }",
                ".shiny-html-output {    
                    margin-left: 20px;
                    margin-right: 20px;
                    }",
                ".action-button{ 
                    margin-left: 10px;
                    margin-top: -10px;
                    margin-bottom: 10px;
                    }",
                ".shiny-options-group{ 
                    margin-left: 15px;
                    }",
                
                ".control-label{ 
                    margin-left: 15px;
                    }",
           ))))         
        ),
    dashboardBody(
      # Information 
      tabItems(
        tabItem(tabName = "Intro_tx",
                fluidRow(htmlOutput("introduction"))),
        tabItem(tabName = "Source_tx",
                fluidRow(htmlOutput("data_source"))),
        tabItem(tabName = "Final_tx",
                fluidRow(htmlOutput("data_final"))),
        

        
        # City Level ----
        # Prices ----
        tabItem(tabName = "Price_ci",

                fluidRow(
                dropdownButton(
                    tags$h3("List of Input"),
                    circle = TRUE, status = "primary", icon = icon("gear"), width = "900px",
                    tooltip = tooltipOptions(title = "Select cities, countries and regions"),
                  checkboxGroupInput(inputId = "d_cc", label = "Show following Cities:", df_bci_wide$cc, inline = T),
                  actionButton("un_selectall_dci","Un/Select All"),  # action Links to select and unselect all (reactiv function in server, idea from https://stackoverflow.com/questions/28829682/r-shiny-checkboxgroupinput-select-all-checkboxes-by-click)
                  
                  
                  
                  checkboxGroupInput(inputId = "d_countries",label = "Show cities in the following countries:", df_bci_wide$country, inline = T),
                  actionButton("un_selectall_dco","Un/Select All"),
                  
                  
                  checkboxGroupInput(inputId = "d_regions",label = "Show cities in the following Regions:", df_bci_wide$region, inline = T),
                  actionButton("un_selectall_dre","Un/Select All"))),
                  
                fluidRow(
                  tabBox(id = "cp", width=12,  
                         tabPanel("Hotel Bar Prices", plotlyOutput("plot1"), "Data: GoEuro, Graphic: Flavio von Rickenbach, CC-BY 4.0"),
                         tabPanel("Supermarket Prices", plotlyOutput("plot2"), "Data: GoEuro, Graphic: Flavio von Rickenbach, CC-BY 4.0"),
                         tabPanel("Overall Prices", plotlyOutput("plot3"), "Data: GoEuro, Graphic: Flavio von Rickenbach, CC-BY 4.0"),
                         tabPanel("Markup", plotlyOutput("plot4"), "Data: GoEuro, Graphic: Flavio von Rickenbach, CC-BY 4.0")
                  ))),
        
        # Beer Index ----
        tabItem(tabName = "BI_ci",

                fluidRow(
                  dropdownButton(
                    tags$h3("List of Input"),
                    circle = TRUE, status = "primary", icon = icon("gear"), width = "900px",
                    tooltip = tooltipOptions(title = "Select cities, countries and regions"),
                   
                    checkboxGroupInput(inputId = "d_cc_l", label = "Show following Cities:", df_bci_wide$cc, inline = T),
                    actionButton("un_selectall_dci_l","Un/Select All"),  # action Links to select and unselect all (reactiv function in server, idea from https://stackoverflow.com/questions/28829682/r-shiny-checkboxgroupinput-select-all-checkboxes-by-click)



                    checkboxGroupInput(inputId = "d_countries_l",label = "Show cities in the following countries:", df_bci_wide$country, inline = T),
                    actionButton("un_selectall_dco_l","Un/Select All"),


                    checkboxGroupInput(inputId = "d_regions_l",label = "Show cities in the following Regions:", df_bci_wide$region, inline = T),
                    actionButton("un_selectall_dre_l","Un/Select All"))),
                fluidRow(
                  tabBox(id = "cp", width=12,  
                         tabPanel("Beer Index", plotlyOutput("plot18"), 
                                  radioButtons(inputId = "sort_by", label = "sort by", choices  =  c("Hotel bar prices", "Supermarket prices", "Overall prices"), selected = "Hotel bar prices", inline = TRUE),
                                  "Data: GoEuro, UBS earning, Graphic: Flavio von Rickenbach, CC-BY 4.0")
                         ), 
                  )),
        
        tabItem(tabName = "Data_ci", width =8,
                
                
                  fluidRow(
                    dataTableOutput("table_city_wide")
                  )
          ),
        
        #Section Country Level ----
        
        # Subsection Price ----
        tabItem(tabName = "Price_co",
            fluidRow(
              dropdownButton(
                tags$h3("List of Input"),
                circle = TRUE, status = "primary", icon = icon("gear"), width = "900px",
                tooltip = tooltipOptions(title = "Select countries and regions"),
                  
                  checkboxGroupInput(inputId = "c_countries_pr",label = "Show cities in the following countries:", df_bco$country, inline = T),
                  actionButton("un_selectall_cco_pr","Un/Select All"),
                  
                  checkboxGroupInput(inputId = "c_regions_pr",label = "Show cities in the following Regions:", unique(df_bco$region), inline = T),
                  actionButton("un_selectall_cre_pr","Un/Select All"))),
            
            fluidRow(
                  tabBox(id = "cp", width=12,   
                         tabPanel("Hotel Bar Prices", plotlyOutput("plot5"), "Data: GoEuro, Graphic: Flavio von Rickenbach, CC-BY 4.0"),
                         tabPanel("Supermarket Prices", plotlyOutput("plot6"), "Data: GoEuro, Graphic: Flavio von Rickenbach, CC-BY 4.0"),
                         tabPanel("Overall Prices", plotlyOutput("plot7"), "Data: GoEuro, Graphic: Flavio von Rickenbach, CC-BY 4.0"),
                         tabPanel("Markup", plotlyOutput("plot8"), "Data: GoEuro, Graphic: Flavio von Rickenbach, CC-BY 4.0")
                  ))),
        
        
      # Subsection Consumption ----       
        tabItem(tabName = "Cons_co",
                fluidRow(
                  dropdownButton(
                    tags$h3("List of Input"),
                    circle = TRUE, status = "primary", icon = icon("gear"), width = "900px",
                    tooltip = tooltipOptions(title = "Select countries and regions"),
                    
                    checkboxGroupInput(inputId = "c_countries_con",label = "Show cities in the following countries:", df_bco$country, inline = T),
                    actionButton("un_selectall_cco_con","Un/Select All"),
                    
                    checkboxGroupInput(inputId = "c_regions_con",label = "Show cities in the following Regions:", unique(df_bco$region), inline = T),
                    actionButton("un_selectall_cre_con","Un/Select All"))),

                fluidRow(
                  tabBox(id = "cd", width=12,   
                         tabPanel("Beer Consumption (GoEuro)", plotlyOutput("plot15"), "Data: GoEuro, Graphic: Flavio von Rickenbach, CC-BY 4.0"),
                         tabPanel("Beer Consumption (GHO)", plotlyOutput("plot16"), "Data: GHO, Graphic: Flavio von Rickenbach, CC-BY 4.0"),
                         tabPanel("Beer Consumption (Wikipedia)", plotlyOutput("plot17"), "Data: Wikipedia, Graphic: Flavio von Rickenbach, CC-BY 4.0")
                  ))),
      
      # Price vs. Consumption ----
      tabItem(tabName = "PvC_co",
              fluidRow(
                dropdownButton(
                  tags$h3("List of Input"),
                  circle = TRUE, status = "primary", icon = icon("gear"), width = "900px",
                  tooltip = tooltipOptions(title = "Select countries and regions"),
                  
                  checkboxGroupInput(inputId = "c_countries_pvc",label = "Show cities in the following countries:", df_bco$country, inline = T),
                  actionButton("un_selectall_cco_pvc","Un/Select All"),
                  
                  checkboxGroupInput(inputId = "c_regions_pvc",label = "Show cities in the following Regions:", unique(df_bco$region), inline = T),
                  actionButton("un_selectall_cre_pvc","Un/Select All"))),
              
              fluidRow(
                tabBox(id = "cd", width=12,   
                       tabPanel("Price vs. Consumption (goEuroe)", plotlyOutput("plot9"), "Data: GoEuro, Graphic: Flavio von Rickenbach, CC-BY 4.0"),
                       tabPanel("Price vs. Consumption (GHO)", plotlyOutput("plot10"), "Data: GoEuro, GHO, Graphic: Flavio von Rickenbach, CC-BY 4.0"),
                       tabPanel("Price vs. Consumption (Wikipedia)", plotlyOutput("plot11"),"Data: GoEuro, Wikipedia, Graphic: Flavio von Rickenbach, CC-BY 4.0")
                ))),


      # Subsection Beer Buying power  ---- 
        tabItem(tabName = "BbP_co",
                fluidRow(
                  dropdownButton(
                    tags$h3("List of Input"),
                    circle = TRUE, status = "primary", icon = icon("gear"), width = "900px",
                    tooltip = tooltipOptions(title = "Select countries and regions"),
                    
                    checkboxGroupInput(inputId = "c_countries_bbp",label = "Show cities in the following countries:", df_bco$country, inline = T),
                    actionButton("un_selectall_cco_bbp","Un/Select All"),
                    
                    checkboxGroupInput(inputId = "c_regions_bbp",label = "Show cities in the following Regions:", unique(df_bco$region), inline = T),
                    actionButton("un_selectall_cre_bbp","Un/Select All"))),

                fluidRow(
                  tabBox(id = "cd", width=12,   
                         tabPanel("Beer buying Power Hotel Bar", plotlyOutput("plot12"), "Data: GoEuro, World Bank, Graphic: Flavio von Rickenbach, CC-BY 4.0"),
                         tabPanel("Beer buying Power Supermaket", plotlyOutput("plot13"), "Data: GoEuro, World Bank, Graphic: Flavio von Rickenbach, CC-BY 4.0"),
                         tabPanel("Beer buying Power Overall", plotlyOutput("plot14"), "Data: GoEuro, World Bank, Graphic: Flavio von Rickenbach, CC-BY 4.0")
                  ))),
        
      # Country level data ----
        
        tabItem(tabName = "Data_co", width =8,
                fluidRow(
                  dataTableOutput("table_country")
                )
        )
                           )))
        
        
 
server <- function(input, output, session) {
  
output$menu <- renderMenu({
sidebarMenu(
  id = "tabs",
  menuItem("Info", tabName = "Info_text", icon = icon("info"),
      menuSubItem("Intro", tabName = "Intro_tx", icon = icon("")),
      menuSubItem("Data Source", tabName = "Source_tx", icon = icon("")),
      menuSubItem("Final Data", tabName = "Final_tx", icon = icon(""))),
  menuItem("City Level", tabName = "city_side", icon = icon("city"),
           menuSubItem("Prices", tabName = "Price_ci", icon = icon("")),
           menuSubItem("Beer Index", tabName = "BI_ci", icon = icon("")),
           menuSubItem("Data", tabName = "Data_ci", icon = icon(""))),
  
  menuItem("Country Level", tabName = "country_side", icon = icon("flag"),
           menuSubItem("Prices", tabName = "Price_co", icon = icon("data")),
           menuSubItem("Consumption", tabName = "Cons_co", icon = icon("data")),
           menuSubItem("Price vs. Consumption", tabName = "PvC_co", icon = icon("data")),
           menuSubItem("Beer buying Power", tabName = "BbP_co", icon = icon("data")),
           menuSubItem("Data", tabName = "Data_co", icon = icon("chart")))
)
    })


# Read infotext for Frontpage

output$introduction <- renderUI({HTML(introduction_text)})
output$data_source <- renderUI({HTML(data_source_text)})
output$data_final <- renderUI({HTML(data_final_text)})


  
  # City Level ----
  
  
  # Table with the Data
  output$table_city_wide <- renderDataTable({DT::datatable(df_bci_wide, filter = "top", extensions = c('Buttons', 'Scroller'), 
                                                      options = list(scrollY = 650,
                                                                     scrollX = 500,
                                                                     deferRender = TRUE,
                                                                      scroller = TRUE))}) # https://stackoverflow.com/questions/59681856/r-large-datatable-display-in-shiny
  
  
  # un/select all cities wide
  observe({
    if (input$un_selectall_dci%%2 == 0) #don't know how and why this works...
    {
      updateCheckboxGroupInput(session, "d_cc","Show following Cities:",choices=df_bci_wide$cc, selected = df_bci_wide$cc, inline=T)
    }
    else if(input$un_selectall_dci == 0) return(NULL)
    else
    {
      updateCheckboxGroupInput(session,"d_cc","Show following Cities:",choices=df_bci_wide$cc, inline=T)
    }
  })

  # un/select all countries
  observe({
    if (input$un_selectall_dco%%2== 0)
    {
      updateCheckboxGroupInput(session, "d_countries","Show following Countries:",choices=sort(unique(df_bci_wide$country)), selected = df_bci_wide$country, inline=T)
    }
    else if(input$un_selectall_dco == 0) return(NULL)
    else
    {
      updateCheckboxGroupInput(session,"d_countries","Show following Countries:",choices=sort(unique(df_bci_wide$country)), inline=T) #use sort and unique that every country is only once shown
    }
  })

  # un/select all regions
  observe({
    if (input$un_selectall_dre%%2== 0)
    {
      updateCheckboxGroupInput(session, "d_regions","Show following Regions:",choices=sort(unique(df_bci_wide$region)), selected = df_bci_wide$region, inline=T)
    }
    else if(input$un_selectall_dre == 0) return(NULL)
    else
    {
      updateCheckboxGroupInput(session,"d_regions","Show following Regions:",choices=sort(unique(df_bci_wide$region)), inline=T) #use sort and unique that every country is only once shown
    }
  })

# reactive data wide
    df_bcir <- reactive({
        dplyr::filter(df_bci_wide, cc %in% input$d_cc & country %in% input$d_countries & region %in% input$d_regions)
    })
    
    # un/select all cities long
    observe({
      if (input$un_selectall_dci_l%%2 == 0) #don't know how and why this works...
      {
        updateCheckboxGroupInput(session, "d_cc_l","Show following Cities:",choices=sort(unique(df_bci_long$cc)), selected = df_bci_long$cc, inline=T)
      }
      else if(input$un_selectall_dci_l == 0) return(NULL)
      else
      {
        updateCheckboxGroupInput(session,"d_cc_l","Show following Cities:",choices=sort(unique(df_bci_long$cc)), inline=T)
      }
    })
    
    # un/select all countries
    observe({
      if (input$un_selectall_dco_l%%2== 0)
      {
        updateCheckboxGroupInput(session, "d_countries_l","Show following Countries:",choices=sort(unique(df_bci_long$country)), selected = df_bci_long$country, inline=T)
      }
      else if(input$un_selectall_dco_l == 0) return(NULL)
      else
      {
        updateCheckboxGroupInput(session,"d_countries_l","Show following Countries:",choices=sort(unique(df_bci_long$country)), inline=T) #use sort and unique that every country is only once shown
      }
    })
    
    # un/select all regions
    observe({
      if (input$un_selectall_dre_l%%2== 0)
      {
        updateCheckboxGroupInput(session, "d_regions_l","Show following Regions:",choices=sort(unique(df_bci_long$region)), selected = df_bci_long$region, inline=T)
      }
      else if(input$un_selectall_dre_l == 0) return(NULL)
      else
      {
        updateCheckboxGroupInput(session,"d_regions_l","Show following Regions:",choices=sort(unique(df_bci_long$region)), inline=T) #use sort and unique that every country is only once shown
      }
    })
    
    # reactive data long
    df_bcir_l <- reactive({
      dplyr::filter(df_bci_long, cc %in% input$d_cc_l & country %in% input$d_countries_l & region %in% input$d_regions_l)
    })    
    
    
    
    
    
# colors for regions    
col_reg <- c( "Europe & Central Asia" = "green3", "East Asia & Pacific" = "chocolate1", "Latin America & Caribbean" = "gold2", "North America" = "darkgoldenrod3", "Middle East & North Africa" = "deepskyblue", "South Asia" = "orchid", "Sub-Saharan Africa" = "red") 
    
  
    # Average price in a bar (City level)
    output$plot1 <-  renderPlotly ({
    p_1 <- ggplot(df_bcir(), aes(x=reorder(cc, -abp), y = abp, fill = region, text = paste("City:", cc, "\nPrice:", abp, "$"))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        scale_x_discrete() +
        xlab("City") +
        ylab("$") +
        ggtitle("Price for a 33cl beer in a hotel bar in $ in selected cities") +
        mytheme
    
    ggplotly(p_1, tooltip = "text") 
    })
    
    # Average price in a Supermarket (City level)
    output$plot2 <-  renderPlotly ({
      p_2 <- ggplot(df_bcir(), aes(x=reorder(cc, -asmp), y = asmp, fill = region, text = paste("City:", cc, "\nPrice:", asmp, "$"))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        scale_x_discrete() +
        xlab("City") +
        ylab("$") +
        ggtitle("Price for a 33cl beer in a supermarket in $ in selected cities") +
        mytheme

      ggplotly(p_2, tooltip = "text") 
    })
    
    # Overall Average price (City level)
    output$plot3 <-  renderPlotly ({
      p_3 <- ggplot(df_bcir(), aes(x=reorder(cc, -aop), y = aop, fill = region, text = paste("City:", cc, "\nPrice:", aop, "$"))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        scale_x_discrete() +
        xlab("City") +
        ylab("$") +
        ggtitle("Overall price for a 33cl beer in $ in selected cities") +
        mytheme
      
      ggplotly(p_3, tooltip = "text") 
    })
    
    # Markup city Level
    output$plot4 <-  renderPlotly ({
      p_4 <- ggplot(df_bcir(), aes(x=reorder(cc, -markup), y = markup, fill = region, text = paste("City:", cc, "\nRatio:", markup))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        scale_x_discrete() +
        xlab("City") +
        ylab("Ratio") +
        ggtitle("How many supermarket beers could you buy for the price of one hotel bar beer") +
        mytheme
      
      ggplotly(p_4, tooltip = "text") 
    })
    

# Beer Index Plot with if else to sort by different price sources
    
    # creat different order functions based on source (https://stackoverflow.com/questions/62011413/ggplot2-bar-chart-order-by-values-of-one-group)
    sel_order_hb <- 
      df_bci_long %>% 
      filter(source == "at the hotel bar") %>% 
      arrange(desc(minutes)) %>% 
      mutate(city = factor(city))
    
    sel_order_sm <- 
      df_bci_long %>% 
      filter(source == "in the supermarket") %>% 
      arrange(desc(minutes)) %>% 
      mutate(city = factor(city))
    
    sel_order_oa <- 
      df_bci_long %>% 
      filter(source == "Overall") %>% 
      arrange(desc(minutes)) %>% 
      mutate(city = factor(city))
    
    
    
    output$plot18 <-  
        renderPlotly ({
    if(input$sort_by == "Hotel bar prices"){
    
    p_18 <-   df_bcir_l() %>% 
        mutate(city = factor(city, levels = sel_order_hb$city, ordered = TRUE)) %>% 
     ggplot(aes(x=city, y = minutes, fill = source, text = paste("City:", city, "\nMinutes:", minutes)), group = city) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        # scale_fill_manual(values = col_reg) +
        scale_x_discrete() +
        xlab("City") +
        ylab("Minutes") +
        ggtitle("How long do you have to work with a average hourly net income for a beer") +
        mytheme
      
    ggplotly(p_18, tooltip = "text") 
    }  
    else if(input$sort_by == "Supermarket prices"){
      p_18 <- df_bcir_l() %>% 
        mutate(city = factor(city, levels = sel_order_sm$city, ordered = TRUE)) %>% 
        ggplot(aes(x=city, y = minutes, fill = source, text = paste("City:", city, "\nMinutes:", minutes)), group = city) +
      geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
      # scale_fill_manual(values = col_reg) +
      scale_x_discrete() +
      xlab("City") +
      ylab("Minutes") +
      ggtitle("How long do you have to work with a average hourly net income for a beer") +
      mytheme
      
      ggplotly(p_18, tooltip = "text") 
    }
    else if(input$sort_by == "Overall prices"){
      p_18 <- df_bcir_l() %>% 
        mutate(city = factor(city, levels = sel_order_oa$city, ordered = TRUE)) %>% 
        ggplot(aes(x=city, y = minutes, fill = source, text = paste("City:", city, "\nMinutes:", minutes)), group = city) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        # scale_fill_manual(values = col_reg) +
        scale_x_discrete() +
        xlab("City") +
        ylab("Minutes") +
        ggtitle("How long do you have to work with a average hourly net income for a beer") +
        mytheme
      
      ggplotly(p_18, tooltip = "text") 
    }
          })
    
    
      

    
    # Country Level -----
    
    # Table with the Data -----
    output$table_country <- renderDataTable({DT::datatable(df_bco, filter = "top", extensions = c('Buttons', 'Scroller'), 
                                                           options = list(scrollY = 650,
                                                                          scrollX = 500,
                                                                          deferRender = TRUE,
                                                                          scroller = TRUE))})
    
    
   
    
    col_reg <- c( "Europe & Central Asia" = "green3", "East Asia & Pacific" = "chocolate1", "Latin America & Caribbean" = "gold2", "North America" = "darkgoldenrod3", "Middle East & North Africa" = "deepskyblue", "South Asia" = "orchid", "Sub-Saharan Africa" = "red") 
    
# Price Section ----    
    
    # un/select all countries price
    observe({
      if (input$un_selectall_cco_pr%%2== 0)
      {
        updateCheckboxGroupInput(session, "c_countries_pr","Show following Countries:",choices=sort(unique(df_bco$country)), selected = df_bco$country, inline=T)
      }
      else if(input$un_selectall_cco_pr == 0) return(NULL)
      else
      {
        updateCheckboxGroupInput(session,"c_countries_pr","Show following Countries:",choices=sort(unique(df_bco$country)), inline=T) #use sort and unique that every country is only once shown
      }
    })
    
    # un/select all regions price
    observe({
      if (input$un_selectall_cre_pr%%2== 0)
      {
        updateCheckboxGroupInput(session, "c_regions_pr","Show following Regions:",choices=sort(unique(df_bco$region)), selected = df_bco$region, inline=T)
      }
      else if(input$un_selectall_cre_pr == 0) return(NULL)
      else
      {
        updateCheckboxGroupInput(session,"c_regions_pr","Show following Regions:",choices=sort(unique(df_bco$region)), inline=T) #use sort and unique that every country is only once shown
      }
    })
    
    df_bcor_pr <- reactive({
      dplyr::filter(df_bco, country %in% input$c_countries_pr & region %in% input$c_regions_pr)
    })
    
    
    
    # Average price in a bar (Country level)
    output$plot5 <-  renderPlotly ({
      p_5 <- ggplot(df_bcor_pr(), aes(x=reorder(country, -abp), y = abp, fill = region, text = paste("Country:", country, "\nPrice:", abp, "$"))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        scale_x_discrete() +
        xlab("Country") +
        ylab("$") +
        ggtitle("Price for a 33cl beer in a hotel bar in $ in selected cities") +
        mytheme
      
      ggplotly(p_5, tooltip = "text") 
    })
    
    # Average price in a supermarket (country level)
    output$plot6 <-  renderPlotly ({
      p_6 <- ggplot(df_bcor_pr(), aes(x=reorder(country, -asmp), y = asmp, fill = region, text = paste("City:", country, "\nPrice:", asmp, "$"))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        scale_x_discrete() +
        xlab("Country") +
        ylab("$") +
        ggtitle("Price for a 33cl beer in a supermarket in $ in selected cities") +
        mytheme
      
      ggplotly(p_6, tooltip = "text") 
    })
    
    # Overall average price (country level)
    output$plot7 <-  renderPlotly ({
      p_7 <- ggplot(df_bcor_pr(), aes(x=reorder(country, -aop), y = aop, fill = region, text = paste("Country:", country, "\nPrice:", aop, "$"))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        scale_x_discrete() +
        xlab("Country") +
        ylab("$") +
        ggtitle("Overall price for a 33cl beer in $ in selected cities") +
        mytheme
      
      ggplotly(p_7, tooltip = "text") 
    })
    
    # Markup (Coutry level)
    output$plot8 <-  renderPlotly ({
      p_8 <- ggplot(df_bcor_pr(), aes(x=reorder(country, -markup), y = markup, fill = region, text = paste("Country:", country, "\nRatio:", round(markup,2)))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        scale_x_discrete() +
        xlab("Country") +
        ylab("Ratio") +
        ggtitle("How many supermarket beers could you buy for the price of one hotel bar beer") +
        mytheme
      
      ggplotly(p_8, tooltip = "text") 
    })

    # Price vs. Consumption section ----
    
    # un/select all countries PvC
    observe({
      if (input$un_selectall_cco_pvc%%2== 0)
      {
        updateCheckboxGroupInput(session, "c_countries_pvc","Show following Countries:",choices=sort(unique(df_bco$country)), selected = df_bco$country, inline=T)
      }
      else if(input$un_selectall_cco_pvc == 0) return(NULL)
      else
      {
        updateCheckboxGroupInput(session,"c_countries_pvc","Show following Countries:",choices=sort(unique(df_bco$country)), inline=T) #use sort and unique that every country is only once shown
      }
    })
    
    # un/select all regions PvC
    observe({
      if (input$un_selectall_cre_pvc%%2== 0)
      {
        updateCheckboxGroupInput(session, "c_regions_pvc","Show following Regions:",choices=sort(unique(df_bco$region)), selected = df_bco$region, inline=T)
      }
      else if(input$un_selectall_cre_pvc == 0) return(NULL)
      else
      {
        updateCheckboxGroupInput(session,"c_regions_pvc","Show following Regions:",choices=sort(unique(df_bco$region)), inline=T) #use sort and unique that every country is only once shown
      }
    })
    
    df_bcor_pvc <- reactive({
      dplyr::filter(df_bco, country %in% input$c_countries_pvc & region %in% input$c_regions_pvc)
    })
    

# Consumption vs Price goEuro    
ge_max <- max(df_bco$aop) # max for x aes
    
output$plot9 <-  renderPlotly ({
  p_9 <- ggplot(df_bcor_pvc(), aes(x = aop, y=bpc_ge, group=1,  label=label, text = paste("Country:", country, "\nConsumption:", round(bpc_ge,2), "\nPrice:", round(aop,2)))) + #group=1 has to be added that geom_smooth is shown https://stackoverflow.com/questions/45948926/ggplotly-text-aesthetic-causing-geom-line-to-not-display
    geom_point() +
    geom_text(family="OpenSansEmoji", size=6) +
    geom_smooth(method = "lm", se=FALSE) +
    xlab("Overall price for a 33cl beer in USD") +
    ylab("Beer per capita per year in Liters") +
    scale_x_continuous(breaks = seq(0, ge_max, 1),
                       limits = c(0, ge_max+1), 
                       expand = c(0,0)) +
    ggtitle("Comparing beer prices and consumption (GoEuro data)") +
    mytheme

  
  ggplotly(p_9, tooltip = "text") 
})

# Consumption vs Price GHO
gho_max <- max(df_bco$aop, na.rm = T)

output$plot10 <-  renderPlotly ({
  p_10 <- ggplot(df_bcor_pvc(), aes(x = aop, y=bpc_gho, group=1, label=label, text = paste("Country:", country, "\nConsumption:", round(bpc_gho,2), "\nPrice:", round(aop,2)))) + #group=1 has to be added that geom_smooth is shown https://stackoverflow.com/questions/45948926/ggplotly-text-aesthetic-causing-geom-line-to-not-display
    geom_point() +
    geom_text(family="OpenSansEmoji", size=6) +
    geom_smooth(method = "lm", se=FALSE) +
    xlab("Overall price for a 33cl beer in USD") +
    ylab("Beer per capita per year in litres of pure alcohol") +
    scale_x_continuous(breaks = seq(0, gho_max, 1),
                       limits = c(0, gho_max+1), 
                       expand = c(0,0)) +
    ggtitle("Comparing beer prices and consumption (GHO data)") +
    mytheme
  
  
  ggplotly(p_10, tooltip = "text") 
})

#Price vs. Consumption wikipedia
wiki_max <- max(df_bco$aop, na.rm = T) # max for x aes

output$plot11 <-  renderPlotly ({
  p_11 <- ggplot(df_bcor_pvc(), aes(x = aop, y=bpc_wiki,  group=1,  label=label, text = paste("Country:", country, "\nConsumption:", round(bpc_wiki,2), "\nPrice:", round(aop,2)))) + #group=1 has to be added that geom_smooth is shown https://stackoverflow.com/questions/45948926/ggplotly-text-aesthetic-causing-geom-line-to-not-display
    geom_point() +
    geom_text(family="OpenSansEmoji", size=6) +
    geom_smooth(method = "lm", se=FALSE) +
    xlab("Overall price for a 33cl beer in USD") +
    ylab("Beer per capita per year in Liters") +
    scale_x_continuous(breaks = seq(0, wiki_max, 1),
                       limits = c(0, wiki_max+1), 
                       expand = c(0,0)) +
    ggtitle("Comparing beer prices and consumption (Wikipedia data)") +
    mytheme
  
  
  ggplotly(p_11, tooltip = "text") 
})

# Beer buying section ----
# un/select all countries beer buying power
observe({
  if (input$un_selectall_cco_bbp%%2== 0)
  {
    updateCheckboxGroupInput(session, "c_countries_bbp","Show following Countries:",choices=sort(unique(df_bco$country)), selected = df_bco$country, inline=T)
  }
  else if(input$un_selectall_cco_bbp == 0) return(NULL)
  else
  {
    updateCheckboxGroupInput(session,"c_countries_bbp","Show following Countries:",choices=sort(unique(df_bco$country)), inline=T) #use sort and unique that every country is only once shown
  }
})

# un/select all regions beer buying power
observe({
  if (input$un_selectall_cre_bbp%%2== 0)
  {
    updateCheckboxGroupInput(session, "c_regions_bbp","Show following Regions:",choices=sort(unique(df_bco$region)), selected = df_bco$region, inline=T)
  }
  else if(input$un_selectall_cre_bbp == 0) return(NULL)
  else
  {
    updateCheckboxGroupInput(session,"c_regions_bbp","Show following Regions:",choices=sort(unique(df_bco$region)), inline=T) #use sort and unique that every country is only once shown
  }
})

df_bcor_bbp <- reactive({
  dplyr::filter(df_bco, country %in% input$c_countries_bbp & region %in% input$c_regions_bbp)
})

# Beer buying power at a bar
output$plot12 <-  renderPlotly ({
  p_12 <- ggplot(df_bcor_bbp(), aes(x=reorder(country, -be_gni_bp), y = be_gni_bp, fill = region, text = paste("Country:", country, "\nBeer buying Power:", be_gni_bp))) +
    geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
    scale_fill_manual(values = col_reg) +
    scale_x_discrete() +
    xlab("Country") +
    ylab("Amount of 33cl beer") +
    ggtitle("How many beers could you buy when you spend the 2015 average GNI at a hotel bar") +
    mytheme
  
  ggplotly(p_12, tooltip = "text") 
})

# # Beer buying power at the supermarket
output$plot13 <-  renderPlotly ({
  p_13 <- ggplot(df_bcor_bbp(), aes(x=reorder(country, -be_gni_sm), y=be_gni_sm, fill = region, text = paste("Country:", country, "\nBeer buying Power:", be_gni_sm))) +
    geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
    scale_fill_manual(values = col_reg) +
    scale_x_discrete() +
    xlab("Country") +
    ylab("Amount of 33cl beer") +
    ggtitle("How many beers could you buy when you spend the 2015 average GNI in a supermarket") +
    mytheme

  ggplotly(p_13, tooltip = "text")
})
# 
# Beer buying power overall
output$plot14 <-  renderPlotly ({
  p_14 <- ggplot(df_bcor_bbp(), aes(x=reorder(country, -be_gni_op), y = be_gni_op, fill = region, text = paste("County:", country, "\nBeer buying Power:", be_gni_op))) +
    geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
    scale_fill_manual(values = col_reg) +
    scale_x_discrete() +
    xlab("Country") +
    ylab("Amount of 33cl beer") +
    ggtitle("How many beers could you buy when you spend the 2015 average GNI overall (spend half in a hotel bar and half in a supermarket)") +
    mytheme

  ggplotly(p_14, tooltip = "text")
})

# Consumption section ----

# un/select all countries consumption
observe({
  if (input$un_selectall_cco_con%%2== 0)
  {
    updateCheckboxGroupInput(session, "c_countries_con","Show following Countries:",choices=sort(unique(df_bco$country)), selected = df_bco$country, inline=T)
  }
  else if(input$un_selectall_cco_con == 0) return(NULL)
  else
  {
    updateCheckboxGroupInput(session,"c_countries_con","Show following Countries:",choices=sort(unique(df_bco$country)), inline=T) #use sort and unique that every country is only once shown
  }
})

# un/select all regions consumption
observe({
  if (input$un_selectall_cre_con%%2== 0)
  {
    updateCheckboxGroupInput(session, "c_regions_con","Show following Regions:",choices=sort(unique(df_bco$region)), selected = df_bco$region, inline=T)
  }
  else if(input$un_selectall_cre_con == 0) return(NULL)
  else
  {
    updateCheckboxGroupInput(session,"c_regions_con","Show following Regions:",choices=sort(unique(df_bco$region)), inline=T) #use sort and unique that every country is only once shown
  }
})

df_bcor_con <- reactive({
  dplyr::filter(df_bco, country %in% input$c_countries_con & region %in% input$c_regions_con)
})

# Beer consumption goEuro
output$plot15 <-  renderPlotly ({
  p_15 <- ggplot(df_bcor_con(), aes(x=reorder(country, -bpc_ge), y = bpc_ge, fill = region, text = paste("Country:", country, "\nBeer per Capita in L:", bpc_ge))) +
    geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
    scale_fill_manual(values = col_reg) +
    scale_x_discrete() +
    xlab("Country") +
    ylab("Liters") +
    ggtitle("Beer consumption per capita per year in liters (GoEuro data)") +
    mytheme
  
  ggplotly(p_15, tooltip = "text") 
})

# # Beer consumption GHO
output$plot16 <-  renderPlotly ({
  p_16 <- ggplot(df_bcor_con(), aes(x=reorder(country, -bpc_gho), y = bpc_gho, fill = region, text = paste("Country:", country, "\nBeer per Capita in pure alcohol:", bpc_gho))) +
    geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
    scale_fill_manual(values = col_reg) +
    scale_x_discrete() +
    xlab("Country") +
    ylab("Liter") +
    ggtitle("Beer consumption per capita in litres of pure alcohol (GHO data)") +
    mytheme
  
  ggplotly(p_16, tooltip = "text") 
})
# 
#  Beer consumption wiki
output$plot17 <-  renderPlotly ({
  p_17 <- ggplot(df_bcor_con(), aes(x=reorder(country, -bpc_wiki), y = bpc_wiki, fill = region, text = paste("Country:", country, "\nBeer per Capita in L:", bpc_wiki))) +
    geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
    scale_fill_manual(values = col_reg) +
    scale_x_discrete() +
    xlab("Country") +
    ylab("Liters") +
    ggtitle("Beer consumption per capita per year in liters (Wikipedia data)") +
    mytheme
  
  ggplotly(p_17, tooltip = "text") 
})




}

shinyApp(ui, server)
