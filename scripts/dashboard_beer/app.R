
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

df_bci <- read_delim("../../processed-data/wwbp_city_add_var.csv", delim = ";" , locale = locale(encoding = 'ISO-8859-2'))
df_bco <- read_delim("../../processed-data/wwbp_country_add_var.csv", delim = ";" , locale = locale(encoding = 'ISO-8859-2')) %>% 
  mutate(label = c(emoji('beer')))
df_br <- read_delim("../../processed-data/wwbp_region_add_var.csv", delim = ";" , locale = locale(encoding = 'ISO-8859-2'))


# Define UI for application that draws a histogram
ui <- dashboardPage(
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
                ".action-button{ 
                    margin-left: 10px;
                    margin-top: -10px;
                    margin-bottom: 10px;
                    }",
                ".control-label{ 
                    margin-left: 10px;
                    }",

              ))))         
        ),
    dashboardBody(
      # define pages for sidebar
      tabItems(
        tabItem(tabName = "Charts_ci",
                fluidRow(
                  checkboxGroupInput(inputId = "d_cc", label = "Show following Cities:", df_bci$cc, inline = T),
                  actionButton("un_selectall_dci","Un/Select All"),  # action Links to select and unselect all (reactiv function in server, idea from https://stackoverflow.com/questions/28829682/r-shiny-checkboxgroupinput-select-all-checkboxes-by-click)
                  
                  
                  
                  checkboxGroupInput(inputId = "d_countries",label = "Show cities in the following countries:", df_bci$country, inline = T),
                  actionButton("un_selectall_dco","Un/Select All"),
                  
                  
                  checkboxGroupInput(inputId = "d_regions",label = "Show cities in the following Regions:", df_bci$region, inline = T),
                  actionButton("un_selectall_dre","Un/Select All"),
                  
                  
                  tabBox(id = "cp", width=12,  #width of max 12 colums, hight not sure...
                         tabPanel("Bar Prices", plotlyOutput("plot1")),
                         tabPanel("Supermarket Prices", plotlyOutput("plot2")),
                         tabPanel("Overall Prices", plotlyOutput("plot3")),
                         tabPanel("Markup", plotlyOutput("plot4"))
                  ))),
        
        tabItem(tabName = "Data_ci", width =8,
                  fluidRow(
                    dataTableOutput("table_city")
                  )
          ),
        tabItem(tabName = "Charts_co",
                fluidRow(
                  
                  checkboxGroupInput(inputId = "c_countries",label = "Show cities in the following countries:", df_bco$country, inline = T),
                  actionButton("un_selectall_cco","Un/Select All"),
                  
                  checkboxGroupInput(inputId = "c_regions",label = "Show cities in the following Regions:", df_bco$region, inline = T),
                  actionButton("un_selectall_cre","Un/Select All"),
                  
                  
                  tabBox(id = "cp", width=12,   #width of max 12 colums, hight not sure...
                         tabPanel("Bar Prices", plotlyOutput("plot5")),
                         tabPanel("Supermarket Prices", plotlyOutput("plot6")),
                         tabPanel("Overall Prices", plotlyOutput("plot7")),
                         tabPanel("Markup", plotlyOutput("plot8")),
                         tabPanel("Beer Consumption (goEurope)", plotlyOutput("plot15")),
                         tabPanel("Beer Consumption (GHO)", plotlyOutput("plot16")),
                         tabPanel("Beer Consumption (Wikipedia)", plotlyOutput("plot17")),
                         tabPanel("Price vs. Consumption (goEurope)", plotlyOutput("plot9")),
                         tabPanel("Price vs. Consumption (GHO)", plotlyOutput("plot10")),
                         tabPanel("Price vs. Consumption (Wikipedia)", plotlyOutput("plot11")),
                         tabPanel("Beer buying Power Bar", plotlyOutput("plot12")),
                         tabPanel("Beer buying Power Supermaket", plotlyOutput("plot13")),
                         tabPanel("Beer buying Power Overall", plotlyOutput("plot14"))
                  ))),
        
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
  menuItem("City Level", tabName = "city_side", icon = icon("city"),
           menuSubItem("Charts", tabName = "Charts_ci", icon = icon("")),
           menuSubItem("Data", tabName = "Data_ci", icon = icon(""))),
  
  menuItem("Country Level", tabName = "country_side", icon = icon("flag"),
           menuSubItem("Charts", tabName = "Charts_co", icon = icon("data")),
           menuSubItem("Data", tabName = "Data_co", icon = icon("chart")))
)
    })
  
  
  # City Level
  
  
  # Table with the Data
  output$table_city <- renderDataTable({DT::datatable(df_bci, filter = "top", extensions = c('Buttons', 'Scroller'), 
                                                      options = list(scrollY = 650,
                                                                     scrollX = 500,
                                                                     deferRender = TRUE,
                                                                      scroller = TRUE))}) # https://stackoverflow.com/questions/59681856/r-large-datatable-display-in-shiny
  
  
  # un/select all cities
  observe({
    if (input$un_selectall_dci%%2 == 0) #don't know how and why this works...
    {
      updateCheckboxGroupInput(session, "d_cc","Show following Cities:",choices=df_bci$cc, selected = df_bci$cc, inline=T)
    }
    else if(input$un_selectall_dci == 0) return(NULL)
    else
    {
      updateCheckboxGroupInput(session,"d_cc","Show following Cities:",choices=df_bci$cc, inline=T)
    }
  })

  # un/select all countries
  observe({
    if (input$un_selectall_dco%%2== 0)
    {
      updateCheckboxGroupInput(session, "d_countries","Show following Countries:",choices=sort(unique(df_bci$country)), selected = df_bci$country, inline=T)
    }
    else if(input$un_selectall_dco == 0) return(NULL)
    else
    {
      updateCheckboxGroupInput(session,"d_countries","Show following Countries:",choices=sort(unique(df_bci$country)), inline=T) #use sort and unique that every country is only once shown
    }
  })

  # un/select all regions
  observe({
    if (input$un_selectall_dre%%2== 0)
    {
      updateCheckboxGroupInput(session, "d_regions","Show following Regions:",choices=sort(unique(df_bci$region)), selected = df_bci$region, inline=T)
    }
    else if(input$un_selectall_dre == 0) return(NULL)
    else
    {
      updateCheckboxGroupInput(session,"d_regions","Show following Regions:",choices=sort(unique(df_bci$region)), inline=T) #use sort and unique that every country is only once shown
    }
  })

    df_bcir <- reactive({
        dplyr::filter(df_bci, cc %in% input$d_cc & country %in% input$d_countries & region %in% input$d_regions)
    })
    
    col_reg <- c( "Europe & Central Asia" = "green3", "East Asia & Pacific" = "chocolate1", "Latin America & Caribbean" = "gold2", "North America" = "darkgoldenrod3", "Middle East & North Africa" = "deepskyblue", "South Asia" = "orchid", "Sub-Saharan Africa" = "red") 
    
  
    # Average price in a bar (City level)
    output$plot1 <-  renderPlotly ({
    p_1 <- ggplot(df_bcir(), aes(x=reorder(cc, -abp), y = abp, fill = region, text = paste("City:", cc, "\nPrice:", abp, "$"))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        scale_x_discrete() +
        xlab("City") +
        ylab("$") +
        ggtitle("Price for a 0.33L beer in a bar in USD in selected cities") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")
    
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
        ggtitle("Price for a 0.33L beer in a supermarket in USD in selected cities") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")
      
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
        ggtitle("Overall price for a 0.33L beer in USD in selected cities") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")
      
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
        ggtitle("How much more does a beer cost in the bar than in the supermarket in selected cities") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")
      
      ggplotly(p_4, tooltip = "text") 
    })
    
 
    
    
    # Country Level
    
    # Table with the Data
    output$table_country <- renderDataTable({DT::datatable(df_bco, filter = "top", extensions = c('Buttons', 'Scroller'), 
                                                           options = list(scrollY = 650,
                                                                          scrollX = 500,
                                                                          deferRender = TRUE,
                                                                          scroller = TRUE))})
    
    
    # un/select all countries
    observe({
      if (input$un_selectall_cco%%2== 0)
      {
        updateCheckboxGroupInput(session, "c_countries","Show following Countries:",choices=sort(unique(df_bco$country)), selected = df_bco$country, inline=T)
      }
      else if(input$un_selectall_cco == 0) return(NULL)
      else
      {
        updateCheckboxGroupInput(session,"c_countries","Show following Countries:",choices=sort(unique(df_bco$country)), inline=T) #use sort and unique that every country is only once shown
      }
    })

    # un/select all regions
    observe({
      if (input$un_selectall_cre%%2== 0)
      {
        updateCheckboxGroupInput(session, "c_regions","Show following Regions:",choices=sort(unique(df_bco$region)), selected = df_bco$region, inline=T)
      }
      else if(input$un_selectall_cre == 0) return(NULL)
      else
      {
        updateCheckboxGroupInput(session,"c_regions","Show following Regions:",choices=sort(unique(df_bco$region)), inline=T) #use sort and unique that every country is only once shown
      }
    })

    
    
    df_bcor <- reactive({
      dplyr::filter(df_bco, country %in% input$c_countries & region %in% input$c_regions)
    })
    
    col_reg <- c( "Europe & Central Asia" = "green3", "East Asia & Pacific" = "chocolate1", "Latin America & Caribbean" = "gold2", "North America" = "darkgoldenrod3", "Middle East & North Africa" = "deepskyblue", "South Asia" = "orchid", "Sub-Saharan Africa" = "red") 
    
    # Average price in a bar (Country level)
    output$plot5 <-  renderPlotly ({
      p_5 <- ggplot(df_bcor(), aes(x=reorder(country, -abp), y = abp, fill = region, text = paste("Country:", country, "\nPrice:", abp, "$"))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        scale_x_discrete() +
        xlab("Country") +
        ylab("$") +
        ggtitle("Price for a 0.33L beer in a bar in USD in selected cities") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")
      
      ggplotly(p_5, tooltip = "text") 
    })
    
    # Average price in a supermarket (country level)
    output$plot6 <-  renderPlotly ({
      p_6 <- ggplot(df_bcor(), aes(x=reorder(country, -asmp), y = asmp, fill = region, text = paste("City:", country, "\nPrice:", asmp, "$"))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        scale_x_discrete() +
        xlab("Country") +
        ylab("$") +
        ggtitle("Price for a 0.33L beer in a supermarket in USD in selected cities") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")
      
      ggplotly(p_6, tooltip = "text") 
    })
    
    # Overall average price (country level)
    output$plot7 <-  renderPlotly ({
      p_7 <- ggplot(df_bcor(), aes(x=reorder(country, -aop), y = aop, fill = region, text = paste("Country:", country, "\nPrice:", aop, "$"))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        scale_x_discrete() +
        xlab("Country") +
        ylab("$") +
        ggtitle("Overall price for a 0.33L beer in USD in selected cities") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")
      
      ggplotly(p_7, tooltip = "text") 
    })
    
    # Markup (Coutry level)
    output$plot8 <-  renderPlotly ({
      p_8 <- ggplot(df_bcor(), aes(x=reorder(country, -markup), y = markup, fill = region, text = paste("Country:", country, "\nRatio:", round(markup,2)))) +
        geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
        scale_fill_manual(values = col_reg) +
        scale_x_discrete() +
        xlab("Country") +
        ylab("Ratio") +
        ggtitle("How much more does a beer cost in the bar than in the supermarket in selected cities") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "bottom")
      
      ggplotly(p_8, tooltip = "text") 
    })

# Scaterplot

# Consumption vs Price goEurop    
ge_max <- max(df_bco$aop) # max for x aes
    
output$plot9 <-  renderPlotly ({
  p_9 <- ggplot(df_bcor(), aes(x = aop, y=bpc_ge, group=1,  label=label, text = paste("Country:", country, "\nConsumption:", round(bpc_ge,2), "\nPrice:", round(aop,2)))) + #group=1 has to be added that geom_smooth is shown https://stackoverflow.com/questions/45948926/ggplotly-text-aesthetic-causing-geom-line-to-not-display
    geom_point() +
    geom_text(family="OpenSansEmoji", size=6) +
    geom_smooth(method = "lm", se=FALSE) +
    xlab("Overall price for a 0.33L beer in USD") +
    ylab("Beer per capita in Liters") +
    scale_x_continuous(breaks = seq(0, ge_max, 1),
                       limits = c(0, ge_max+1), 
                       expand = c(0,0)) +
    ggtitle("Comparing beer prices and consumption (goEurop data)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 7)) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "bottom")

  
  ggplotly(p_9, tooltip = "text") 
})

# Consumption vs Price GHO
gho_max <- max(df_bco$aop, na.rm = T)

output$plot10 <-  renderPlotly ({
  p_10 <- ggplot(df_bcor(), aes(x = aop, y=bpc_gho, group=1, label=label, text = paste("Country:", country, "\nConsumption:", round(bpc_gho,2), "\nPrice:", round(aop,2)))) + #group=1 has to be added that geom_smooth is shown https://stackoverflow.com/questions/45948926/ggplotly-text-aesthetic-causing-geom-line-to-not-display
    geom_point() +
    geom_text(family="OpenSansEmoji", size=6) +
    geom_smooth(method = "lm", se=FALSE) +
    xlab("Overall price for a 0.33L beer in USD") +
    ylab("Beer per capita in litres of pure alcohol") +
    scale_x_continuous(breaks = seq(0, gho_max, 1),
                       limits = c(0, gho_max+1), 
                       expand = c(0,0)) +
    ggtitle("Comparing beer prices and consumption  (GHO data)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 7)) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "bottom")
  
  
  ggplotly(p_10, tooltip = "text") 
})

#Price vs. Consumption wikipedia
wiki_max <- max(df_bco$aop, na.rm = T) # max for x aes

output$plot11 <-  renderPlotly ({
  p_11 <- ggplot(df_bcor(), aes(x = aop, y=bpc_wiki,  group=1,  label=label, text = paste("Country:", country, "\nConsumption:", round(bpc_wiki,2), "\nPrice:", round(aop,2)))) + #group=1 has to be added that geom_smooth is shown https://stackoverflow.com/questions/45948926/ggplotly-text-aesthetic-causing-geom-line-to-not-display
    geom_point() +
    geom_text(family="OpenSansEmoji", size=6) +
    geom_smooth(method = "lm", se=FALSE) +
    xlab("Overall price for a 0.33L beer in USD") +
    ylab("Beer per capita in Liters") +
    scale_x_continuous(breaks = seq(0, wiki_max, 1),
                       limits = c(0, wiki_max+1), 
                       expand = c(0,0)) +
    ggtitle("Comparing beer prices and consumption (Wikipedia data)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 7)) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "bottom")
  
  
  ggplotly(p_11, tooltip = "text") 
})


# Beer buying power at a bar
output$plot12 <-  renderPlotly ({
  p_12 <- ggplot(df_bcor(), aes(x=reorder(country, -be_gni_bp), y = be_gni_bp, fill = region, text = paste("Country:", country, "\nBeer buying Power:", be_gni_bp))) +
    geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
    scale_fill_manual(values = col_reg) +
    scale_x_discrete() +
    xlab("Country") +
    ylab("Amount of 0.33L beer") +
    ggtitle("If you use the countries GNI per capita to only buy beer, how many beers could you buy at the bar?") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 7)) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "bottom")
  
  ggplotly(p_12, tooltip = "text") 
})

# # Beer buying power at the supermarket
output$plot13 <-  renderPlotly ({
  p_13 <- ggplot(df_bcor(), aes(x=reorder(country, -be_gni_sm), y=be_gni_sm, fill = region, text = paste("Country:", country, "\nBeer buying Power:", be_gni_sm))) +
    geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
    scale_fill_manual(values = col_reg) +
    scale_x_discrete() +
    xlab("Country") +
    ylab("Amount of 0.33L beer") +
    ggtitle("If you use the countries GNI per capita to only buy beer, how many beers could you buy at the supermarket?") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 7)) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "bottom")

  ggplotly(p_13, tooltip = "text")
})
# 
# # Beer buying power overall
output$plot14 <-  renderPlotly ({
  p_14 <- ggplot(df_bcor(), aes(x=reorder(country, -be_gni_op), y = be_gni_op, fill = region, text = paste("County:", country, "\nBeer buying Power:", be_gni_op))) +
    geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
    scale_fill_manual(values = col_reg) +
    scale_x_discrete() +
    xlab("Country") +
    ylab("Amount of 0.33L beer") +
    ggtitle("If you use the countries GNI per capita to only buy beer, how many beers could you buy at overall Prices?") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 7)) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "bottom")

  ggplotly(p_14, tooltip = "text")
})


# Beer consumption goEurop
output$plot15 <-  renderPlotly ({
  p_15 <- ggplot(df_bcor(), aes(x=reorder(country, -bpc_ge), y = bpc_ge, fill = region, text = paste("Country:", country, "\nBeer per Capita in L:", bpc_ge))) +
    geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
    scale_fill_manual(values = col_reg) +
    scale_x_discrete() +
    xlab("Country") +
    ylab("Liters") +
    ggtitle("Beer consumption per capita in Liters (goEurop data)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 7)) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "bottom")
  
  ggplotly(p_15, tooltip = "text") 
})

# # Beer consumption GHO
output$plot16 <-  renderPlotly ({
  p_16 <- ggplot(df_bcor(), aes(x=reorder(country, -bpc_gho), y = bpc_gho, fill = region, text = paste("Country:", country, "\nBeer per Capita in pure alcohol:", bpc_gho))) +
    geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
    scale_fill_manual(values = col_reg) +
    scale_x_discrete() +
    xlab("Country") +
    ylab("Liter") +
    ggtitle("Beer consumption per capita in litres of pure alcohol (GHO data)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 7)) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "bottom")
  
  ggplotly(p_16, tooltip = "text") 
})
# 
#  Beer consumption wiki
output$plot17 <-  renderPlotly ({
  p_17 <- ggplot(df_bcor(), aes(x=reorder(country, -bpc_wiki), y = bpc_wiki, fill = region, text = paste("Country:", country, "\nBeer per Capita in L:", bpc_wiki))) +
    geom_bar(stat="identity", position = position_dodge(width=0.9), width = 0.85, color = NA) +
    scale_fill_manual(values = col_reg) +
    scale_x_discrete() +
    xlab("Country") +
    ylab("Liters") +
    ggtitle("Beer consumption per capita in Liters (Wikipedia data)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 7)) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "bottom")
  
  ggplotly(p_17, tooltip = "text") 
})




}

shinyApp(ui, server)
