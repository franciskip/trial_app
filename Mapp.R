

library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(htmltools)
library(DT)
library(RColorBrewer)
library(readr)
library(reshape2)

#loading population trend file
trend<-read_csv("./www/trend.csv")
fin<-read.csv("./www/fin.csv")
#loading county data
county_data<-read_csv("./www/Kenya_census.csv")
fin1<-read.csv("./www/fins11.csv")
#fin2<-read.csv("./www/fins12.csv")
cnt<-county_data %>% melt(id.vars="County")
fnt<-fin1 %>% melt(id.vars="County")
#loading county shapefile
county_shp<-readOGR("./www/shp/county.shp")
county_shp$merge_key <- tolower(trimws(county_shp$ADM1_EN))
fin1$merge_key <- tolower(trimws(fin1$County))
merged_data <- merge(county_shp, fin1, by = "merge_key", all = TRUE)


# Define UI 
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = "KENYA Pension Uptake.", titleWidth = 400,
    tags$li(actionLink("LinkedIn", 
                       label = "", 
                       icon = icon("linkedin"),
                       onclick = "window.open('https://www.linkedin.com/in/francis-kipkogei-20058b139/')"),
            class = "dropdown"),
    tags$li(actionLink("LinkedIn", 
                       label = "", 
                       icon = icon("linkedin"),
                       onclick = "window.open('https://www.linkedin.com/in/nelson-kemboi-yego-98574325/')"),
            class = "dropdown")
  ),
  
  dashboardSidebar(
    
    sidebarUserPanel("Francis Yego & Nelson Yego",
                     subtitle = "Data Scientist"
                     ),
    sidebarMenu(
      menuItem(
        "Introduction",
        tabName = "intro",
        icon = icon("bullhorn")
      ),
      
      menuItem(
        "Data Visualization",
        tabName = "map",
        icon = icon("chart-bar")
      ),
      
      menuItem(
        "Data Set",
        tabName = "data",
        icon = icon("table")
      ),
      
      menuItem(
        "About",
        tabName = "author",
        icon = icon("user")
      )
      
    )             
    
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML('
                              .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 20px;
                              }
                              '))),
    tabItems(
      
      tabItem(
        tabName = "intro",
        
        
        fluidRow(
          
           h2(HTML("<strong>Background</strong>")),
           h3(tags$p("Pension uptake refers to the percentage of eligible individuals who are taking advantage of a pension plan or program.
                     In other words, it measures the proportion of individuals who are actively receiving retirement benefits from their employers 
                     or the government.he pension uptake in Kenya has fluctuated over the years. In 2013, there was a significant increase in pension 
                     uptake, with 11% of eligible individuals taking up pensions, which increased to 12% in 2016. However, there was a drop in pension 
                     uptake in 2018 to only 1%, which increased slightly to 2% in 2021. The low pension uptake could be due to various reasons such as a 
                     lack of information on the importance of pensions, low income levels, or limited access to pension plans. Increasing awareness and 
                     access to pension plans is crucial for individuals to secure their financial futures in retirement.")),
           plotlyOutput("lineplot",width = 1050),
           h2(HTML("<strong>Need to Study Pension Uptake.</strong>")),
           h3(tags$p("Understanding pension uptake is important because it provides insights into the level of retirement planning and financial 
                     security of a population. Pension uptake data can help governments, policymakers, and employers to identify gaps in pension 
                     coverage, and develop strategies to increase access to pension plans. It can also inform decisions on social security policies
                     and retirement benefits. Moreover, pension uptake data can be used to assess the effectiveness of existing pension programs, 
                     identify the demographics of individuals who are more likely to take up pensions, and develop targeted communication strategies 
                     to improve pension uptake. By understanding pension uptake, stakeholders can make informed decisions that ensure better financial
                     security for retirees and promote economic stability")),
           h2(HTML("<strong>Theme of Pension Uptake.</strong>")),
           h3(tags$p("importance of retirement planning and financial security. This theme would explore the reasons why individuals should plan for 
                     their retirement, including the potential consequences of not doing so. It could also examine the role of pensions in ensuring 
                     financial security in retirement, and the barriers that prevent individuals from taking up pensions. Additionally, this theme 
                     could delve into the policy and structural changes needed to increase pension uptake, such as improving access to pension 
                     plans and increasing public awareness about the importance of retirement planning. Overall, the theme of retirement planning and 
                     financial security would highlight the need for individuals, governments, and employers to prioritize retirement planning and work 
                     together to ensure financial stability in old age."))
          )
        #end fluidrow
        
          ), #end tabname
      
      tabItem(
        tabName = "map",
        
        fluidRow(

          valueBoxOutput(
             "total"

          ),#end box

          valueBoxOutput(
             "male"

          ), #end box

          valueBoxOutput(
             "female"

          ),
          
          valueBoxOutput(
             "income_box"
            
          ),
          valueBoxOutput(
             "nhif_box"
            
          ),
          
          valueBoxOutput(
            "pension_box"
            
          )

        ),
        
        fluidRow(
          column(width = 3,
                     selectInput(
                       inputId = "stats",
                       label = "Select Indicator",
                       choices =c(
                         "Sample Population"=1,
                         "Pension Usage"=2,
                         "Male Population"=3,
                         "Female Population"=4,
                         "NHIF Usage"=5,
                         "Average Probability Poverty"=6,
                         "Average Monthly Income"=7
                       ),selected = 1
                        
                     ) 
                 
                 
          ) #end column
          
          
        ),
        #end row
        fluidRow(
          column(
            width = 6,
            #box(
              # title = "MAP",
              # status = "primary",solidHeader = TRUE,
              # width = NULL,height = 600,collapsible = TRUE,
              leafletOutput("maps",height = 500)
            #)
          ),#end column
          
          column(width = 3,
                 plotlyOutput("top",height = 500)
                 ),
          
          column(width = 3,
                 plotlyOutput("bottom",height = 500)
          )
          
          
          
        )#end row
        
        
        
      ), #end tab item
      
      tabItem(
        tabName = "data",
        
        div(
          h1(strong("Dataset.")),
          h3("The table below displays the total,male,female and NHIF Usage, Pension Usage, 
              HH size, land area for each of the 47 counties in Kenya.
             ")
          ),
        dataTableOutput("table")
        ),
      tabItem(
        tabName = "author",
        fluidRow(
          br(),
          
          img(src ="profile.jpg", width = "17%", style = "display: block; margin-left: auto; margin-right: auto;")
          
        ),
        
        fluidRow(
          h3(strong("Francis Yego & Nelson Yego"), style = "text-align: center"),
          h4("francisyego4@gmail.com & nelsonyego@gmail.com ", style = "text-align: center")
        ),
        
        hr(),
        fluidRow(column(5, ""),
                 column(
                   3,
                   tags$h3(
                     HTML('&nbsp;'),
                     HTML('&nbsp;'),
                     HTML('&nbsp;'),
                     tags$a(
                       href = 'https://www.linkedin.com/in/francis-kipkogei-20058b139/',
                       img(
                         src = 'LinkedIn.png',
                         height = "50px"
                       )
                     ),
                     HTML('&nbsp;'),
                     tags$a(href = 'https://www.linkedin.com/in/nelson-kemboi-yego-98574325/', img(
                       src = 'LinkedIn.png',
                       height = "50px"
                     ))
                   )
                 )),
        
        
        fluidRow(
          column(2, ""),
          column(
            1,
            h3(icon("briefcase"), style = "text-align: right; line-height: 165%;"),
            br(),
            br(),
            h3(icon("globe"), style = "text-align: right; line-height: 200%"),
            br(),
            h3(icon("heart"), style = "text-align: right; line-height: 170%;")
          ),
          column(
            6,
            h4(
              "Data scientist|Analyst experience and familiar 
              with designing and implementing projects, gathering, 
              cleaning, and organizing data for use by technical 
              and non-technical personnel. Advanced understanding of statistical, 
              algebraic, and other analytical techniques. Highly organized,
              motivated, and diligent with significant background 
              in Data Mining, Data Analysis, Data Visualization, 
              Machine learning, Artificial Intelligence , 
              data intuition Mathematical and Statistical 
              Modelling. Moreover, I am proficient to an 
              advanced level in using Python, R, Julia, SQL,
              MS Excel, and Cloud computing.",
              style = "text-align: left; line-height: 150%;"
            ),
            br(),
            h4(
              "Skills
Statistical analysis, data analysis and data mining

Machine learning, Artificial Intelligence and Big Data Analytics Project coordination

Team management

Data Visualization, Dashboarding and Reporting
",
              style = "text-align: left; line-height: 150%;"
            ),
            br(),
            h4(
              "Data Analytics|Research|Impact Assessment|Foresight ",
              style = "text-align: left; line-height: 150%;"
            )
            
          ),
        
          column(3, "")
        
        
      
      )#end row
      
    )
          )#end tab items
    
    )
  
  
    )

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  
  
  
  output$lineplot<-renderPlotly({
    ggplotly(
      ggplot(data = trend,aes(x=Year,y=Pension))+
        geom_line(color="blue")+
        labs(
          title = "Pension Uptake trends from 2006 to 2021 in Kenya.",
          x="Year",
          y="Percentage of Pension Upatake."
        )+
        scale_x_continuous(breaks = c(2006,2009,2013,2016,2018,2021
                                      )) +
        geom_text(aes(label = paste0(round(Pension, 2), "%")), nudge_y = 0.5) +
        theme_minimal()  
      
    )
  })
  
  # Calculate the Sample Population
  total_pop <- sum(fin1$Population)
  
  # Calculate the male and female populations
  male_pop <- sum(fin1$Male_Population)
  female_pop <- sum(fin1$Female_Population)
  # Calculate average monthly income
  pov_avg <- mean(fin1$Avg_Probability_Pov_NPL)
  income_avg <- mean(fin1$Avg_Monthly_Income)
  # Calculate number of urban and rural observations
  urban_count <- sum(fin$Cluster.Type..rural.urban. == "Urban")
  rural_count <- sum(fin$Cluster.Type..rural.urban. == "Rural")
  
  # Calculate number of bank, NHIF, and pension users
  bank_users <- sum(fin1$Bank_Users)
  nhif_users <- sum(fin1$NHIF_users)
  pension_users <- sum(fin1$Pension_users)
  output$total<-renderValueBox({
    valueBox(
      format(total_pop, big.mark = ","), "Sample Population", icon = icon("users"),
      color = "red"
    )
  })
  
  output$male<-renderValueBox({
    valueBox(
      format(male_pop, big.mark = ","), "MALE POPULATION", icon = icon("male"),
      color = "blue"
    )
  })
  
  output$female<-renderValueBox({
    valueBox(
      format(female_pop, big.mark = ","), "FEMALE POPULATION", icon = icon("female"),
      color = "yellow"
    )
  })
  
  output$income_box<-renderValueBox({
    valueBox(
      paste0("KSh ", format(income_avg, big.mark = ",")), "AVERAGE MONTHLY INCOME", icon = icon("money"),
      color = "green"
    )
  })
  
  output$nhif_box<-renderValueBox({
    valueBox(
      paste0(format(pov_avg, big.mark = ",")), "AVERAGE PROBABILITY POVERTY INDEX", icon = icon("food"),
      color = "aqua"
    )
  })
  
  output$pension_box<-renderValueBox({
    valueBox(
      format(pension_users, big.mark = ","), "PENSION USERS", icon = icon("chart-line"),
      color = "orange"
    )
  })
  
  
  
  
  #rendering the basemap
  output$maps<-renderLeaflet(
    leaflet(merged_data) %>%
      setView(lng=37.9083,lat=0.1769,zoom = 6) %>%
      addPolygons(
        color = ~palpop(Population),
        smoothFactor = 0.5,
        weight = 2, opacity = 1.0,
        fillOpacity = 1.0,
        highlightOptions = highlightOptions(
          weight = 1,
          color = "brown",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = paste(
          "<strong>County:</strong>",merged_data$County,
          "<br>",
          "<strong>Sample Population:</strong>",merged_data$Population
          
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                  padding = "3px 8px"), 
                                     textsize = "13px", direction = "auto"),
        
        popup = ~paste(
          "<strong>County:</strong>",County,
          "<br>",
          "<strong>Sample Population:</strong>",Population
          
        )
        
      ) %>%
      addLegend(title = "Sample Population",
                pal = palpop, values = merged_data$Population, opacity = 1)
        
      )

  
  
 #color functions
  #population legend
  palpop<-colorBin("Purples", merged_data$Population)
  
  #Pension Usage
  pal2<-colorBin("Blues", merged_data$Pension_users)
  
  #male population
  pal3<-colorBin("YlOrBr", merged_data$Male_Population)
  
  #female population
  pal4<-colorBin("viridis", merged_data$Female_Population)
  
  #NHIF Usage
  pal5<-colorBin("inferno", merged_data$NHIF_users)
  
  #Number of HHs
  pal6<-colorBin("plasma", merged_data$Avg_Probability_Pov_NPL)
  
  #HH size
  pal7<-colorBin("magma", merged_data$Avg_Monthly_Income)
  
  
  observe({
    proxy<-leafletProxy("maps") %>% clearControls()
    if ("1" %in% input$stats){
      proxy %>%
        addPolygons(
          data = merged_data,
          color = ~palpop(Population),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",merged_data$County,
            "<br>",
            "<strong>Sample Population:</strong>",merged_data$Population
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          
          popup = ~paste(
            "<strong>County:</strong>",County,
            "<br>",
            "<strong>Sample Population:</strong>",Population
            
          )
          
        ) %>%
        addLegend(title = "Sample Population",
                  pal = palpop, values = merged_data$Population, opacity = 1)
    }

   else if ("2" %in% input$stats){
      proxy %>%
        addPolygons(
          data = merged_data,
          color =  ~pal2(Pension_users),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "green",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",merged_data$County,
            "<br>",
            "<strong>Pension Usage:</strong>",merged_data$Pension_users

          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",County,
            "<br>",
            "<strong>Pension Usage:</strong>",Pension_users

          )

        ) %>%
        addLegend(title = "Pension Usage",
                  pal = pal2, values = merged_data$Pension_users, opacity = 1)
    }
    
    
   else if ("3" %in% input$stats){
      proxy %>%
        addPolygons(
          data = merged_data,
          color =  ~pal3(Male_Population),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "blue",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",merged_data$County,
            "<br>",
            "<strong>Male Population:</strong>",merged_data$Male_Population
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",County,
            "<br>",
            "<strong>Male Population:</strong>",Male_Population
            
          )
          
        ) %>%
        addLegend(title = "Male Population",
                  pal = pal3, values = merged_data$Male_Population, opacity = 1)
    }
    
    
  else if ("4" %in% input$stats){
      proxy %>%
        addPolygons(
          data = merged_data,
          color =  ~pal4(Female_Population),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "aqua",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",merged_data$County,
            "<br>",
            "<strong>Female Population:</strong>",merged_data$Female_Population
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",County,
            "<br>",
            "<strong>Female Population:</strong>",Female_Population
            
          )
          
        ) %>%
        addLegend(title = "Female Population",
                  pal = pal4, values = merged_data$Female_Population, opacity = 1)
    }
    
    
  else  if ("5" %in% input$stats){
      proxy %>%
        addPolygons(
          data = merged_data,
          color =  ~pal5(NHIF_users),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",merged_data$County,
            "<br>",
            "<strong>NHIF Usage:</strong>",merged_data$NHIF_users
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",County,
            "<br>",
            "<strong>NHIF Usage:</strong>",NHIF_users
            
          )
          
        ) %>%
        addLegend(title = "NHIF Usage",
                  pal = pal5, values = merged_data$NHIF_users, opacity = 1)
    }
    
    
   else if ("6" %in% input$stats){
      proxy %>%
        addPolygons(
          data = merged_data,
          color =  ~pal6(Avg_Probability_Pov_NPL),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",merged_data$County,
            "<br>",
            "<strong>Average Probability Poverty:</strong>",merged_data$Avg_Probability_Pov_NPL
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",County,
            "<br>",
            "<strong>Average Probability Poverty:</strong>",Avg_Probability_Pov_NPL
            
          )
          
        ) %>%
        addLegend(title = "Average Probability Poverty",
                  pal = pal6, values = merged_data$Avg_Probability_Pov_NPL, opacity = 1)
    }
    

    
   else if ("7" %in% input$stats){
      proxy %>%
        addPolygons(
          data = merged_data,
          color =  ~pal7(Avg_Monthly_Income),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",merged_data$County,
            "<br>",
            "<strong>Average Monthly Income:</strong>",merged_data$Avg_Monthly_Income
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",ADM1_EN,
            "<br>",
            "<strong>Average Monthly Income:</strong>",Avg_Monthly_Income
            
          )
          
        ) %>%
        addLegend(title = "Average Monthly Income",
                  pal = pal7, values = merged_data$Avg_Monthly_Income, opacity = 1)
    }
    
  })
  
  
  
  
  output$top<-renderPlotly({
    if("1" %in% input$stats){
      fin1 %>% select(County,Population) %>%
        arrange(desc(Population)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Population),y=Population))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Population))+
        labs(
          title = "Top 5 counties",
          y="Sample Population",
          x="County"
        )+
        coord_flip()
  
    }
    
   else if("2" %in% input$stats){
     fin1 %>% select(County,Pension_users) %>%
        arrange(desc(Pension_users)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Pension_users),y=Pension_users))+
        geom_col(fill="#90EE90")+
       geom_text(aes(label=Pension_users))+
        labs(
          title = "Top 5 counties",
          y="Pension Usage",
          x="County"
        )+
        coord_flip()
   }
    
    else if("3" %in% input$stats){
      fin1 %>% select(County,Male_Population) %>%
        arrange(desc(Male_Population)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Male_Population),y=Male_Population))+
        geom_col(fill="#ADD8E6")+
        geom_text(aes(label=Male_Population))+
        labs(
          title = "Top 5 counties",
          y="Male Population",
          x="County"
        )+
        coord_flip()
      
    }  
    
    else if("4" %in% input$stats){
      fin1 %>% select(County,Female_Population) %>%
        arrange(desc(Female_Population)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Female_Population),y=Female_Population))+
        geom_col(fill="#0000FF")+
        geom_text(aes(label=Female_Population))+
        labs(
          title = "Top 5 counties",
          y="Female Population",
          x="County"
        )+
        coord_flip()
      
    }  
    
    else if("5" %in% input$stats){
      fin1 %>% select(County,NHIF_users) %>%
        arrange(desc(NHIF_users)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,NHIF_users),y=NHIF_users))+
        geom_col(fill="#ADD8E6")+
        geom_text(aes(label=NHIF_users))+
        labs(
          title = "Top 5 counties",
          y="NHIF Usage",
          x="County"
        )+
        coord_flip()
      
    }  
    
    else if("6" %in% input$stats){
      fin1 %>% select(County,Avg_Probability_Pov_NPL) %>%
        arrange(desc(Avg_Probability_Pov_NPL)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Avg_Probability_Pov_NPL),y=Avg_Probability_Pov_NPL))+
        geom_col(fill="#90EE90")+
        geom_text(aes(label=Avg_Probability_Pov_NPL))+
        labs(
          title = "Top 5 counties",
          y="Average Probability Poverty",
          x="County"
        )+
        coord_flip()
      
    }  
    
    else if("7" %in% input$stats){
      fin1 %>% select(County,Avg_Monthly_Income) %>%
        arrange(desc(Avg_Monthly_Income)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Avg_Monthly_Income),y=Avg_Monthly_Income))+
        geom_col(fill="#7b2cbf")+
        geom_text(aes(label=Avg_Monthly_Income))+
        labs(
          title = "Top 5 counties",
          y="Average Monthly Income",
          x="County"
        )+
        coord_flip()
      
    }  
    
   
  })
  
  
  output$bottom<-renderPlotly({
    if("1" %in% input$stats){
      fin1 %>% select(County,Population) %>%
        arrange(desc(Population)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Population),y=Population))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Population))+
        labs(
          title = "Bottom 5 counties",
          y="Sample Population",
          x="County"
        )+
        coord_flip()
      
    }
    
    else if("2" %in% input$stats){
      fin1 %>% select(County,Pension_users) %>%
        arrange(desc(Pension_users)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Pension_users),y=Pension_users))+
        geom_col(fill="#90EE90")+
        geom_text(aes(label=Pension_users))+
        labs(
          title = "Bottom 5 counties",
          y="Pension Usage",
          x="County"
        )+
        coord_flip()
    }
    
    else if("3" %in% input$stats){
      fin1 %>% select(County,Male_Population) %>%
        arrange(desc(Male_Population)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Male_Population),y=Male_Population))+
        geom_col(fill="#ADD8E6")+
        geom_text(aes(label=Male_Population))+
        labs(
          title = "Bottom 5 counties",
          y="Male Population",
          x="County"
        )+
        coord_flip()
      
    }
    
    else if("4" %in% input$stats){
      fin1 %>% select(County,Female_Population) %>%
        arrange(desc(Female_Population)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Female_Population),y=Female_Population))+
        geom_col(fill="#0000FF")+
        geom_text(aes(label=Female_Population))+
        labs(
          title = "Bottom 5 counties",
          y="Female Population",
          x="County"
        )+
        coord_flip()
      
    }
    
    else if("5" %in% input$stats){
      fin1 %>% select(County,NHIF_users) %>%
        arrange(desc(NHIF_users)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,NHIF_users),y=NHIF_users))+
        geom_col(fill="#ADD8E6")+
        geom_text(aes(label=NHIF_users))+
        labs(
          title = "Bottom 5 counties",
          y="NHIF Usage",
          x="County"
        )+
        coord_flip()
      
    }
    
    else if("6" %in% input$stats){
      fin1 %>% select(County,Avg_Probability_Pov_NPL) %>%
        arrange(desc(Avg_Probability_Pov_NPL)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Avg_Probability_Pov_NPL),y=Avg_Probability_Pov_NPL))+
        geom_col(fill="#90EE90")+
        geom_text(aes(label=Avg_Probability_Pov_NPL))+
        labs(
          title = "Bottom 5 counties",
          y="Average Probability Poverty",
          x="County"
        )+
        coord_flip()
      
    }
    
    else if("7" %in% input$stats){
      fin1 %>% select(County,Avg_Monthly_Income) %>%
        arrange(desc(Avg_Monthly_Income)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Avg_Monthly_Income),y=Avg_Monthly_Income))+
        geom_col(fill="#7b2cbf")+
        geom_text(aes(label=Avg_Monthly_Income))+
        labs(
          title = "Bottom 5 counties",
          y="Average Monthly Income",
          x="County"
        )+
        coord_flip()
      
    }  
    
  })
  

  
  output$table<-renderDataTable({
   datatable( fin,
    class = 'cell-border stripe',
    editable = TRUE,
    options = list(scrollX = T)
   ) 
  }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
#: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black
