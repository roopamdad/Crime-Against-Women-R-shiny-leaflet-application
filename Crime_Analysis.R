#Loading required libraries
library(shiny) #for shiny app
library(leaflet) #to generate leaflet
library(RColorBrewer) # colour palete
library(rgdal)# for importing shape files
library(tfplot) #to calculate percentage change
library(shinydashboard) #to create shiny dashboard
library(plotly) #to generate plots
library(reshape2) #for melting dataframe

#Importing Shape files
shp_city <- readOGR("Data/shapefile/IND_adm2.shp") #shape file for cities
shp_state <- readOGR("Data/shapefile/IND_adm1.shp") #shape file for cities
shp_state1 <- readOGR("Data/shapefile/IND_adm1.shp") #shape file for inidian states

#Importing Dataframes
# From 2001-2015
crime_total <- read.csv("Data/csv_total.csv")

#Detailed Study of 2015
crime_2015 <- read.csv("Data/Table_5.2-2015.csv")

#dividing dataframe into incidents and rates
#crime_2015 incidents
seq_inci <- seq(4,83,2)
crime_incidents <- crime_2015[c(2,seq_inci)]

#crime_2015 rate
seq_rate <- seq(5,83,2)
crime_rate <- crime_2015[c(2,seq_rate)]

#Police Data 2016
police_2016 <- read.csv("Data/Table_3A.5_2016.csv")

#Court Data 2016
court_2016 <- read.csv("Data/Table_3A.7_2016.csv")

#importing national crime bureau data for crime incidents rate and percentage of crime for each state and crime from 2001 -2011
crime_2001_2011 <- read.csv("Data/NCRB_2001 - 2011.csv")

#aggregating the crime data on state for every year

crime_state_agg <- crime_total%>%
  group_by(STATE.UT,Year)%>%
  summarise(Rape =sum(Rape),
            Kidnapping.and.Abduction = sum(Kidnapping.and.Abduction),
            Dowry.Deaths=sum(Dowry.Deaths),
            Assault.on.women.with.intent.to.outrage.her.modesty = sum(Assault.on.women.with.intent.to.outrage.her.modesty),
            Insult.to.modesty.of.Women=sum(Insult.to.modesty.of.Women),
            Cruelty.by.Husband.or.his.Relatives=sum(Cruelty.by.Husband.or.his.Relatives),
            Importation.of.Girls=sum(Importation.of.Girls),
            Total.Crime=sum(Total.Crime))

##aggregating the crime data on city for every year

crime_city_agg <- crime_total%>%
  group_by(DISTRICT,Year)%>%
  summarise(Rape =sum(Rape),
            Kidnapping.and.Abduction = sum(Kidnapping.and.Abduction),
            Dowry.Deaths=sum(Dowry.Deaths),
            Assault.on.women.with.intent.to.outrage.her.modesty = sum(Assault.on.women.with.intent.to.outrage.her.modesty),
            Insult.to.modesty.of.Women=sum(Insult.to.modesty.of.Women),
            Cruelty.by.Husband.or.his.Relatives=sum(Cruelty.by.Husband.or.his.Relatives),
            Importation.of.Girls=sum(Importation.of.Girls),
            Total.Crime=sum(Total.Crime))

#joining state shapefile and crime_state_agg dataframe

#######initial processing

agg_states <- crime_state_agg%>%
  group_by(STATE.UT)%>%
  summarise(Rape =sum(Rape),
            Kidnapping.and.Abduction = sum(Kidnapping.and.Abduction),
            Dowry.Deaths=sum(Dowry.Deaths),
            Assault.on.women.with.intent.to.outrage.her.modesty = sum(Assault.on.women.with.intent.to.outrage.her.modesty),
            Insult.to.modesty.of.Women=sum(Insult.to.modesty.of.Women),
            Cruelty.by.Husband.or.his.Relatives=sum(Cruelty.by.Husband.or.his.Relatives),
            Importation.of.Girls=sum(Importation.of.Girls),
            Total.Crime=sum(Total.Crime))

ordered_states <- match(tolower(shp_state@data$NAME_1),tolower(agg_states$STATE.UT))

shp_state@data$states <- agg_states[ordered_states,]

#Shiny code
#code for UI
ui <- dashboardPage(skin="green",
  dashboardHeader(titleWidth = 350,
    title ="Analysis - Crime Against Women",
    dropdownMenu(type="messages",
                 messageItem(from = "Roopam Dad",icon= icon("users"),message=""
                 ),
                 messageItem(from="xxx",icon=icon("university"),message=""),
                 messageItem(from = "Dated: 10/06/2019",icon=icon("calendar"),message="")
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      selectInput("crime_type","Crime",colnames(subset(crime_total[c(4:11)])))
      ,
      menuItem("Time Analysis over India",
      menuItem("Time Analysis over India", tabName = "india"),
      sliderInput("range","Time",min(crime_total$Year),max(crime_total$Year),
                  value = range(crime_total$Year),step = 1.0)
      ),
      menuItem("Study of Year 2015",
      menuItem("State Analysis", tabName = "state"),
      menuItem("Ranking on Incidents vs Crime Rate", tabName = "Ranking"),
               selectInput("selectState","STATE",crime_2015[,2],crime_2015[,2][30]) ),
      menuItem("Study of Year 2016 ",tabName = "judicial"),
      menuItem("Time Series Analysis of crimes",
      menuItem("Ananlysis from 2001-2011",tabName = "timeseries"),
      selectInput("selectLevel","State or Top Cities  ?",c("State","City"),"State"),
      selectInput("xaxis","X - Axis",colnames(crime_2001_2011[c(-40,-1,-2)]),colnames(crime_2001_2011[c(37)])),
      selectInput("yaxis","Y - Axis",colnames(crime_2001_2011[c(-40,-1,-2)]),colnames(crime_2001_2011[c(4)])),
      selectInput("bubble","Bubble Size",colnames(crime_2001_2011[c(-40,-1,-2)])),
      selectInput("selectColour","colour",colnames(crime_2001_2011[c(-40,-1)]),colnames(crime_2001_2011[c(2)]))
      
      )
      ),
    collapsed = TRUE
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
                              background-color: #3c8dbc;
                              }
                              .skin-blue .main-header .logo:hover {
                              background-color: #3c8dbc;
                              }
                              '))),
    fluidRow(
      tabItems(
        tabItem(box(status="success",title = "Crime Count Colour Map of India",width=12,collapsible = TRUE,tags$li("Below is the colour Map of India reperesenting
                    crime count over the years. Colour changes as per range selected and crime selected."),
                    tags$li("Rape Crimes have increased dramatically over the years"),
                    tags$li("Percentage change is the change in percentage of crimes from the last year."),
                    tags$li("Dowry Deaths are very hight in states Uttar Pradesh and Bihar.These are also the states with high kidnnaping rates")),
                tabName = "india",
                leafletOutput("map1", height = 650),
                absolutePanel(id="controls2",top=140,left="auto",right=0,draggable=FALSE,class ="panel panel-default",
                              width = 400,height="auto",
                              plotlyOutput("plot1", height = 378),
                              leafletOutput("map2",height = 378)
                              
                )
                ),
        tabItem(tabName = "state",
                fluidPage(
                  box(status = "success",title = "Incident and Rate Analysis",width=12,collapsible=TRUE,
                      tags$li("We can see that low count of incident doesnt mean that crime rate will also be low "),
                      tags$li("Take Example of A & N Islands. Total crimes commited against women in
                              A& N Islands in 2015 but it has 
                              high crime rate of 51.1. It is almost equal to overall crime rate of India."))
                  ),
                fluidPage(
                  plotlyOutput("incident_plot")
                )
                ),
        tabItem(tabName = "Ranking",
                fluidPage(box(status="success",width=12,title ="State Ranking on Incident Count vs Crime Rate",
                             collapsible = TRUE,
                             tags$li("Delhi is not only the capital but also the Rape capital of India. We can back this up
                                     with Data we have. We can see that in terms of Incident count for rape Delhi stands at
                                     9th place. These includes India level totals too. But when it comes to Rape rate, it is highest in Delhi.",
                             tags$li("Delhi is also gets 1st spot in Kidnapping and abduction of women. Interesting fact is that
                                     Kidnapping and abduction for Rape is also very very high in Delhi. It is 40.3%. This statement can be backeup
                                     from the data presented in State Analysis Tab.",
                              tags$li("Half of the total crime incidents against women reported in Assam were for Cruelty by Husband and that gives it top
                                      spot in crime rate for Cruelty by  husband and Relatives. It is 71.5"),
                              tags$li("From time Analysis graph in India tab, we can see that Assam has seen the rise of over 20% from 2014 to 2015
                                      for Cruelty by Husband or Relatives. For the same period India show the trend as low but Assam is going in different direction.
                                      It is also the only North-Eastern state with such a high Crime Rate and Incidents all other north eastern states are at bottom of the table."))))
                ),
                fluidPage(
                  column(width=6,
                  plotlyOutput("state_incident_plot", height = 800)
                  ),
                  column(width=6,
                  plotlyOutput("state_rate_plot",height = 800)
                  )
                  )
                ),
        tabItem(tabName = "judicial",
                fluidPage(
                  box(status="success",title="Efficiency of Judicial System",width=12,collapsible = TRUE,
                      tags$li("Conditon of Judicial System is in very bad condition, specially functioning of courts."),
                      tags$li(" Courts have pendency rate of More than 90% for majority of the crimes. This shows that it is
                               very difficult for women to find justice in India."),
                      tags$li("The same thing has also been said in many reports published by BBC and Washington post that
                                India is one of the most dangerousg country to live for women."),
                      tags$li("Article by BBC - BBC - https://www.bbc.com/news/world-asia-india-42436817"),
                      tags$li("Article by Washington Post - https://www.washingtonpost.com/news/worldviews/wp/2018/06/27/india-rank
                                ed-worlds-most-dangerous-place-for-women-reigniting-debate-about-womens-safety/?utm_term=.d81c9c4674dd"),
                      tags$li("We cannot say that Police is not functioning well. Chargsheeting Rate by
                               police is very high. That means police solves the case from their end 
                               and files the chargesheet in courts. The real problem is in the functioning of
                               courts where cases stuck."),
                      tags$li("A report from 'Times of India' states that vacancies in lower courts are all time high in India."),
                      tags$li("Articel by Times of India - https://timesofindia.indiatimes.com/india/vacanc
                                ies-in-lower-courts-at-all-time-high/articleshow/62320296.cms"))
                ),
                fluidPage(
                selectInput("selectCrime",label = "Select Crime :",choices = police_2016[,1])
                ),
                fluidPage(
                  column(width=6,
                plotlyOutput("judicial_plot",height=600)
                ),
                column(width=6,
                plotlyOutput("police_plot",height =600)
                )
                )
        ),
        tabItem(tabName = "timeseries",
                plotlyOutput("gap_plot",height=700)
        )
                
      )
  )
  
)
)

#code for server
server <- function(input, output, session){
  
  #Render Leaflet for India level
  output$map1 <- renderLeaflet({
     shp_state$varcolor <- as.numeric(unlist(shp_state@data$states[,"Rape"]))
     pal <- colorBin(sort(heat.colors(20),decreasing = TRUE),domain = shp_state$varcolor)
    leaflet(data = shp_state) %>%
      addProviderTiles(provider = "CartoDB.Positron")%>%
      addPolygons(
                  #fillColor = ~pal(varcolor),
                  color = "#444444",
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 1.0,
                  fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE))%>%
      addLegend(pal =pal, values = ~varcolor,opacity = 0.7, title = "Rape")
  })
  
  #reactive events for state
  crime_year <- reactive({
    crime_1 <- crime_state_agg[crime_state_agg$Year >= input$range[1] & crime_state_agg$Year <= input$range[2],]
    return(crime_1)
  })
  
  #reactive function for crime percentage change city
  crime_year_city <-reactive({
    crime_1 <- crime_city_agg[crime_city_agg$Year >= input$range[1] & crime_city_agg$Year <= input$range[2],]
    return(crime_1)
  })
  
  
  map_data <- reactive({
    x<- input$crime_type
    crime_year <- crime_year()
    map_data_1 <-crime_year%>%
      group_by(STATE.UT)%>%
      summarise(Rape =sum(Rape),
                Kidnapping.and.Abduction = sum(Kidnapping.and.Abduction),
                Dowry.Deaths=sum(Dowry.Deaths),
                Assault.on.women.with.intent.to.outrage.her.modesty = sum(Assault.on.women.with.intent.to.outrage.her.modesty),
                Insult.to.modesty.of.Women=sum(Insult.to.modesty.of.Women),
                Cruelty.by.Husband.or.his.Relatives=sum(Cruelty.by.Husband.or.his.Relatives),
                Importation.of.Girls=sum(Importation.of.Girls),
                Total.Crime=sum(Total.Crime))
    return(map_data_1)
  })
  
  #reactive events for cities
  crime_city <- reactive({
    crime_1 <- crime_total[crime_total$Year >= input$range[1] & crime_total$Year <= input$range[2] & crime_total$STATE.UT == input$map1_shape_click$id ,]
    return(crime_1)
  })
  
  map_data_city <- reactive({
    x<- input$crime_type
    crime_year <- crime_city()
    map_data_1 <-crime_year%>%
      group_by(DISTRICT)%>%
      summarise(Rape =sum(Rape),
                Kidnapping.and.Abduction = sum(Kidnapping.and.Abduction),
                Dowry.Deaths=sum(Dowry.Deaths),
                Assault.on.women.with.intent.to.outrage.her.modesty = sum(Assault.on.women.with.intent.to.outrage.her.modesty),
                Insult.to.modesty.of.Women=sum(Insult.to.modesty.of.Women),
                Cruelty.by.Husband.or.his.Relatives=sum(Cruelty.by.Husband.or.his.Relatives),
                Importation.of.Girls=sum(Importation.of.Girls),
                Total.Crime=sum(Total.Crime))
    return(map_data_1)
  })
  
  ##reactive event for percentage change for country
  crime_ind_change <- reactive({
    crime_total <- crime_year()
    crime_total_1 <- crime_total%>%
      group_by(Year)%>%
      summarise(Rape =sum(Rape),
                Kidnapping.and.Abduction = sum(Kidnapping.and.Abduction),
                Dowry.Deaths=sum(Dowry.Deaths),
                Assault.on.women.with.intent.to.outrage.her.modesty = sum(Assault.on.women.with.intent.to.outrage.her.modesty),
                Insult.to.modesty.of.Women=sum(Insult.to.modesty.of.Women),
                Cruelty.by.Husband.or.his.Relatives=sum(Cruelty.by.Husband.or.his.Relatives),
                Importation.of.Girls=sum(Importation.of.Girls),
                Total.Crime=sum(Total.Crime))
    return(crime_total_1)
      
  })
  
  #reactive event for percentage change for state
  crime_state <- reactive({
    state<-input$map1_shape_click
    state_name <- state$id
    crime_temp <- crime_year()
    crime_1 <- crime_temp[crime_temp[["STATE.UT"]]==state_name,]
    return(crime_1)
  })
  
  #reactive event for percentage change for city
  crime_perct_city <- reactive({
    city <- input$map2_shape_click
    city_name <- city$id
    crime_temp <- crime_year_city()
    crime_1 <- crime_temp[crime_temp[["DISTRICT"]]==city_name,]
    return(crime_1)
  })
  
  #observe event to change india map on change in crime type and range of time
  observe({
    
    map_data <- map_data()
    ordered_states <- match(tolower(shp_state1@data$NAME_1),tolower(map_data$STATE.UT))
    shp_state1@data$states <- map_data[ordered_states,]
    shp_state1$varcolor <- as.numeric(unlist(shp_state1@data$states[,input$crime_type]))
    
    pal <- colorBin(sort(heat.colors(20),decreasing = TRUE),domain = shp_state1$varcolor)
    
    label1 <- sprintf("<strong>State: %s<br>%s : %g<br>Year:%g - %g</strong>",shp_state1@data$states$STATE.UT,input$crime_type,shp_state1$varcolor,input$range[1],input$range[2])%>%
      lapply(htmltools::HTML)
    
    leafletProxy("map1",data = shp_state1) %>%
      clearControls()%>%
      addPolygons(label = ~label1,
                  fillColor = ~pal(varcolor),
                  layerId = ~states$STATE.UT,
                  color = "#444444",
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 1.0,
                  fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE))%>%
      addLegend(pal =pal, values = ~varcolor,opacity = 0.7, title = input$crime_type,"topleft",layerId = "layer1")
    
  })
  
  #observe event on map1 click to generate percentage change graph and state map on the  basis of map part clicked
  observeEvent(eventExpr =input$map1_shape_click,{
    p<- input$map1_shape_click
    state <- p$id
    shp_city1 <- shp_city[toupper(shp_city@data$NAME_1)==state,]
    map_data_city <- map_data_city()
    ordered_city <- match(tolower(shp_city1@data$NAME_2),tolower(map_data_city$DISTRICT))
    
    shp_city1@data$city <- map_data_city[ordered_city,]
    
    shp_city1$varcolor <- as.numeric(unlist(shp_city1@data$city[,input$crime_type]))
    
    pal <- colorBin(sort(heat.colors(20),decreasing = TRUE),domain = shp_city1$varcolor)
    
    label1 <- sprintf("<strong>State: %s<br>Shapefile: %s<br>%s : %g</strong>",shp_city1@data$city$DISTRICT,shp_city1@data$NAME_2,input$crime_type,shp_city1$varcolor)%>%
      lapply(htmltools::HTML)
    
    if(!is.null(state))
    {
    output$map2 <-renderLeaflet({
      leaflet(data = shp_city1) %>%
        addProviderTiles(provider = "CartoDB.Positron")%>%
        clearShapes()%>%
        addPolygons(label = ~label1,
                    fillColor = ~pal(varcolor),
                    layerId = ~city$DISTRICT,
                    color = "#444444",
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 1.0,
                    fillOpacity = 0.5,
                    highlightOptions = highlightOptions(color = "white",
                                                        weight = 2,
                                                        bringToFront = TRUE))

    })
    
    #plot for crime percentage change for state
    output$plot1 <- renderPlotly({
      xrange <- range(input$range)
      yrange <- range(-20,100)
      crime_total <- crime_ind_change()
      crime_state <-crime_state()
      crime_subset <- crime_total[c("Year",input$crime_type)]
      crime_subset$state <- crime_state[[input$crime_type]]
      perct_ind <- percentChange(ts(crime_total[input$crime_type]))
      perct_state <- percentChange(ts(crime_state[input$crime_type]))
      x<-c(0,perct_ind)
      y<-c(0,perct_state)
      crime_subset$perct_change_ind <- x
      crime_subset$perct_change_state <-y
      
      p<- plot_ly(x = crime_subset[["Year"]], y = crime_subset[["perct_change_ind"]], name = 'India', type = 'scatter', mode = 'lines+markers')%>%
        add_trace(y = crime_subset[["perct_change_state"]], name = state, mode = 'lines+markers') %>%
        layout(title = "Percentage Change in Crime ",
               xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE))
    
    })
    
    }
  })
  
  # observe event for click on map2 to generate percentage change for the district on which click event was performed
  observeEvent(
    input$map2_shape_click,{
      p<- input$map2_shape_click
      city <- p$id
      
      p1<- input$map1_shape_click
      state <- p1$id
      
      shp_city1 <- shp_city[toupper(shp_city@data$NAME_2)==city,]
      
      #plotting percentage change data for India and state and city on which click event performed.
      output$plot1 <- renderPlotly({
        xrange <- range(input$range)
        yrange <- range(-20,100)
        
        crime_total <- crime_ind_change()
        crime_state <-crime_state()
        crime_perct_city <- crime_perct_city()
        
        #subsetting dataframes on crime tupe
        crime_india <- crime_total[c("Year",input$crime_type)]
        crime_state1 <- crime_state[c("Year",input$crime_type)]
    
        crime_city <- crime_perct_city[c("Year",input$crime_type)]
        
        #function to get relative percentage change in crime to total crime count in previous year
        perct_ind <- percentChange(ts(crime_total[input$crime_type]))
        perct_state <- percentChange(ts(crime_state[input$crime_type]))
        perct_city <- percentChange(ts(crime_perct_city[input$crime_type]))
        
        #embeding 0 for the first year
        x<-c(0,perct_ind)
        y<-c(0,perct_state)
        z<-c(0,perct_city)
        
        crime_india$perct_change_ind <- x
        crime_state1$perct_change_state <-y
        crime_city$perct_chng_city <-z
        
        #generating plot
        
        p <- plot_ly(x = crime_india[["Year"]], y = crime_india[["perct_change_ind"]], name = 'India', type = 'scatter', mode = 'lines+markers') %>%
          add_trace(x=crime_state1[["Year"]],y = crime_state1[["perct_change_state"]], name = state, mode = 'lines+markers') %>%
          add_trace(x=crime_city[["Year"]],y = crime_city[["perct_chng_city"]], name = city, mode = 'lines+markers')%>%
          layout(title = "Percentage Change in Crime ",
                 xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE))
        
        # plot(xrange,yrange,main="Percentage Change",xlab="Year",ylab="Percentage")
        # 
        # lines(crime_india[["Year"]],crime_india[["perct_change_ind"]],lwd=3,col="red")
        # lines(crime_state1[["Year"]],crime_state1[["perct_change_state"]],lwd=3,col="green")
        # lines(crime_city[["Year"]],crime_city[["perct_chng_city"]],lwd=3,col="blue")
        # 
        # legend(crime_india[["Year"]][2],80,legend = c("INDIA",state,city),pch=15,ncol=1,bty ="n",cex=1.1,col = c("red","green","blue"))
        # 
      })
      
  })
  
  ######Detailed study of 2015
  # subsetting dataframe crime_incident on the crime type selected from the input. A crime can be sub divided into further
  #crimes and hence i subsetted dataframe like this
  
   crime_incident_2015 <- reactive({
     #subset for Rape
     if (input$crime_type == colnames(crime_total[c(4)]))
     {
       crime_temp <- crime_incidents[,1:9]
     }
     #subset for kidnapping
     else if (input$crime_type == colnames(crime_total[c(5)]))
     {
       crime_temp <- crime_incidents[,c(1,10:15)]
     }
     else if (input$crime_type == colnames(crime_total[c(6)]))
     {
       crime_temp <- crime_incidents[,c(1,16)]
     }
     else if (input$crime_type == colnames(crime_total[c(7)]))
     {
       crime_temp <- crime_incidents[,c(1,17:22)]
     }
     else if (input$crime_type == colnames(crime_total[c(8)]))
     {
       crime_temp <- crime_incidents[,c(1,23:27)]
     }
     else if (input$crime_type == colnames(crime_total[c(9)]))
     {
       crime_temp <- crime_incidents[,c(1,28)]
     }
     else if(input$crime_type == colnames(crime_total[c(10)]))
     {
       crime_temp <- crime_incidents[,c(1,29)]
     }
     #subset for total crime
     else if(input$crime_type == colnames(crime_total[c(11)]))
     {
       crime_temp <- crime_incidents[,c(1,41)]
     }
     
     return(crime_temp)
   })
   
   #subsetiing the dataframe which stores crime rate of year 2015 for all states and India total
   crime_rate_2015 <- reactive({
     
     if (input$crime_type == colnames(crime_total[c(4)]))
     {
       crime_temp <- crime_rate[,1:9]
     }
     else if (input$crime_type == colnames(crime_total[c(5)]))
     {
       crime_temp <- crime_rate[,c(1,10:15)]
     }
     else if (input$crime_type == colnames(crime_total[c(6)]))
     {
       crime_temp <- crime_rate[,c(1,16)]
     }
     else if (input$crime_type == colnames(crime_total[c(7)]))
     {
       crime_temp <- crime_rate[,c(1,17:22)]
     }
     else if (input$crime_type == colnames(crime_total[c(8)]))
     {
       crime_temp <- crime_rate[,c(1,23:27)]
     }
     else if (input$crime_type == colnames(crime_total[c(9)]))
     {
       crime_temp <- crime_rate[,c(1,28)]
     }
     else if(input$crime_type == colnames(crime_total[c(10)]))
     {
       crime_temp <- crime_rate[,c(1,29)]
     }
     else if(input$crime_type == colnames(crime_total[c(11)]))
     {
       crime_temp <- crime_rate[,c(1,41)]
     }
     
     #returning the result
     return(crime_temp)
   })
   
   
   #plotting the crime Incidents and rate of year 2015
   output$incident_plot <-renderPlotly({
     crime_incident_2015<-crime_incident_2015()
     crime_incident_values <- melt(crime_incident_2015[][crime_incident_2015$State.UT==input$selectState,])
     
     crime_rate_2015 <- crime_rate_2015()
     crime_rate_values <- melt(crime_rate_2015[][crime_rate_2015$State.UT==input$selectState,])
     
     crime_incident_values$Rate <- crime_rate_values$value
     crime_incident_values$crime_type <- crime_rate_values$variable
     
    
     #plot for incidents   
     p1 <-plot_ly(x=crime_incident_values$value,y=reorder(
       crime_incident_values$variable,crime_incident_values$value), name = "Crime Incidents",
                 type ="bar", orientation = "h",
                 marker = list(color = 'rgba(50, 171, 96, 0.6)',
                               line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>%
       layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
              xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) %>%
       add_annotations(xref = 'x1', yref = 'y',
                       x = crime_incident_values$value * 2.1 + 3,  y = crime_incident_values$variable,
                       text = paste(round(crime_incident_values$value, 2)),
                       font = list(family = 'Arial', size = 12, color = 'rgb(50, 171, 96)'),
                       showarrow = FALSE)
     
     #plot for Crime Rate
     p2 <- plot_ly(x=crime_incident_values$Rate,y=reorder(crime_incident_values$variable,crime_incident_values$value),
                   name = "Crime Rates", type = "bar", orientation="h") %>%
       layout(yaxis = list(showgrid = FALSE, showline = TRUE, showticklabels = FALSE,
                           linecolor = 'rgba(102, 102, 102, 0.8)', linewidth = 2,
                           domain = c(0, 0.85)),
              xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE,
                           side = 'top', dtick = 3)) %>%
       add_annotations(xref = 'x2', yref = 'y',
                       x = crime_incident_values$Rate, y = crime_incident_values$variable,
                       text = paste(crime_incident_values$Rate),
                       font = list(family = 'Arial', size = 12, color = 'rgb(128, 0, 128)'),
                       showarrow = FALSE)
     
     #Joining both the plots 
     p <- subplot(p1, p2) %>%
       layout(title = 'Crime Incidents and Crime Rate',
              legend = list(x = 0.029, y = 1.038,
                            font = list(size = 10)),
              margin = list(l = 100, r = 20, t = 70, b = 70),
              paper_bgcolor = 'rgb(248, 248, 255)',
              plot_bgcolor = 'rgb(248, 248, 255)') %>%
       add_annotations(xref = 'paper', yref = 'paper',
                       x = -0.14, y = -0.15,
                       text = paste(""),
                       font = list(family = 'Arial', size = 10, color = 'rgb(150,150,150)'),
                       showarrow = FALSE)
       
   })
   output$state_incident_plot <- renderPlotly({
     crime_incident_2015<-crime_incident_2015()
     crime_incident_2015 <- crime_incident_2015[c(1,2)]
     
     #plot for incidents   
     p1 <-plot_ly(x=crime_incident_2015[[2]],y=reorder(
       crime_incident_2015$State.UT,crime_incident_2015[[2]]), name = "Crime Incidents Ranking",
       type ="bar", orientation = "h",
       marker = list(color = 'rgba(50, 171, 96, 0.6)',
                     line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>%
       add_annotations(xref = 'x1', yref = 'y',
                       x = crime_incident_2015[[2]]+ 3,  y = crime_incident_2015$State.UT,
                       text = paste(round(crime_incident_2015[[2]], 2)),
                       font = list(family = 'Arial', size = 12, color = 'rgb(50, 171, 96)'),
                       showarrow = FALSE)%>%
       layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE),
              xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE),
              title="Ranking of States as per Incident Count in 2015"
       )
       
   })
   
   #Ranking of states as per Crime Rate and Incidents in 2015 to compare the difference in Ranking
   output$state_rate_plot <-renderPlotly ({
     
     crime_rate_2015 <- crime_rate_2015()
     crime_rate_2015 <- crime_rate_2015[c(1,2)]
     
     p2 <-plot_ly(x=crime_rate_2015[[2]],y=reorder(
       crime_rate_2015$State.UT,crime_rate_2015[[2]]), name = "Crime Rate Ranking",
       type ="bar", orientation = "h",
       marker = list(color = 'rgba(128, 0, 128)',
                     line = list(color = 'rgba(102, 102, 102, 0.8)', width = 1))) %>%
       add_annotations(xref = 'x1', yref = 'y',
                       x = crime_rate_2015[[2]] + 3,  y = crime_rate_2015$State.UT,
                       text = paste(round(crime_rate_2015[[2]], 2)),
                       font = list(family = 'Arial', size = 12, color = 'rgb(128, 0, 128)'),
                       showarrow = FALSE)%>%
       layout(title = "Ranking of States as per Crime Rate in 2015 ",
              xaxis = list(title = ""),
              yaxis = list(title = ""))
     
   })
   
   #reactive function for court data
   court_data <-reactive({
     temp_data <- court_2016[c(13,14)][court_2016[[1]]==input$selectCrime,]
     return(melt(temp_data))
   })
   
   #Reactive function for Police Data
   
   police_data <-reactive({
     temp_data <- police_2016[c(16,17)][court_2016[[1]]==input$selectCrime,]
     return(melt(temp_data))
   })
   
   #Plotting Judicial Data for 2016
   output$judicial_plot <-renderPlotly({
     court_data<-court_data()
     p <- plot_ly(court_data, x = ~variable, y = ~value, type = 'bar',
                  marker = list(color = 'rgba(222,45,38,0.8)',
                                line = list(color = 'black',
                                            width = 1.5))) %>%
       layout(title = "How Efficient is Judical System of India ?",
              xaxis = list(title = ""),
              yaxis = list(title = ""))
   })
  
   #Plotting Police Data for 2016
   output$police_plot <-renderPlotly({
     police_data<-police_data()
     p <- plot_ly(police_data, x = ~variable, y = ~value, type = 'bar',
                  marker = list(color = 'rgb(158,202,225)',
                                line = list(color = 'black',
                                            width = 1.5))) %>%
       layout(title = "How Efficient is Police in India?",
              xaxis = list(title = ""),
              yaxis = list(title = ""))
   })
   
   #reactive function to do analysis over state or cities
   crime_subset_2001_2011 <- reactive({
     if (input$selectLevel=="State")
     {
       temp_crime <- crime_2001_2011[][crime_2001_2011$Category!="City",]
     }
     else
     {
       temp_crime <- crime_2001_2011[][crime_2001_2011$Category=="City",]
     }
     return(temp_crime)
   })
   
   #generating a animated plot for crime from 2001_2011
   output$gap_plot <- renderPlotly({
     
     crime_subset_2001_2011 <- crime_subset_2001_2011()
     p <-plot_ly(
         x = crime_subset_2001_2011[[input$xaxis]], 
         y = crime_subset_2001_2011[[input$yaxis]],
         size = crime_subset_2001_2011[[input$bubble]]*2,
         color = crime_subset_2001_2011[[input$selectColour]],
         frame = crime_subset_2001_2011$Year,
         text = paste("State:",crime_subset_2001_2011$STATE.UT,input$bubble,crime_subset_2001_2011[[input$bubble]]),
         hoverinfo = "text",
         type = 'scatter',
         mode = 'markers',
         marker = list(opacity=0.5,sizemode="diameter")
       ) %>%
       layout(
         xaxis = list(title = input$xaxis),
         yaxis = list(title = input$yaxis),
         title = "Crime Analysis from 2001-2011",
         showlegend =FALSE
       )
   })
}

shinyApp(ui, server)