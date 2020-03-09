library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)


ui <- fluidPage(
  titlePanel(h2("Aggie YelperHelper!",align="center")
             ),
  
  sidebarPanel(img(src = "yelp_fusion.png", height = 75, width =200),
               img(src= "aggie_logo.jpg",height=75,width=75,align="right"),
                h3("Instructions",align="center"),
                strong("For City Stats:"),
                p("Just pick a local city! The 'City' tab wil give you some quick stats."),
                br(),
                strong("For a specific restaurant:"),
                p("Select a local city, food category, and price below. Then choose the restaurant 
                  you want to look at in the restaurant tab."),
                br(),
                br(),
                selectInput("cities",
                            label="Choose a city to look in.",
                            choices = list("Davis",
                                           "Dixon",
                                           "Sacramento",
                                           "Woodland"),
                            selected = "Davis"),
                #this section will be pulled from cats in API
                selectInput("category",
                            label="What kind of food did you want?",
                            choices = list("American","Breakfast","Chinese","Mexican"),
                            selected="Breakfast"),
                checkboxGroupInput("price",label="What's your budget",
                                   choices = list("$"=1,"$$"=2,"$$$"=3,"$$$$"=4),selected=1),width = 3),
                
  
  mainPanel(
    tabsetPanel(
      tabPanel("City Stats",
               
       column(4, 
              h4("Rating Overview",align="left"),
              plotOutput("hist")
              ),
       column(4,
              h4("Quick Stats",align="left"),
              textOutput("avgR"),
              tableOutput("avgrate"),
              textOutput("pcnt"),
              tableOutput("price")),
       column(4,
              h4("maps",align="left"),
              leafletOutput("cityMap",height=500,width=400))
        
              
             ),
               
                 
               
      tabPanel("Restaurant Stats",
               column(4,
                      h4("restaurant",align="left"),
                      selectInput("rest",
                                  label = "Make your choice",
                                  choices=list("Mikuni's", 
                                               "Ali Baba",
                                               "3rd & U Cafe"),
                                  selected="Ali Baba")
                      ),
               column(4,
                 h4("maps",align="right"),
                 leafletOutput("restMap",height=400,width=350))
      )
    ),
    
    
    
    # h1("Introducing Shiny"),
    
    width=8)
)




# Define server logic ---------------------------------------------------------------------------
server <- function(input, output,session) {
 

  rest = reactive({yp <- GET(
    "https://api.yelp.com/v3/businesses/search",
    add_headers(Authorization = paste("Bearer", Sys.getenv("MY_YELp"))),
    query = list(
      term="Restaurant",
      location = paste(input$cities, ", CA"), 
      limit=50
      
      
    )
  )
  stop_for_status(yp)
  json <- content(yp, as = "text",encoding="UTF-8")
  
  #change to yp for all 4 cities and then filter based on user input//will be reactive I think
  fromJSON(json)$business %>% 
    select(id, name, image_url,review_count,categories,rating,coordinates,price,location,display_phone, transactions) %>% 
    unnest(categories) %>% 
    mutate(lat = coordinates$lat,lon = coordinates$lon) %>% 
    mutate(display_address = location$display_address %>% map_chr(str_c, collapse = "\n"),city=as.character(location$city)) %>% 
    select(name,title,rating,price,display_phone,lat,lon,display_address,city) %>%
    distinct(name,.keep_all = TRUE) })  

  
  output$selected_var <- renderText({ 
    paste("You have selected", input$cities)
  })           

  ###want to change to ggplot 
   output$hist <- renderPlot({
     
     x = input$cities
     
     updateSelectInput(session,"category",
                       label = paste("What kind of food did you want?"),
                       choices = unique(rest()$title),
                       selected = head(unique(rest()$title),1))
     
     
     hist(rest()$rating,main="Ratings") 
     
     #not the same data as below//add second arg main=isolate(input$title) makes the title variable non-reactive
    
  })
  output$price <- renderTable({
    rest() %>%  group_by(price) %>% 
      summarise(n()) %>%  slice(-4) 
      #not the same data as above
    
  })
  
  output$avgR<-renderText({
    paste("Avg number of stars for ",input$cities)
  })
  
  output$avgrate <-renderTable({
    rest() %>% summarise(Avg=mean(rating))
  })
  output$pcnt<-renderText({
    paste("Number of restaurants for ",input$cities,  "\nby price")
  })
  
  
  output$cityMap <- renderLeaflet({
    
    df.map<-rest() %>% select(name,lat,lon)
    
    leaflet(df.map) %>% 
      addTiles() %>% 
      addMarkers(
        popup = paste(rest()$name,"<br>"
                      ,rest()$display_address)
      ) %>% 
      addTiles()
      
      
  })
  


output$restMap <- renderLeaflet({
  
  x <- input$cities
  
  observeEvent(updateSelectInput(session,"restaurant",
                                 label = paste("Make your choice"),
                                 choices = unique(rest()$name),
                                 selected = head(unique(rest()$name),1)))
  
  df<-rest() %>% filter(name == input$rest) %>% select(name,lat,lon)
  
  leaflet() %>% 
    addTiles() %>% 
    addMarkers() %>% 
    addTiles()
  #setView(lng=-121.740517,lat=38.544907,zoom =09 )
  
})

}

# Run the app -----------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)