library(shiny)
library(shinydashboard)
library(leaflet)

ui <- fluidPage(
  titlePanel(h2("YelperHelper!",align="center")),
  
  sidebarPanel( h4("Instructions",align="center"),
                p("Select a local city, food category, and price below. Then choose the restaurant you want to look at."),
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
                                   choices = list("$"=1,"$$"=2,"$$$"=3,"$$$$"=4),selected=2),width = 3),
  
  mainPanel(
    tabsetPanel(
      tabPanel("City Stats",
               
               
               
       column(5, 
              h4("Rating Overview",align="left"),
              plotOutput("hist"),
              tableOutput("avgrate")),
       column(3,
              h4("Average Price",align="left"),
              tableOutput("price")),
       column(4,
              h4("maps",align="center"),
              leafletOutput("cityMap",height=500,width=400))
        
              
             ),
               
                 
               
               
               
               
               
               
      
      
      tabPanel("Restaurant Stats",
               sidebarPanel(selectInput("rest",
                                        label = "Make your choice",
                                        choices=list(
                                          "Mikuni's", "Ali Baba","3rd & U Cafe"),
                                        selected="Ali Baba"))
      )
    ),
    
    
    
    # h1("Introducing Shiny"),
    
    width=8)
)




# Define server logic ----
server <- function(input, output) {
 
  yp <- GET(
    "https://api.yelp.com/v3/businesses/search",
    add_headers(Authorization = paste("Bearer", Sys.getenv("MY_YELP"))),
    query = list(
      term="Restaurant",
      location = "Davis, CA 95618", 
      limit=50
      
      
    )
  )
  stop_for_status(yp)
  json <- content(yp, as = "text",encoding="UTF-8")
  
  #change to yp for all 4 cities and then filter based on user input//will be reactive I think
  yp_davis<-fromJSON(json)$business %>% 
    select(id, name, image_url,review_count,categories,rating,coordinates,price,location,display_phone, transactions) %>% 
    unnest(categories) %>% 
    mutate(lat = coordinates$lat,lon = coordinates$lon) %>% 
    mutate( display_address = as.character(location$display_address),city=as.character(location$city)) %>% 
    select(name,title,rating,price,display_phone,lat,lon,display_address,city) %>%
    distinct(name,.keep_all = TRUE) 
  
               

  
   output$hist <- renderPlot({
    hist(yp_davis$rating,main="Ratings") #not the same data as below//add second arg main=isolate(input$title) makes the title variable non-reactive
    
  })
  output$price <- renderTable({
    yp_davis %>% group_by(price) %>% 
      summarise(n()) %>%  slice(-4) 
      #not the same data as above
    
  })
  output$avgrate <-renderTable({
    yp_davis %>% summarise(Avg=mean(rating))
  })
  
  #output$cities <-
  
  output$cityMap <- renderLeaflet({
    
    df<-yp_davis %>% select(name,lat,lon)
    
    leaflet(df) %>% 
      addTiles() %>% 
      addMarkers() %>% 
      addTiles()
      #setView(lng=-121.740517,lat=38.544907,zoom =09 )
      
  })
  
}



# Run the app ----
shinyApp(ui = ui, server = server)