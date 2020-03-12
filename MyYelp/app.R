library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)
library(DT)
library(plotly)
library(shinythemes)
library(viridis)

ui <- fluidPage(
  theme = shinytheme("united"),

  titlePanel(h2("Aggie YelperHelper!", align = "center")),
  navbarPage(
    "Menus",
    tabPanel("Stats", fluidPage(
      sidebarPanel(img(src = "https://s3-media3.fl.yelpcdn.com/assets/srv0/developer_pages/b2ca299e2633/assets/img/318x90_yelp_fusion.png", height = 75, width = 200),
        img(src = "https://rlv.zcache.com/uc_davis_aggies_poster-r572efe85d8b34d16969c4d2549da6c22_wvk_8byvr_540.jpg", height = 75, width = 75, align = "right"),
        h3("Instructions", align = "center"),
        strong("For City Stats:"),
        p("Just pick a local city! The 'City' tab wil give you some quick stats."),
        br(),
        strong("For a specific restaurant:"),
        p("Select a local city and category. Then head over to the restaurant tab."),
        br(),
        br(),
        selectInput("cities",
          label = "Choose a city to look in.",
          choices = list(
            "Davis",
            "Dixon",
            "Sacramento",
            "Woodland"
          ),
          selected = "Davis"
        ),

        selectInput("category",
          label = "What kind of food did you want?",
          choices = list("American", "Breakfast", "Chinese"),
          selected = "None",
        ),
        strong("Not sure where to go? Let us Pick!"),
        br(),
        actionButton("lucky", "Click Me!"),
        br(),
        br(),
        textOutput("rand"),
        width = 3
      ),


      mainPanel(
        tabsetPanel(
          tabPanel(
            "City Stats",

            column(
              4,
              h4("Rating Overview", align = "left"),
              plotlyOutput("hist"),
              textOutput("avgR"),
              tableOutput("avgrate")
            ),
            column(
              4,
              h4("Quick Stats", align = "left"),
              textOutput("Categories"),
              dataTableOutput("cat.sum"),
              textOutput("pcnt"),
              tableOutput("price")
            ),
            column(
              3,
              h4("maps", align = "left"),
              leafletOutput("cityMap", height = 450, width = 350)
            )
          ),



          tabPanel(
            "Restaurants",
            column(
              4,
              h4("Restaurant Info", align = "Left"),
              dataTableOutput("info")
            ),
            column(4,
              offset = 4,
              h4("maps", align = "left"),
              leafletOutput("restMap", height = 350, width = 350)
            )
          )
        ),
        width = 9
      )
    )),
    tabPanel("About",
             h3(strong("Welcome to the Aggie YelperHelp")),
             p(strong("This section includes information about the app you are using. Please enjoy!")),
             br(),
             p("")
             ,width=8),
    tabPanel("References"
             )
  ),
)




# Define server logic ---------------------------------------------------------------------------
server <- function(input, output, session) {
  rest <- reactive({
    yp <- GET(
      "https://api.yelp.com/v3/businesses/search",
      add_headers(Authorization = paste("Bearer", Sys.getenv("MY_YELP"))),
      query = list(
        term = "Restaurant",
        location = paste(input$cities, ", CA"),
        limit = 50
      )
    )
    stop_for_status(yp)
    json <- content(yp, as = "text", encoding = "UTF-8")

    # change to yp for all 4 cities and then filter based on user input//will be reactive I think
    fromJSON(json)$business %>%
      select(id, name, image_url, url, review_count, categories, rating, coordinates, price, location, display_phone, transactions) %>%
      unnest(categories) %>%
      mutate(lat = coordinates$lat, lon = coordinates$lon) %>%
      mutate(display_address = location$display_address %>% map_chr(str_c, collapse = "\n"), city = as.character(location$city)) %>%
      mutate(link = paste0("<a href='", url, "'>", name, "</a>")) %>%
      mutate(pic = paste("<img src=", image_url, "height='125'></img>")) %>%
      filter(city == input$cities) %>%
      distinct(name, .keep_all = TRUE)
  })




  ### want to change to ggplot
  output$hist <- renderPlotly({
    x <- input$cities

    observeEvent(input$cities, {
      updateSelectInput(session, "category",
        label = paste("What kind of food did you want?"),
        choices = unique(rest()$title),
        selected = head(unique(rest()$title), 1)
      )
    })



    plot_ly(rest(),
      x = rest()$rating,
      marker = list(color = viridis_pal(option = "C", direction = -1)(4)),
      type = "histogram", name = "Histogram"
    )
  })
  output$price <- renderTable({
    rest() %>%
      group_by(price) %>%
      summarise(count = n()) %>%
      na.omit()
  })

  output$avgR <- renderText({
    paste("Avg number of stars for ", input$cities)
  })

  output$avgrate <- renderTable({
    rest() %>% summarise(Avg = mean(rating))
  })
  output$pcnt <- renderText({
    paste("Number of restaurants for ", input$cities, "by price")
  })

  output$cat.sum <- renderDataTable(
    {
      rest() %>%
        group_by(title) %>%
        summarise(Count = n()) %>%
        na.omit()
    },
    options = list(searching = FALSE, pageLength = 5)
  )

  output$info <- renderDataTable(
    {
      return(rest() %>% select(name:pic) %>% filter(title == input$category, city == input$cities)
        %>% select(name, rating, price, display_address, link, pic))
    },
    escape = FALSE,
    options = list(searching = FALSE, pageLength = 4)
  )



  output$cityMap <- renderLeaflet({
    df.map <- rest() %>% select(name, lat, lon)

    leaflet(df.map) %>%
      addTiles() %>%
      addMarkers(
        popup = paste(
          rest()$name, "<br>",
          rest()$display_address
        )
      ) %>%
      addTiles()
  })



  output$restMap <- renderLeaflet({
    df.rest <- rest() %>%
      filter(title == input$category, city == input$cities) %>%
      select(name, display_address, lat, lon)

    leaflet(df.rest) %>%
      addTiles() %>%
      addMarkers(
        popup = paste(
          df.rest$name, "<br>",
          df.rest$display_address
        )
      ) %>%
      addTiles()
  })

  observeEvent(input$lucky, {
    output$rand <- renderText({
      paste(
        "You got ",
        rest() %>% select(name) %>%
          slice(round(runif(1, 1, nrow(rest())))),
        "as your restaurant!"
      )
    })
  })

  # output$rand <- renderText({
  #   input$lucky
  #   isolate({paste("You got ",
  #                  rest() %>% select(name) %>%
  #                  slice(round(runif(1,1,50))),
  #                  "as your restaurant!")
  #  })

  # })
}

# Run the app -----------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
