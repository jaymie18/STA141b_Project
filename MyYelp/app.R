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

# Create User Interface----------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("united"),

  titlePanel(h2("Aggie YelperHelper!", align = "center")),
  navbarPage(
    "Menus",
    # Panel for instructions and user selctions
    tabPanel("Stats", fluidPage(
      sidebarPanel(img(src = "https://s3-media3.fl.yelpcdn.com/assets/srv0/developer_pages/b2ca299e2633/assets/img/318x90_yelp_fusion.png", height = 75, width = 200),
        img(src = "https://rlv.zcache.com/uc_davis_aggies_poster-r572efe85d8b34d16969c4d2549da6c22_wvk_8byvr_540.jpg", height = 75, width = 75, align = "right"),
        h3("Instructions", align = "center"),
        strong("For City Stats:"),
        p("Just pick a local city! The \"City Stats\" tab wil give you some quick stats."),
        br(),
        strong("For a closer look at specific restaurants:"),
        p("Select a local city and category. Then head over to the \"Restaurants\" tab."),
        br(),
        br(),
        selectInput("cities",
          label = "Choose a local city.",
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

      # Main Information Panel----------------------------
      mainPanel(
        tabsetPanel(
          tabPanel(
            "City Stats", # start city tab results

            column(
              4,
              h4("Rating Overview", align = "left"),
              plotlyOutput("hist"),
              textOutput("avgR"),
              tableOutput("avgrate")
            ),
            column(
              4,
              h4("Category Stats", align = "left"),
              textOutput("Categories"),
              wellPanel(dataTableOutput("cat.sum")),
              textOutput("pcnt"),
              tableOutput("price")
            ),
            column(
              3,
              h4("Maps", align = "left"),
              leafletOutput("cityMap", height = 450, width = 350)
            )
          ),



          tabPanel(
            "Restaurants", # start restaurant tab results
            column(
              4,
              h4("Restaurant Info", align = "Left"),
              dataTableOutput("info")
            ),
            column(4,
              offset = 4,
              h4("Maps", align = "left"),
              leafletOutput("restMap", height = 350, width = 350)
            )
          )
        ),
        width = 9
      )
    )),


    # Documentation for the About page-------------------------------------------------
    tabPanel(
      "About",
      h3(strong("Welcome to the Aggie YelperHelp")),
      p(strong("This section includes information about the app you are using. Please enjoy!")),
      p("The Aggie YelperHelper is meant to be used mainly by students at UC Davis. 
             Many students are unfamiliar with the local area when they first come to Davis.
             The YelperHelper provides students with a condensed look at local cities so they
             can gain a better understadning of what restaurant options Davis and nearby cities offer.
             Please note, the category selection feature is used on in the \"Restaurants\" tab. Choices made here will not
             reflect the output in the \"City\" tab."),
      p("The maps feature of this app provides the user with an overview of the entire city when using the \"City\" tab.
               When viewing the \"Restaurants\" tab, the map shows only the restaurants in the corresponding category."),
      p(
        "The app also provides users the chance to leave their restaunt selection to chance. The",
        em("Let Us Pick"), "button will randomly select a local restaurant for the user who has no idea where to eat."
      ),
      hr(),
      h4(strong("Description of Source Data")),
      p("The data used in the Aggie YelperHelper is all collected directly from the Yelp Fusion API. 
        The API allows users to collect the same data one would see on the Yelp website or through the Yelp mobile app. 
        For the purposes of this application we used the “business search” endpoint."),
      hr(),
      h4(strong("Developers")),
      tags$ul(
        tags$li("Grant Smith: programming, app design, app development, de-bugging"),
        tags$li("Jaymie Tam: programming, app development, de-bugging, EDA "),
        tags$li("Zejia Cai: User manual, Technical writing"),
        tags$li("Ke Huang: User Manual, Technical writing")
      ),
      hr(),
      h4(strong("References")),
      p("All data used in this app was collected via the Yelp Fusion API. See", tags$a(href = "https://www.yelp.com/fusion", "yelp.com/fusion"), "for more info"),
      p("Yelp Fusion logo.", em("“Discover the Power of Yelp Fusion.” Yelp Fusion, “www.yelp.com/fusion.”")),
      p("UCD Aggie logo", em("“UC Davis Aggies Poster.” Zazzle, www.zazzle.com/uc_davis_aggies_poster-228442860864946488.")),
      br(),
      hr()
    )
  ),
)




# Define server logic ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # reactive element to communicate user input with the GET parameters for Yelp API
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

  # Define output for histgram and datatables-----------------------------------------

  output$hist <- renderPlotly({ # creates histogram for ratings
    x <- input$cities

    observeEvent(input$cities, {
      updateSelectInput(session, "category",
        label = paste("What kind of food did you want?"),
        choices = unique(rest()$title),
        selected = head(unique(rest()$title), 1)
      )
    })

    plot_ly(rest(), # interactive plot_ly histogram
      x = rest()$rating,
      marker = list(color = viridis_pal(option = "C", direction = -1)(4)),
      type = "histogram", name = "Histogram"
    )
  })


  output$price <- renderTable( # price table
    {
      rest() %>%
        group_by(price) %>%
        summarise(count = n()) %>%
        na.omit()
    },
    bordered = TRUE
  )

  output$avgR <- renderText({ # average rating text (by city)
    paste("Average user rating for ", input$cities)
  })

  output$avgrate <- renderTable( # average rating table
    {
      rest() %>% summarise(Avg = mean(rating))
    },
    bordered = TRUE
  )
  output$pcnt <- renderText({ # price category text
    paste("Number of restaurants for ", input$cities, "by price")
  })

  output$cat.sum <- renderDataTable( # datatable for categories
    {
      rest() %>%
        group_by(title) %>%
        summarise(Count = n()) %>%
        na.omit()
    },
    options = list(searching = FALSE, pageLength = 5)
  )

  output$info <- renderDataTable( # datatable used to display restaurant information
    {
      return(rest() %>% select(name:pic) %>% filter(title == input$category, city == input$cities)
        %>% select(Name = name, Rating = rating, Price = price, Address = display_address, Link = link, Picture = pic))
    },
    escape = FALSE,
    options = list(searching = FALSE, pageLength = 4)
  )



  output$cityMap <- renderLeaflet({ # map for the city overview
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



  output$restMap <- renderLeaflet({ # map for the category sections
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

  observeEvent(input$lucky, { # creates random selection only after the user's initial interaction
    output$rand <- renderText({
      paste(
        "You got ",
        rest() %>% select(name) %>%
          slice(round(runif(1, 1, nrow(rest())))), # selects a random restaurant
        "as your restaurant!"
      )
    })
  })
}

# Run the app -----------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
