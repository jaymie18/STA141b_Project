ui <- fluidPage(
  titlePanel("YelperHelper!"),
  
mainPanel(
     
        tabsetPanel(
        tabPanel("City Stats",
                 
                sidebarPanel( h4("Instructions"),
                              p("Select a local city, food category, and price below. Then choose the restaurant you want to look at."),
                              br(),
                              br(),
                              br(),
                              h4("City"),
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
                                                 choices = list("$"=1,"$$"=2,"$$$"=3,"$$$$"=4),selected=2))),
        
        
        
        
        
        tabPanel("Restaurant Stats",
                 sidebarPanel(selectInput("rest",
                                          label = "Make your choice",
                                          choices=list(
                                            "Mikuni's", "Ali Baba","3rd & U Cafe"),
                                          selected="Ali Baba"))
                 )
      ),
      h1("Introducing Shiny"),
      p("Shiny is a new package from RStudio that makes it ", 
        em("incredibly easy "), 
        "to build interactive web applications with R."),
      br(),
      p("For an introduction and live examples, visit the ",
        a("Shiny homepage.", 
          href = "http://shiny.rstudio.com")),
 
    )
  )




# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)