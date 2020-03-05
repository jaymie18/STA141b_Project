
ui <- fluidPage(
  titlePanel("YelperHelper!"),
  
  mainPanel(
    
    tabsetPanel(
      tabPanel("City Stats",
               
               sidebarPanel( h4("Instructions"),
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
                 h4("Look at me"),
                 plotOutput("hist"),
                 verbatimTextOutput("stats"),
                 verbatimTextOutput("stats2")
                 
               )
               
               
               
               
               
      ),
      
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
    
    width=10)
)




# Define server logic ----
server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(5)) #not the same data as below//add second arg main=isolate(input$title) makes the title variable non-reactive
    
  })
  output$stats <- renderPrint({
    summary(rnorm(5)) #not the same data as above
    
  })
  output$stats2 <-renderPrint({
    summary(rnorm(2))
  })
  
}



# Run the app ----
shinyApp(ui = ui, server = server)