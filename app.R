library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)
bcl <-read.csv("https://raw.githubusercontent.com/daattali/shiny-server/master/bcl/www/bcl-data.csv")


ui <- fluidPage(
##Assignment 4 - Feature 1: I added a CSS file under www and use the function includeCSS() to change the font. I also added a theme called united by using shinnytheme package.

  includeCSS("www/mystyle.css"),theme = shinytheme("united"),
  titlePanel("BC liquor store App - assignment B-4"),
  h4("This app helps you to explore alcohol content"),



##Assignment 3 - Feature 1: I added an image of the BC Liquor Store.

  img(src = "logo.jpg"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("id_slider","Please choose your desired price range:", min=0, max=100, value=c(25,40),pre="$"),


##Assignment 4 - Feature 2: I allow the user to choose the desired sweetness levels

      sliderInput("id_sweet","Please choose your desired sweetness range:", min=0, max=10, value=c(0,2)),


      checkboxInput("showtype", "Show result by type (e.g.,Beer, Wine, Spirits...)", FALSE),
      checkboxInput("canada", "Love Canadian liquor? Show Canadian liquor ONLY", FALSE),
    ),

    mainPanel(helpText("There are two tabs: one for viewing the plot; the other for viewing and downloading data"),
      tabsetPanel(

##Assignment 3 - Feature 2: Since I have both a plot and a table, I place them in separate tabs.

##Assignment 4 - Feature 3: I Show the number of results found whenever the filters change.

        tabPanel("Plot", textOutput("id_n_result"),plotOutput("id_histogram")),
        tabPanel("Table", downloadButton("download", "Download results"), DT::dataTableOutput("id_table"))
      )
    )
  )
)

##Assignment 3 - Feature 3: Allow the user to decide whether to only view Canadian liquor.

server <- function(input,output){
  observe(print(input$id_slider))

  bcl_filtered <- reactive(if (input$canada){  bcl %>%
     filter(Price<input$id_slider[2],Price>input$id_slider[1],Sweetness<input$id_sweet[2],Sweetness>input$id_sweet[1])%>% filter(Country=="CANADA")}
     else {  bcl %>%
         filter(Price<input$id_slider[2],Price>input$id_slider[1],Sweetness<input$id_sweet[2],Sweetness>input$id_sweet[1])}
     )

##Assignment 3 - Feature 4: Allow the user to decide whether to show result by liquor type.

  observe(if (input$showtype){
    output$id_histogram <- renderPlot({
      bcl_filtered() %>%
        ggplot(aes(Alcohol_Content)) +
        geom_histogram(aes(fill=Type))})
    } else{
      output$id_histogram <- renderPlot({
        bcl_filtered() %>%
          ggplot(aes(Alcohol_Content)) +
          geom_histogram()})
    })

##Assignment 4 - Feature 4: User can download the table

  output$download <- downloadHandler(
    filename = function() {
      "bcl-results.csv"
    },
    content = function(con) {
      write.csv(bcl_filtered(), con)
    }
  )

##Assignment 3 - Feature 5: Use the DT package to turn a static table into an interactive table

  output$id_table <- DT::renderDataTable({
    bcl_filtered()})

  output$id_n_result<- renderText({
    paste("We found ",
     bcl_filtered()%>% nrow(),"options for you. Here's the histgram." )

    })

}

shinyApp(ui=ui,server=server)
