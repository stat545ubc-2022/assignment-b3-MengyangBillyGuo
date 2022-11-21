library(shiny)
library(tidyverse)
library(DT)
bcl <-read.csv("https://raw.githubusercontent.com/daattali/shiny-server/master/bcl/www/bcl-data.csv")


ui <- fluidPage(
  titlePanel("BC liquor store App"),
  h4("use this app to explore alcohol content"),

##Feature 1: I added an image of the BC Liquor Store.

  img(src = "logo.jpg"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("id_slider","select a price range:", min=0, max=100, value=c(25,40),pre="$"),
      checkboxInput("showtype", "Show result by type", FALSE),
      checkboxInput("canada", "Show Canadian liquor ONLY", FALSE),
    ),
    mainPanel(
      tabsetPanel(

##Feature 2: Since I have both a plot and a table, I place them in separate tabs.

        tabPanel("Plot", plotOutput("id_histogram")),
        tabPanel("Table",  DT::dataTableOutput("id_table"))
      )
    )
  )
)

##Feature 3: Allow the user to decide whether to only view Canadian liquor.

server <- function(input,output){
  observe(print(input$id_slider))

  bcl_filtered <- reactive(if (input$canada){  bcl %>%
     filter(Price<input$id_slider[2],Price>input$id_slider[1])%>% filter(Country=="CANADA")}
     else {  bcl %>%
         filter(Price<input$id_slider[2],Price>input$id_slider[1])}
     )

##Feature 4: Allow the user to decide whether to show result by liquor type.

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


##Feature 5: Use the DT package to turn a static table into an interactive table

  output$id_table <- DT::renderDataTable({
    bcl_filtered()})
}

shinyApp(ui=ui,server=server)
