library(shiny)
library(tidyverse)
library (shinythemes)
sonnets <- read_csv("https://blades.byu.edu/hon290/queneauEng.txt")
##Print arbitrary sonnet Function##
printSonnet <-function(whichsonnet){
  library(tidyverse)
  sonnets %>%
    filter(sonnet==whichsonnet) %>%
    select(text) %>%
    print()
}
##Generate Random Sonnet Function##
rando <- function(){
  vect<-NULL
  vect<-sample(1:10, 14, replace = T)
  randsonnet <- NULL
  for (i in 1:14){
    nextline <- sonnets %>%
      filter (sonnet == vect[i] & line == i)
    randsonnet <- bind_rows(randsonnet, nextline)
  }
  randsonnet%>%
    select(text)%>%
    print() 
}

## For Arbitrary Discrete Combinations##
Arbsonnet <- function(user){
  mylines <- c(user)
  for(i in 1:14){
    sonnets %>%
      filter(sonnet ==mylines[i], line ==i)
  }
  posonnet <- NULL
  for (i in 1:14){
    nextline <- sonnets %>%
      filter (sonnet == mylines[i] & line == i)
    posonnet <- bind_rows(posonnet, nextline)
  }
  posonnet%>%
    select(text)%>%
    print()
}
######################################################

##Start Shiny##
ui <- fluidPage(theme=shinytheme("cyborg"),
  titlePanel("Queneau's Potential Sonents \n\n"),
  tabsetPanel(
    tabPanel( "Original or Random",
      sidebarLayout(
      sidebarPanel(
        fluidRow(numericInput("whichsonnet", "Choose a single sonnet or generate a random one",0, min=1, max=10, value=1)),
        actionButton("d","Generate Random")
      ),
      mainPanel(
        fluidRow(
         column(6,h5("These are the sonnets verbatim (but translated from the French)"),
                tableOutput("q")),
         column(6,h5("Compare to a randomly generated sonnet of randomly picked lines")),
         tableOutput("s"))
         ),
      )
),
tabPanel( "Choose by Line",
  sidebarLayout(
    sidebarPanel(
    fluidRow(
      column(6, numericInput("s1", "line 1 from", 1, min=1, max =10, value =1)),
      column(6, numericInput("s2", "line 2 from", 1, min=1, max =10, value =1))
      ),
    fluidRow(
      column(6, numericInput("s3", "line 3 from", 1, min=1, max =10, value =1)),
      column(6, numericInput("s4", "line 4 from", 1, min=1, max =10, value =1))
      ),
    fluidRow(
     column(6, numericInput("s5", "line 5 from", 1, min=1, max =10, value =1)),
     column(6, numericInput("s6", "line 6 from", 1, min=1, max =10, value =1))
    ),
    fluidRow(
      column(6, numericInput("s7", "line 7 from", 1, min=1, max =10, value =1)),
      column(6, numericInput("s8", "line 8 from", 1, min=1, max =10, value =1))
    ),
    fluidRow(
      column(6, numericInput("s9", "line 9 from", 1, min=1, max =10, value =1)),
      column(6, numericInput("s10", "line 10 from", 1, min=1, max =10, value =1))
      ),
    fluidRow(
      column(6, numericInput("s11", "line 11 from", 1, min=1, max =10, value =1)),
      column(6, numericInput("s12", "line 12 from", 1, min=1, max =10, value =1))
    ),
    fluidRow(
      column(6, numericInput("s13", "line 13 from", 1, min=1, max =10, value =1)),
      column(6, numericInput("s14", "line 14 from", 1, min=1, max =10, value =1))
    )),
    mainPanel(
      tableOutput("r"))
)),
tabPanel("More information", 
         h1("Raymond Queneau's \"Hundred Thousand Billion Poems\"", align= "center" ),
         h5(" Queneau's \"Hundren Thousand Billion Poems\" is a collection of 10 
           Sonnets of identical rhyming structure that allow lines to be interchanged, 
           creating 10^14 possible poems. That is a number with 14 zeros or
           100,000,000,000,000."),
         br(), 
         img(src = "quneau.jpg", width = 500, height = 400), 
         br(),
         h5( "Using these 10 poems, this program allows you to 
           discover new peoms by selecting lines from any given sonnet, 
           reading each of the sonnets as each were written, or generating a random sonnet of the 10^14 possibilities."),
         a("Click here for more information", href = "https://cellproject.net/creative-work/100000000000000-poems-0", 
           style = "font-size:26px")
         ))
)
server <- function(input, output){
  output$q <- renderTable({
    printSonnet(input$whichsonnet)
  })
  output$s <-renderTable({
    input$d
    rando()
  })
  
  output$r <- renderTable({
    Arbsonnet(c(input$s1,input$s2,input$s3,input$s4,input$s5,input$s6,input$s7,input$s8,input$s9,input$s10,
                input$s11,input$s12,input$s13,input$s14))
   
    })
  
    
  #output$d <-renderTable({
   # input$d
    #if (input$d == 0)
     # return()
    #isolate(rando())
#})
} 
  
shinyApp(ui=ui, server=server)

