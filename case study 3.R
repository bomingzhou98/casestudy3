library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(shiny)
library(rsconnect)

load("moviedata.RData")

rsconnect::setAccountInfo(name='z1nked-boming-zhou',
                          token='53DB0FE5ED2E8812E23DD7ADA66E39F7',
                          secret='Xh+KLM+0kEmNtfLryvBw/4GJhLQkyvm0BVgscqir')

vars <- setdiff(names(allmovies), "Movies")
ui <- pageWithSidebar(
    headerPanel('Clustering of movies based on IMDb and Metacritic scores'),
    sidebarPanel(
      selectInput('xcolumn', 'X Variable', vars),
      selectInput('ycolumn', 'Y Variable', vars, selected = vars[[2]]),
      sliderInput("slider1", label = h3("Slider"), min = 1, 
                  max = 5, value = 1),
      
      selectInput('xcolumn2', 'X Variable2', vars),
      selectInput('ycolumn2', 'Y Variable2', vars, selected = vars[[2]]),
      sliderInput("slider2", label = h3("Slider"), min = 1, 
                  max = 5, value = 1)
    ),
    
    mainPanel(
      plotOutput('plot1'),
      
      plotOutput('plot2')
    )

  )


server <- function(input, output, session) {
  
  selectedData <- reactive({
    allmovies[, c(input$xcolumn, input$ycolumn)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$slider1)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    
    
  })
  
  selectedData2 <- reactive({
    allmovies[, c(input$xcolumn2, input$ycolumn2)]
  })
  
  clusters2 <- reactive({
    kmeans(selectedData2(), input$slider2)
  })
  
  output$plot2 <- renderPlot({
    palette(c("#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData2(),
         col = clusters2()$cluster,
         pch = 20, cex = 3)
    points(clusters2()$centers, pch = 4, cex = 4, lwd = 4)
    
    
  })
  
}

#### K-Means Cluster graph code#######

# mydata = select(moviedata, c(4,5,6))
# 
# #WSS plot function
# 
# wssplot = function(data, nc=15, seed=1234)
# {
#   wss = (nrow(data)-1)*sum(apply(data,2,var))
#   for (i in 2:nc){
#     set.seed(seed)
#     wss[i] = sum(kmeans(data, centers=i)$withinss)}
#   plot(1:nc, wss, type="b", xlab="Number of Clusters",
#        ylab="Within groups of sum of squares")
# }
# 
# wssplot(mydata)
# 
# KM = kmeans(mydata,4)

#autoplot(KM, mydata, frame=TRUE)

#KM$centers
#######################################

shinyApp(ui = ui, server = server)



