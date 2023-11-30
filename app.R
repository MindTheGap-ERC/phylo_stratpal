library(shiny)

load("adm_list.Rdata")

tree_time = ape::rlineage(birth = 1.5, death = 0, Tmax = 2)
ui = fluidPage(
  sidebarLayout(
  sidebarPanel(
    sliderInput("adm_index","index",1,150,1, animate = TRUE),
    width = 2
    ),
  mainPanel( 
    fluidRow(
      column(6, 
             plotOutput(outputId = "strat_tree_plot")),
      column(6, 
      plotOutput(outputId = "adm_plot"))

    ),
    fluidRow(
      column(6),
      column( 6,     plotOutput(outputId = "time_tree_plot"))
    )


  )
  )

)

server = function(input, output){
  
  
  output$time_tree_plot = renderPlot({
    plot(tree_time)
    axis(1)
    mtext("Time [Myr]", side = 1, line = 3)
  }
  )
  
  output$adm_plot = renderPlot({
    plot(adm_list[[input$adm_index]], main = paste("Age-depth model", input$adm_index / 10 , "km off shore"))
  }
  )
  
  output$strat_tree_plot = renderPlot({
    plot(time_to_strat(tree_time, adm_list[[input$adm_index]]), direction = "upwards")
    axis(2)
    mtext("Stratigraphic Height [m]", side = 2, line = 3)
  }
  )
  
  
}

shinyApp(ui,server)