library(shiny)
library(svglite)
library(png)
library(ggplot2)

# Define the function to draw the rose
draw_rose <- function(n, d, maurer, size, gg, colorize, drawRose, drawMaurer, xlim, ylim, step) {
  par(mar = c(0, 0, 0, 0))  # Remove margins
  plot(NULL, xlim = xlim, ylim = ylim, type = "n", xlab = "", ylab = "", asp = 1, axes = FALSE)
  
  k <- n / d
  

  # Draw either the Maurer pattern or the additional rose pattern
  if (drawRose) {
    # Draw the additional rose pattern
    xPrev <- yPrev <- NULL
    for (a in seq(0, 3600 * ceiling(d) / 6, by = step)) {
      deg <- a * pi / 180
      r <- sin(-k * deg - 0) * size + round(gg)
      x <- r * cos(deg)
      y <- r * sin(deg)
      if(!is.null(xPrev)) {
        lines(c(xPrev, x), c(yPrev, y), col = "red")
      }
      xPrev <- x
      yPrev <- y
    }
  } else if (drawMaurer) {
    # Draw the Maurer rose pattern
    for (i in seq(0, 3600, by = step)) {
      fi <- maurer * i * pi / 180
      r <- sin(-k * fi - 0) * size + round(gg)
      x <- r * cos(fi)
      y <- r * sin(fi)
      if(colorize) {
        col <- rainbow(360)[(i %% 360) + 1]
      } else {
        col <- "black"
      }
      if(i > 0) {
        lines(c(xPrev, x), c(yPrev, y), col = col)
      }
      xPrev <- x
      yPrev <- y
    }
  }
}




ui <- fluidPage(
    titlePanel("Maurer Rose Drawing App"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("nSlider", "n Value", min = 0.1, max = 20, value = 2, step = 0.01),
            sliderInput("dSlider", "d Value", min = 0.1, max = 180, value = 20, step = 0.01),
            sliderInput("maurerSlider", "Maurer Connection Angle", min = 1, max = 359, value = 50, step = 0.1),
            sliderInput("sizeSlider", "Size", min = 200, max = 2000, value = 500, step = 1),
            sliderInput("ggSlider", "Radius Adjustment", min = 0, max = 500, value = 0, step = 10),
            sliderInput("stepSlider", "Pattern Variation", min = 1, max = 100, value = 1, step = 1),
            checkboxInput("colorize", "Colorize"),
            checkboxInput("drawRose", "Draw Rose"),
            actionButton("randomizeBtn", "Randomize"),
            actionButton("saveBtn", "Save")
        ),
        mainPanel(
            plotOutput("plot", width = "1000px", height = "1000px")
        )
    )
)


# Define server logic
server <- function(input, output, session) {
    observeEvent(input$randomizeBtn, {
        # Update sliders
        updateSliderInput(session, "nSlider", value = runif(1, min = 1, max = 20))
        updateSliderInput(session, "dSlider", value = runif(1, min = 1, max = 180))
        updateSliderInput(session, "maurerSlider", value = runif(1, min = 1, max = 359))
        updateSliderInput(session, "ggSlider", value = runif(1, min = 0, max = 500))
        updateSliderInput(session, "stepSlider", value = runif(1, min = 1, max = 100))
    })

  output$plot <- renderPlot({
    max_extent <- abs(input$sizeSlider) + 100  # Dynamic extent based on size slider
    draw_rose(input$nSlider, input$dSlider, input$maurerSlider, input$sizeSlider, 
              input$ggSlider, input$colorize, input$drawRose, TRUE,
              xlim = c(-max_extent, max_extent), ylim = c(-max_extent, max_extent), input$stepSlider)
  })
  

    observeEvent(input$saveBtn, {
        filename <- paste("Maurer_rose_n", input$nSlider, "_d", input$dSlider, 
                          "_maurer", input$maurerSlider, "_gg", input$ggSlider, "_step", input$stepSlider, sep="")

        # Define the file paths for PNG and SVG
        png_filepath <- file.path(getwd(), paste0(filename, ".png"))
        svg_filepath <- file.path(getwd(), paste0(filename, ".svg"))
        
        max_extent <- abs(input$sizeSlider) + 100  # Dynamic extent for saving

        # Save as PNG
        png(png_filepath, width = 1800, height = 1800)
        draw_rose(input$nSlider, input$dSlider, input$maurerSlider, input$sizeSlider, 
                  input$ggSlider, input$colorize, input$drawRose, TRUE,
                  xlim = c(-max_extent, max_extent), ylim = c(-max_extent, max_extent), input$stepSlider)
        dev.off()
        
        # Save as SVG
        svg(svg_filepath, width = 800, height = 800)
        draw_rose(input$nSlider, input$dSlider, input$maurerSlider, input$sizeSlider, 
                  input$ggSlider, input$colorize, input$drawRose, TRUE,
                  xlim = c(-max_extent, max_extent), ylim = c(-max_extent, max_extent), input$stepSlider)
        dev.off()
        
    })
}

shinyApp(ui, server)