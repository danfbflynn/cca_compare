library(shiny)

server <- function(input, output, session) {
  
  ##addClass(selector = "body", class = "sidebar-collapse")
  
  roundUp <- function(x, to = 100){
    # Function for rounding up to nearest 100
    to*(x%/%to + as.logical(x%%to))
  }
  
  output$plot1 <- renderPlot({
    plotcols <- c("palegreen3","palegreen4","deepskyblue")
    
    bill5 <- input$custcharge + input$deliverycharges * input$kwhslider + input$rate5 * input$kwhslider
    bill100 <- input$custcharge + input$deliverycharges * input$kwhslider + input$rate100 * input$kwhslider
    billprev <- input$custcharge + input$deliverycharges * input$kwhslider + input$rateprev * input$kwhslider
    
    plotdat <- c(bill5, bill100, billprev)
    
    par(xpd=TRUE, cex = 1.25)
    
    bp <- barplot(plotdat,
                  ylim = c(0, roundUp(1.1*max(plotdat))),
                  ylab = "Monthly bill ($)",
                  xlab = "Rates",
                  col = plotcols,
                  main = "Typical Monthly Bill")
    
    text(x = bp, y = plotdat + max(plotdat) * 0.15,
         labels = paste("$", format(
           round(plotdat, 2),
           nsmall = 2)),
         cex = 1.5, font = 2)
    
    axis(1, at = bp, 
         lwd = 0,
         labels = paste(
           c("+5% Renewable \n", 
             "100% Renewable \n",
             "Previous \n "),
           c(round(100*input$rate5, 1),
             round(100*input$rate100, 1),
             round(100*input$rateprev, 1)),
           "¢")
    )
  })
  
  output$dollartext <- renderText({
    bill5 <- input$custcharge + input$deliverycharges * input$kwhslider + input$rate5 * input$kwhslider
    bill100 <- input$custcharge + input$deliverycharges * input$kwhslider + input$rate100 * input$kwhslider
    
    paste("Your typical monthly bill would be <b>$",
          format(round(bill100 - bill5, 2), nsmall = 2), 
          "</b> greater if you choose the 100% renewable option (Somerville 100% Local Green)",
          sep = "")
    
  })
  
  output$pcttext <- renderText({
    bill5 <- input$custcharge + input$deliverycharges * input$kwhslider + input$rate5 * input$kwhslider
    bill100 <- input$custcharge + input$deliverycharges * input$kwhslider + input$rate100 * input$kwhslider
    
    r1 <- bill100/bill5
    r2 <- r1-1
    paste("This would be a <b>",
          round(100*(r2), 1),
          "%</b> increase over the +5% renewable option (Somerville Local Green)",
          sep = "")
    
  })
}

