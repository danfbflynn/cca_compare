library(shiny)
library(shinydashboard)

# Lookup table for reneable portfolio standards from 2018-2030
# https://www.mass.gov/service-details/program-summaries
rps_frame = data.frame(Year = 2018:2030,
                       RPS = 13:25)

rps_yr = rps_frame[rps_frame$Year == format(Sys.Date(), "%Y"),"RPS"]

# test values

# input = list(custcharge = 6.43,
#              co2avoid = 0.857,
#              deliverycharges = 0.09824,
#             kwhslider = 450,
#             rate5 = 0.10538,
#             rate100 = 0.13198)


server <- function(input, output, session) {
  ##addClass(selector = "body", class = "sidebar-collapse")
  roundUp <- function(x, to = 100){
    # Function for rounding up to nearest 100
    to*(x%/%to + as.logical(x%%to))
  }
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(custcharge = input$custcharge,
                     deliverycharges = input$deliverycharges,
                     kwhslider = input$kwhslider,
                     rate5 = input$rate5,
                     co2avoid = input$co2avoid,
                     rate100 = input$rate100)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$plot1 <- renderPlot({
    par(mar = c(6, 5, 4, 2))
    plotcols <- c("palegreen3","deepskyblue3",
                  "palegreen3", "deepskyblue3")
    
    bill10 <- input$custcharge + input$deliverycharges * input$kwhslider + input$rate5 * input$kwhslider
    bill100 <- input$custcharge + input$deliverycharges * input$kwhslider + input$rate100 * input$kwhslider
    co2_10 <- input$kwhslider * (rps_yr+5)/100 * input$co2avoid 
    co2_100 <- input$kwhslider * input$co2avoid
    
    plotdat <- c(bill10, -co2_10, bill100, -co2_100)
    par(xpd=TRUE, cex = 1.25)
    
    bp <- barplot(matrix(plotdat, nrow = 2), beside= T,
                  ylim = c(roundUp(1.1*min(plotdat)), roundUp(1.1*max(plotdat))),
                  yaxt = "n",
                  xlab = '',#"Rates",
                  col = plotcols,
                  main = "Typical Monthly Bill and CO[2] Avoided"
    )
    axis(2, at = seq(0, roundUp(1.1*max(plotdat)), by = 100),
         labels = seq(0, roundUp(1.1*max(plotdat)), by = 100))
    mtext("Monthly bill \n ($)", side = 2, line = 2.5,
          cex = 1.25, at = median(seq(0, roundUp(1.1*max(plotdat)), by = 100))+50)
    axis(2, at = seq(-100, roundUp(0.9*min(plotdat)), by = -100),
         labels = seq(100, -roundUp(0.9*min(plotdat)), by = 100))
    
    mtext("CO[2] avoided \n (lbs)", side = 2, line = 2.5,
          cex = 1.25, at = median(seq(-100, roundUp(0.9*min(plotdat)), by = -100))-50)
    
    text(x = bp[1,], y = plotdat[c(1,3)] + max(plotdat[c(1,3)]) * 0.25,
         labels = paste("$", format(
           round(plotdat[c(1,3)], 2),
           nsmall = 2)),
         cex = 1.3, font = 2)
    
    text(x = bp[2,], y = plotdat[c(2,4)] + min(plotdat[c(2,4)]) * 0.05,
         labels = paste(round(-plotdat[c(2,4)], 1), "lbs"),
         cex = 1.3, font = 2)
    
    axis(1, at = bp[1,]+0.25, 
         line = 3,
         lwd = 0,
         labels = paste(
           c("+10% Renewable \n", 
             "100% Renewable \n"
             #,"Previous \n "
           ),
           c(round(100*input$rate5, 1),
             round(100*input$rate100, 1)
             #,round(100*input$rateprev, 1)
           ),
           "¢")
    )
  }) # end plot1
  
  # other option: two plots, one for $ and one for CO2
  output$plot0 <- renderPlot({
    
    plotcols <- c("palegreen3", "palegreen4")
    
    bill10 <- input$custcharge + input$deliverycharges * input$kwhslider + input$rate5 * input$kwhslider
    bill100 <- input$custcharge + input$deliverycharges * input$kwhslider + input$rate100 * input$kwhslider
    
    plotdat <- c(bill10, bill100)
    par(xpd=TRUE, cex = 1.25)
    
    bp <- barplot(plotdat,
                  ylim = c(0, roundUp(1.1*max(plotdat))),
                  yaxt = "n",
                  xlab = "Rates",
                  col = plotcols,
                  main = "Typical Monthly Bill"
    )
    axis(2, at = seq(0, roundUp(1.1*max(plotdat)), by = 100),
         labels = seq(0, roundUp(1.1*max(plotdat)), by = 100))
    mtext("Monthly bill ($)", side = 2, line = 2.5,
          cex = 1.25, at = median(seq(0, roundUp(1.1*max(plotdat)), by = 100)))
    
    text(x = bp, y = plotdat + max(plotdat) * 0.25,
         labels = paste("$", format(
           round(plotdat, 2),
           nsmall = 2)),
         cex = 1.3, font = 2)
    
    axis(1, at = bp, 
         lwd = 0,
         labels = paste(
           c("+10% Renewable \n", 
             "100% Renewable \n"
             #,"Previous \n "
           ),
           c(round(100*input$rate5, 1),
             round(100*input$rate100, 1)
             #,round(100*input$rateprev, 1)
           ),
           "¢")
    )
  }) # end plot0
  
  output$plot2 <- renderPlot({
    plotcols2 <- c("deepskyblue3","deepskyblue4")
    
    co2.5 <- input$co2avoid * input$kwhslider * (rps_yr+5)/100 
    co2.100 <- input$co2avoid * input$kwhslider
    plotdat2 <- c(co2.5, co2.100)
    par(xpd=TRUE, cex = 1.25)
    
    bp2 <- barplot(plotdat2,
                   ylim = c(0, roundUp(1.1*max(plotdat2))),
                   ylab = "Pounds of CO[2]",
                   xlab = "Rates",
                   col = plotcols2,
                   main = "Emissions avoided per month")
    
    text(x = bp2, y = plotdat2 + max(plotdat2) * 0.15,
         labels = paste(round(plotdat2, 1), "lbs"),
         cex = 1.5, font = 2)
    
    axis(1, at = bp2, 
         lwd = 0,
         labels = paste(
           c("+10% Renewable \n", 
             "100% Renewable \n"
           ),
           c(round(100*input$rate5, 1),
             round(100*input$rate100, 1)
           ),
           "¢")
    )
  })
  
  output$dollartext <- renderText({
    bill10 <- input$custcharge + input$deliverycharges * input$kwhslider + input$rate5 * input$kwhslider
    bill100 <- input$custcharge + input$deliverycharges * input$kwhslider + input$rate100 * input$kwhslider
    
    paste("Your typical monthly bill would be <b>$",
          format(round(bill100 - bill10, 2), nsmall = 2), 
          "</b> greater if you choose the 100% renewable option (Somerville 100% Local Green)",
          sep = "")
    
  })
  
  output$pcttext <- renderText({
    bill10 <- input$custcharge + input$deliverycharges * input$kwhslider + input$rate5 * input$kwhslider
    bill100 <- input$custcharge + input$deliverycharges * input$kwhslider + input$rate100 * input$kwhslider
    
    r1 <- bill100/bill10
    r2 <- r1-1
    paste("This would be a <b>",
          round(100*(r2), 1),
          "%</b> increase over the +10% renewable option (Somerville Local Green)",
          sep = "")
    
  })
  
  output$co2avoid <- renderText({
    co2_10 <- input$kwhslider * (rps_yr+5)/100 * input$co2avoid # lbs of CO2 per kWh of renewable energy
    co2_100 <- input$kwhslider * input$co2avoid
    
    r1 <- co2_100/co2_10
    r2 <- r1-1
    
    r3 <- co2_100-co2_10
    
    paste("This would be a <b>",
          round(r2, 1),
          "x</b> decrease in CO2 emissions compared to the +10% renewable option. </br>",
          paste("This is equivalent to removing an additional <b>",
                round(r3*12/1000, 1),
                "</b> tons of CO2 per year, just for your household"),      
          sep = "")
    
  })
  
}
