# Creating Somerville Community Choice Aggregation page to compare old and new rates, and effect of going to 100% renewable on typical monthy bill
# Started July 2017
# danfbflynn@gmail.com

# Typical kWh: US avereage 901
# https://www.eia.gov/tools/faqs/faq.php?id=97&t=3
# Massachusetts: 602

library(shiny)
library(shinydashboard)
#library(ggplot2)
#library(plotly)
#library(shinyjs)

# Deploy: make sure options(repos = c(CRAN = "https://cran.rstudio.com"))

ui <- dashboardPage(skin = "black",
                    
  dashboardHeader(title = "Comparing Renewable Electricity Options for Somerville", titleWidth = 650),

  # Dashboard Menu
    dashboardSidebar(collapsed = TRUE,
              sidebarMenu(
                   menuItem("Rate Comparison", tabName = "ratecompare", icon = icon("dashboard")),
                   menuItem("About", tabName = "about", icon = icon("th"))
                   ) # end sidebarMenu
              ),

  # Body
    dashboardBody(
    # Apply custom css here
  #  tags$head(
   #   tags$link(rel = "stylesheet", type = "text/css", href = "cosmo.css")),
   
   # useShinyjs(),
   
    tabItems(
     tabItem(tabName = "ratecompare",
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(width = 12,
        title = "Typical Monthly Electricity Usage ",
        sliderInput("kwhslider", "Usage in kWh:", 1, 1000, 450)
      )),
    
    fluidRow(
      box(plotOutput("plot1", height = 350)),
  
             box(h2(htmlOutput("dollartext"))),
             box(h2(htmlOutput("pcttext")))
    ),
    
    fluidRow(
      box(width = 12, title = "Electricity generation rates", 
          splitLayout(
            numericInput("rate5", "Default +5% renewable", value = 0.10538, step = 0.001),
            numericInput("rate100", "Opt up 100% renewable", value = 0.13198, step = 0.001),
            numericInput("rateprev", "Previous rate June 2017", value = 0.10759, step = 0.001)
          ))),
    fluidRow(
            box(width = 12, title = "Electricity delivery rates", 
            splitLayout(
              numericInput("custcharge", "Customer account charge", value = 6.43, step = 0.50),
              numericInput("deliverycharges", "Delivery charges", value = 0.092, step = 0.001)
                ))
      )
    ), # End tabItem ratecompare
    tabItem(tabName = "about",
      h2("About this page"),
      fluidPage(
        box(width = 12,
          title = "Somerville Community Choice Electricity (CCE) Aggregation",
            "The Somerville Community Choice Electricity (CCE) Aggregation began July 2017.", br(), "See the", 
            a("Somerville CCE page", href="https://somervillecce.com"),
            "for more information."),
        
        box(width = 12,
          title = "This calculator",
            "Translating the information on the", a("CCE mailer", href="https://somervilledev.files.wordpress.com/2016/10/somerville-opt-out-eversource-2017-final.pdf"), "to your bill is not straightforward. The new program will acheive stable electricity pricing, increase Somerville's commitment to local renewable energy, and provide residents an option to opt up to 100% local renewable energy as their source of electricity. This calculator attempts to provide an easy way to compare how the options will affect your bill, with a goal of showing that the benefits of 100% local renewable energy can be acheived with a relatively modest increase in a household's bill.", br(),br(),
          "To select the Somerville 100% Local Green option,", a("contact Dynegy at (866) 220-5696, Monday through Friday from 9AM to 8PM EST, or via email at DESCustCare@Dynegy.com", href="https://somervillecce.com/how-to-select-the-somerville-100-local-green-option/"),". You will need your Eversource account nubmer."),
        
        box(width = 12,
            title = "Calculating an electrical bill",
          "A monthly bill is calculated as follows:", br(),
            tags$em("kWh * Generation Rate + kWh * Delivery Rate + Customer Charge"), br(), br(),
          "The Generation Rate is what is being changed by the CCE plan. By opting up to 100%, only the Generation Rate aspect would increase.",
          a(img(src="https://somervilledev.files.wordpress.com/2016/10/somerville-chart.jpg", width = 700, alt = "CCE Chart"), href = "https://somervilledev.files.wordpress.com/2016/10/somerville-chart.jpg"),br(),
          "Delivery Rates are composed of multiple items, see your Eversource bill for details. The delivery rates shown in the calculator by default are from June 2017 for a residential customer.",
          br(),
          "For more details on these rates, see the Eversource page on ",
          a("Understanding My Electricity Bill", href="https://www.eversource.com/Content/ema-c/residential/my-account/billing-payment/understanding-my-bill", ".")
            ),
        
        box(width = 12,
            title = "Feedback",
            "This is a 0.001 beta version, written in ",
            a("Shiny.", href="http://shiny.rstudio.com"),
            "Please leave feedback on ",
            a("this Reddit thread!", href="https://www.reddit.com/r/Somerville/comments/6l57xs/compare_renewable_electricity_options_under_the/"),
        "Or find this on ", a("GitHub.", href="https://github.com/danfbflynn/somervillecce/"))
        )
            ) # end tabItem about
   ) # end tabItems
  ) # end dashboardBdy
) 



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

  


shinyApp(ui, server)