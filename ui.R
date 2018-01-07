# Creating Somerville Community Choice Aggregation page to compare old and new rates, and effect of going to 100% renewable on typical monthy bill
# Started July 2017
# danfbflynn@gmail.com

# Typical kWh: US avereage 901
# https://www.eia.gov/tools/faqs/faq.php?id=97&t=3
# Massachusetts: 602

library(shiny)
library(shinydashboard)
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
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "cosmo.css")),
                      
                      # useShinyjs(),
                      
                      # Trying to customize the slider bar. Can we make it follow the page CSS?
                      # Height of the number hovering over the slider is controlled in .irs-single, top: -25%. 
                      
                      tags$style(HTML(".irs-bar {width: 100%; height: 12px; background: black; border-top: 1px solid black; border-bottom: 1px solid black;}")),
                      tags$style(HTML(".irs-bar-edge {background: black; border: 1px solid black; height: 12px; border-radius: 6px 6px 6px 6px;}")),
                      tags$style(HTML(".irs-line {border: 1px solid black; height: 11px;}")),
                      tags$style(HTML(".irs-grid-text {font-family: 'Segoe UI', 'Source Sans Pro', Calibri, Candara, Arial, sans-serif; color: black; font-size: 15px}")),
                      tags$style(HTML(".irs-max {font-family: 'arial'; color: black;}")),
                      tags$style(HTML(".irs-min {font-family: 'Segoe UI', 'Source Sans Pro', Calibri, Candara, Arial, sans-serif; color: black;}")),
                      tags$style(HTML(".irs-single {color:black; background:#ffffff; font-size: 25px; font-weight: bold; font-family: 'Segoe UI', 'Source Sans Pro', Calibri, Candara, Arial, sans-serif; top: -25%}")),
                      tags$style(HTML(".irs-slider {width: 16px; height: 16px; top: 24px;}")),
                      
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
                                        numericInput("rate100", "Opt up 100% renewable", value = 0.13198, step = 0.001)
                                      ))),
                                fluidRow(
                                  box(width = 12, title = "Electricity delivery rates", 
                                      splitLayout(
                                        numericInput("custcharge", "Customer account charge", value = 6.43, step = 0.50),
                                        numericInput("deliverycharges", "Delivery charges", value = 0.0938, step = 0.001)
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
                                      "Translating the information on the", a("CCE mailer", href="https://somervilledev.files.wordpress.com/2017/12/somerville-opt-out-chart-2018.pdf"), "to your bill takes a bit of work. This program aims to acheive stable electricity pricing, increase Somerville's commitment to local renewable energy, and provide residents an option to opt up to 100% local renewable energy as their source of electricity. This calculator provides an easy way to compare how the options will affect your bill, with a goal of showing that the benefits of 100% local renewable energy can be acheived with a relatively modest increase in a household's bill.", br(),br(),
                                      "To select the Somerville 100% Local Green option,", a("contact Dynegy at (866) 220-5696, Monday through Friday from 9AM to 8PM EST, or via email at DESCustCare@Dynegy.com", href="https://somervillecce.com/how-to-select-the-somerville-100-local-green-option/"),". You will need your Eversource account nubmer.",br(), "Alternatively, fill out the information in", a("this form on the Somerville CCE site.", href = "https://somervillecce.com/opt-up-to-premium-100-local-green/")),
                                  
                                  box(width = 12,
                                      title = "Calculating an electrical bill",
                                      "A monthly bill is calculated as follows:", br(),
                                      tags$em("kWh * Generation Rate + kWh * Delivery Rate + Customer Charge"), br(), br(),
                                      "The Generation Rate is what is being changed by the CCE plan. By opting up to 100%, only the Generation Rate aspect would increase.",
                                      a(img(src="https://somervilledev.files.wordpress.com/2017/12/somerville-opt-out-chart-2018.pdf", width = 700, alt = "CCE Chart"), href = "https://somervilledev.files.wordpress.com/2017/12/somerville-opt-out-chart-2018.pdf"),br(),
                                      "Delivery Rates are composed of multiple items, see your Eversource bill for details. The delivery rates shown in the calculator by default are from January 2018 for a residential customer.",
                                      br(),
                                      "For more details on these rates, see the Eversource page on ",
                                      a("Understanding My Electricity Bill", href="https://www.eversource.com/Content/ema-c/residential/my-account/billing-payment/understanding-my-bill", ".")
                                  ),
                                  
                                  box(width = 12,
                                      title = "Feedback",
                                      "This is a beta version, written in ",
                                      a("Shiny.", href="http://shiny.rstudio.com"),
                                      "Please leave feedback by making an Issue on", a("GitHub.", href="https://github.com/flynn-d/somervillecce/"), "or by contacting dan@flynnd.io")
                                )
                        ) # end tabItem about
                      ) # end tabItems
                    ) # end dashboardBdy
) 
