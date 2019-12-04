library(leaflet)

# Choices for drop-downs

vars1 <- c(
    "Suicide Rate" = "suicides_100k_pop",
    "Suicide number" = "suicides_no"
)

vars3 = c(
    "1992" = "1992",
    "1993" = "1993",
    "1994" = "1994",
    "1995" = "1995",
    "1996" = "1996",
    "1997" = "1997",
    "1998" = "1998",
    "1999" = "1999",
    "2000" = "2000",
    "2001" = "2001",
    "2002" = "2002",
    "2003" = "2003",
    "2004" = "2004",
    "2005" = "2005",
    "2006" = "2006",
    "2007" = "2007",
    "2008" = "2008",
    "2009" = "2009",
    "2010" = "2010",
    "2011" = "2011",
    "2012" = "2012",
    "2013" = "2013",
    "2014" = "2014",
    "2015" = "2015",
    "2016" = "2016"
)

navbarPage("Suicides", id="nav",

           tabPanel("Interactive map",
                    div(class="outer",
                      tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                        ),
                      
                      # If not using custom CSS, set height of leafletOutput to a number instead of percent
                      
                      leafletOutput("map", width="100%", height="100%"),
                      
            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                          width = 330, height = "auto",
                          
                          h2("Suicide Analysis"),
                          
                          selectInput("metric", "Metric", vars1),
                          selectInput("year1", "Year", vars3, selected = "1992"),
           
             plotOutput("hist", height = 200),
             plotOutput("scatter", height = 250)),
            
                          tags$div(id="cite",
                                 'Data compiled for ', tags$em('Suicide Rates Overview 1985 to 2016, Compares socio-economic info with suicide rates by year and country')
                        ))),
           
          tabPanel("Data explorer",
                   fluidRow(
                       column(2,
                              selectInput("country", "Country", maindata$country, c("All countrys"=""), multiple=TRUE)
                        ),
                  column(2,conditionalPanel("input.country",
                                            selectInput("year", "Year", maindata$year, c("All years"=""), multiple=TRUE)
                                            )
                        )
                       ),
                  fluidRow(
                      column(1,
                             numericInput("minRate", "Min rate", min=0, max=60, value=0)
                            ),
                      column(1,
                             numericInput("maxRate", "Max rate", min=0, max=60, value=60)
                            )
                    ),
                    
                    hr(),
                  
                    DT::dataTableOutput("suicidetable")
                  ),
          tabPanel(HTML("|</a></li><li><a href=\"https://suicideproj.github.io\">Home")),   
          
          conditionalPanel("false", icon("crosshair"))
)