# Shiny Application

# Load libraries
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(tidyverse)
library(leaflet)
# library(oregonfrogs) not on CRAN
# let's build the oregonfrogs data
library(tidyverse)
oregonfrogs_raw <-
  read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv"
  )


frogs_coord <- oregonfrogs_raw %>%
  dplyr::select(UTME_83, UTMN_83)

# Tranform it to lat and long
longlat <-
  frogs_coord %>%
  # transform to simple features as geometry
  sf::st_as_sf(coords = c(1, 2),
               crs = "+proj=utm +zone=10") %>%
  # utm tranformation to longlat
  sf::st_transform(crs = "+proj=longlat +datum=WGS84")  %>%
  sf::st_coordinates() %>%
  as.data.frame() %>%
  rename(long = X, lat = Y)

oregonfrogs <- cbind(longlat, oregonfrogs_raw) %>%
  select(-UTME_83,-UTMN_83, -Site) %>%
  janitor::clean_names() %>%
  rename(doy=ordinal,sex=female)

######## ######## ######## ######## ######## ######## ######## ########

button_color_css <- "#Search{
/* Change the background color of the update button
to blue. */
background: #32cd32;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"

#007BA7;
background_color <- '.container-fluid {
                             background-color: #cf5f25; 
              }'

######## ######## ######## ######## ######## ######## ######## ########

# Define UI
ui <- fluidPage(
  navbarPage("Oregon Spotted Frog - Rana pretiosa", 
             theme = shinytheme("darkly"), #lumen
             tabPanel("Spatial Modeling", fluid = TRUE, icon = icon("frog"),
                      tags$style(button_color_css,background_color),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Frog Habitat"),
                          selectInput(inputId = "HabitatFinder",
                                      label = "Select Habitat:",
                                      choices = unique(oregonfrogs$hab_type),
                                      selected = "Pond",
                                      width = "220px"),
                          helpText("Select a Habitat, then click Search."),
                          hr(),
                          titlePanel("Select a scenario"),
                          fluidRow(column(3,
                                          radioButtons(inputId = "SexFinder",
                                                       label = "Select Sex:",
                                                       choices = c("Female" = "0", "Male" = "1")
                                                       )
                                          ),
                          column(6, offset = 2,
                                 # Select
                                 checkboxGroupInput(inputId = "WaterFinder",
                                                    label = "Select Water:",
                                                    choices = unique(oregonfrogs$water),
                                                    selected = "Deep water")
                                 )
                          ),
                          actionButton(inputId = "Search", label = "Search"),
                        ),
                        mainPanel(
                          leafletOutput("mymap",width = "100%",height = 200),
                          br(),
                          withSpinner(plotOutput(outputId = "SmoothFinder",width = "100%",height = 350))
                        ),
                      )

             ),
             navbarMenu("More", icon = icon("info-circle"),
                        tabPanel(
                          "About", fluid = TRUE,
                          fluidRow(
                            column(6,
                                   #br(),
                                   h4(p("About the App")),
                                   h5(p("Shiny Application and Reproducible Pitch.")),
                                   #br(),
                                   h5(p("This App is intended to facilitate Spatial Model information at a standardized level.")),
                                   #br(),
                                   h5(p("Data for making this app is from the ", a("USG website",href = "https://usg.org/"), "at:", a("Oregon Spotted Frog",href = "https://on.doi.gov/3KTglV7"))),
                                   h5(p("Original source of data is a @USGS study from Pearl, C.A., Rowe, J.C., McCreary,B., and Adams, M.J., 2022. Geological Survey data release: https://doi.org/10.5066/P9DACPCV .")),
                                   br(),
                                   h5(p("Further information: ", a("fede.gazzelloni@gmail.com",href ="fede.gazzelloni@gmail.com"),"."),
                                      p("The source code and the presentation for this Shiny app is available ", a("on github", href = "https://github.com/fgazzelloni/oregonfrogs"), "."),
                                   )

                                   #hr()

                            ),
                            column(6,
                                   h4(p("About the Author")),
                                   h5(p("Federica Gazzelloni is a Statistician and an Actuary. She is the author of the", a("oregonfrogs package", href = "https://github.com/fgazzelloni/oregonfrogs"), "made for modeling with spatial and categorical data in the R environment."),
                                      p("For more info about Federica's work as ", a("Data Scientist", href = 'https://federicagazzelloni.netlify.app'), "."),
                                   ),
                                   #HTML('<img src="~/Documents/R/R_general_resources/Spatials/presentation/shiny/data/fg_image.png", height="200px"'),
                                   br()
                            )
                          )
                        )
             ),
  ),
  print("Federica Gazzelloni"),tags$img(src = "logo.png",height = 30)
)

# Shiny Application for oregonfrogs
# author: FG

# Define server
server <- function(input, output, session) {

  # searching set
  sel_frog_habitat <- reactive({
    req(input$HabitatFinder)
    req(input$SexFinder)
    req(input$WaterFinder)
    filter(oregonfrogs, hab_type %in% input$HabitatFinder) %>%
      filter(sex %in% input$SexFinder) %>%
      filter(water %in% input$WaterFinder) 
  })


  output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        setView(lng=-121.800973 , lat =43.787527, zoom = 13) %>%
        addMarkers(lng=-121.800973 , lat =43.787527,
                   popup="Crane Prairie Reservoir") %>%
        addCircleMarkers(lng=oregonfrogs$long,
                         lat=oregonfrogs$lat,
                         radius = 0.1,color = "#32cd32")
  })

  output$SmoothFinder <- renderPlot({
    input$Search
    input$HabitatFinder
    input$SexFinder
    input$HabitatFinder
    isolate({
      ggplot(data = sel_frog_habitat(),
             aes(x=long,y=lat)) +
        geom_point(
          alpha = 0.5,
          shape = 21,
          stroke = 0.5,
          fill = "#32cd32"
        ) +
        geom_smooth(color = "#cf5f25", alpha = 0.2) +
        geom_smooth(method = "lm",
                    color = "#a8bd3a",
                    alpha = 0.2) +
        labs(x = "Longitude", y = "Latitude") +
        theme_bw()
    })
  })


}


# Run the application
shinyApp(ui = ui, server = server)
