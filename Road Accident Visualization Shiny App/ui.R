###############################################################################################
# UI Section of the Application
###############################################################################################

# Uniques State names
states.names <- unique(as.character(state_group$state_name))

# Converting state codes to factors
states_dataset$state_code <- as.factor(states_dataset$state_code)

# Finding unique state codes
states.codes <- unique(states_dataset$state_code)
road <- read.csv(file = "data/injured1.csv")

# INput Choices for Prediction Tab
input_choices <- c("Defective.Steering", "Punctured.burst.Tyres", "Bald.Tyres", "Other.serious.mechanical.defect", "Metalled.Roads", "Pucca.road..Normal.Road.", "Kutcha.Roads", "Loose.Surface", "Road.under.repair.construction", "Corrugated.Wavy.road", "Snowy", "Muddy", "Slight.Curve", "Flat.Road", "Gentle.Incline", "Pot.Holes", "Speed.Breaker", "Steep.Incline", "Sharp.Curve", "Others.road.conditions")

# Options Names to display values on Map
var <- c(
  "Persons Killed in Road Accidents: 2014" = "p10",
  "Persons Killed in Road Accidents: 2015" = "p11",
  "Persons Killed in Road Accidents: 2016" = "p12",
  "Persons Killed in Road Accidents: 2017" = "p13",
  "Persons Injured in Road Accidents: 2014" = "p14",
  "Persons Injured in Road Accidents: 2015" = "p15",
  "Persons Injured in Road Accidents: 2016" = "p16",
  "Persons Injured in Road Accidents: 2017" = "p17"
  
)

list_select <- c(
  "Number of Persons Killed in Road Accidents during - 2014" = 1,
  "Number of Persons Killed in Road Accidents during - 2015" = 2,
  "Number of Persons Killed in Road Accidents during - 2016" = 3,
  "Number of Persons Killed in Road Accidents during - 2017" = 4,
  "Number of Persons Injured in Road Accidents during - 2014" = 5,
  "Number of Persons Injured in Road Accidents during - 2015" = 6,
  "Number of Persons Injured in Road Accidents during - 2016" = 7,
  "Number of Persons Injured in Road Accidents during - 2017" = 8
  
)

map.view.options.names <-
  c(
    "Number of Persons Killed in Road Accidents during - 2014",
    "Number of Persons Killed in Road Accidents during - 2015",
    "Number of Persons Killed in Road Accidents during - 2016",
    "Number of Persons Killed in Road Accidents during - 2017",
    "Number of Persons Injured in Road Accidents during - 2014",
    "Number of Persons Injured in Road Accidents during - 2015",
    "Number of Persons Injured in Road Accidents during - 2016",
    "Number of Persons Injured in Road Accidents during - 2017",
    "Road Accidents due to Weather Condition: Fine/Clear ",
    "Road Accidents due to Weather Condition: Mist/Foggy ",
    "Road Accidents due to Weather Condition: Cloudy ",
    "Road Accidents due to Weather Condition: Rainy ",
    "Road Accidents due to Weather Condition: Flooding ",
    "Road Accidents due to Weather Condition: Hail/Sleet ",
    "Road Accidents due to Weather Condition: Snow ",
    "Road Accidents due to Weather Condition: Dust Storm ",
    "Road Accidents due to Weather Condition: Other Extreme Weather Conditions ",
    "Road Accidents due to Road Condition: Surfaced ",
    "Road Accidents due to Road Condition: Metalled ",
    "Road Accidents due to Road Condition: Normal / Pucca ",
    "Road Accidents due to Road Condition: Kutcha ",
    "Road Accidents due to Road Condition: Dry ",
    "Road Accidents due to Road Condition: Wet ",
    "Road Accidents due to Road Condition: Good Surface ",
    "Road Accidents due to Road Condition: Loose Surface ",
    "Road Accidents due to Road Condition: Under Construction/Repairing ",
    "Road Accidents due to Road Condition: Corrugated ",
    "Road Accidents due to Road Condition: Slippery ",
    "Road Accidents due to Road Condition: Snowy ",
    "Road Accidents due to Road Condition: Muddy ",
    "Road Accidents due to Road Condition: Oily ",
    "Road Accidents due to Road Condition: Straight ",
    "Road Accidents due to Road Condition: SlightCurve ",
    "Road Accidents due to Road Condition: Flat ",
    "Road Accidents due to Road Condition: Gentle Incline ",
    "Road Accidents due to Road Condition: Humps ",
    "Road Accidents due to Road Condition: Dip ",
    "Road Accidents due to Road Condition: Pot Holes ",
    "Road Accidents due to Road Condition: Speed breaker ",
    "Road Accidents due to Road Condition: Steep Incline ",
    "Road Accidents due to Road Condition: Sharp Curve ",
    "Road Accidents due to Road Condition: Earthern Shoulder Edge Drop ",
    "Road Accidents due to Road Condition: Others ",
    "Road Accidents due to Vehicle Condition: Defective Brakes ",
    "Road Accidents due to Vehicle Condition: Defective Steering ",
    "Road Accidents due to Vehicle Condition: Defective Punctured/Burst Tyres ",
    "Road Accidents due to Vehicle Condition: Defective Bald Tyres ",
    "Road Accidents due to Vehicle Condition: Defective Worn Out tyres ",
    "Road Accidents due to Vehicle Condition: Other Mechanical Defects "
  )

# Options Values to display values on Map
map.view.options.values <-
  c(
    
    "persons_killed_2014",
    "persons_killed_2015",
    "persons_killed_2016",
    "persons_killed_2017",
    "persons_injured_2014",
    "persons_injured_2015",
    "persons_injured_2016",
    "persons_injured_2017",
    "weather_normal",
    "weather_mist_fog",
    "weather_cloudy",
    "weather_rain",
    "weather_flooding",
    "weather_hail_sleet",
    "weather_snow",
    "weather_dust_storm",
    "weather_other_extreme_conditions",
    "road_surfaced_road_acc",
    "road_metalled_road_acc",
    "road_normalpucca_road_acc",
    "road_Kutcha_road_acc",
    "road_dry_road_acc",
    "road_wet_road_acc",
    "road_goodsurface_road_acc",
    "road_loosesurface_road_acc",
    "road_under_repair_road_acc",
    "road_corrugated_road_acc",
    "road_slippery_road_acc",
    "road_snowy_road_acc",
    "road_muddy_road_acc",
    "road_oily_road_acc",
    "road_straight_road_acc",
    "road_slightcurve_road_acc",
    "road_flat_road_acc",
    "road_gentleincline_road_acc",
    "road_hump_road_acc",
    "road_dip_road_acc",
    "road_pothole_road_acc",
    "road_speedbreaker_road_acc",
    "road_steepincline_road_acc",
    "road_sharpcurve_road_acc",
    "road_earthernshoulderedgedrop_road_acc",
    "road_other_road_acc",
    "vehicle_defect_brakes",
    "vehicle_defect_steering",
    "vehicle_defect_puncturedbursttyres",
    "vehicle_defect_baldtyres",
    "vehicle_defect_wornouttyres",
    "vehicle_defect_othermechanical"
    
  )

# Options Icons to display values on Map
map.view.options.icons <- c(
  "icon1.png",
  "icon2.png",
  "icon3.png",
  "icon4.png",
  "icon5.png",
  "icon6.png",
  "icon7.png",
  "icon8.png",
  "icon9.png",
  "icon10.png",
  "icon11.png",
  "icon12.png"
)

# Radio Button names to switch between Map and Choreopleth
top.states.names <- c("All States View")

# Radio Button values to switch between Map and Choreopleth
top.states.values <- c("all_view")

# Radio Button icons to switch between Map and Choreopleth
top.states.icons <- c("icon14.png")


navbarPage(
  "Road Accident Analysis and Visualization of India",
  id = "nav",
  theme = shinytheme("flatly"),
  
  # Tab 1- Interactive Map
  tabPanel(
    "Interactive Spatial map",
    div(
      class = "outer",
      
      tags$head(# Include the custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")),
      
      # Leaflet Library Map
      leafletOutput("tab1_leaflet_map", width = "100%", height = "100%"),
      
      # Right Filter Panel for drop-down and the graph
      absolutePanel(
        id = "controls",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = TRUE,
        top = 80,
        left = "auto",
        right = 20,
        bottom = "auto",
        width = 490,
        height = 870,
        
        # Dropdown Header
        h2("Choose State"),
        
        # Initializing the dropdown
        selectInput(
          "tab1_dropdown_states",
          NULL,
          c(
            "ALL STATES" = "All",
            structure(
              by_state_order$state_code,
              names = as.character(by_state_order$state_name)
            )
          )
        ),
        
        # Initializing all the value boxes
        valueBoxOutput("tab1_valuebox_persons_killed_2014", width = 6),
        valueBoxOutput("tab1_valuebox_persons_killed_2015", width = 6),
        valueBoxOutput("tab1_valuebox_persons_killed_2016", width = 6),
        valueBoxOutput("tab1_valuebox_persons_killed_2017", width = 6),
        
        # Initializing the polar chart/ spider chart
        
        highchartOutput("tab1_polar_plot", height = 400)

      )
    )
  ),
  # Tab 2 - MAP
  tabPanel(title="Injured/Killed Distribution", sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "state",label="",choices = c("State"=1)),p(),
      selectInput("selectMap", label ="Select Statistic", 
                  choices = var, 
                  selected = 1),p()),
    mainPanel(leafletOutput("mymap",height = 700))
  )),
# Tab 3- Data Table 
tabPanel(
  "Dataset",
  box(
    title = "Road Accident Dataset",
    width = 12,
    status = "primary",
    height = "850",
    solidHeader = T,
    DT::dataTableOutput("state_group", height = 800)
  )
),
# Tab 4- Different type of plots
tabPanel(
  "Plots",
  tags$head(tags$style(HTML(
    "
    div#checkGroup {
    font-size: 75%;
    }
    "
  ))),
  sidebarLayout(
    
    # Left Sidbar State Filter Panel
    sidebarPanel(
      h4("Select States"),
      checkboxInput('all.none', 'All/None', value = F),
      checkboxGroupInput(
        "state.checkbox.filter",
        label = NULL,
        choiceNames = as.character(by_state_order$state_name),
        choiceValues = by_state_order$state_code,
        selected = c(35, 28, 22, 34, 3, 8, 19, 21, 20)
      )
      ,
      width = 2
    ),
    
    mainPanel(box(
      tabsetPanel(
        
        tabPanel(
          "Bar Plots: Total Accidents",
          fluidRow(
            column(
              6,
              box(
                title = "Frequency Plot of Number of Persons injured in 2014",
                status = "primary",
                solidHeader = TRUE,
                highchartOutput("tab4_column_chart1", height = "320px")
                ,
                width = 12
              )
            ),
            column(
              6,
              box(
                title = "Frequency Plot of Number of Persons injured in 2015",
                status = "primary",
                solidHeader = TRUE,
                highchartOutput("tab4_column_chart2", height = "320px"),
                width = 12
              )
            )),fluidRow(
              column(
                6,
                box(
                  title = "Frequency Plot of Number of Persons injured in 2016",
                  status = "primary",
                  solidHeader = TRUE,
                  highchartOutput("tab4_column_chart3", height = "320px")
                  ,
                  width = 12
                )
              ),
              column(
                6,
                box(
                  title = "Frequency Plot of Number of Persons injured in 2017",
                  status = "primary",
                  solidHeader = TRUE,
                  highchartOutput("tab4_column_chart4", height = "320px"),
                  width = 12
                )
              )),fluidRow(
                column(
                  6,
                  box(
                    title = "Frequency Plot of Number of Persons Killed in 2014",
                    status = "primary",
                    solidHeader = TRUE,
                    highchartOutput("tab4_column_chart5", height = "320px")
                    ,
                    width = 12
                  )
                ),
                column(
                  6,
                  box(
                    title = "Frequency Plot of Number of Persons Killed in 2015",
                    status = "primary",
                    solidHeader = TRUE,
                    highchartOutput("tab4_column_chart6", height = "320px"),
                    width = 12
                  )
                )),fluidRow(
                  column(
                    6,
                    box(
                      title = "Frequency Plot of Number of Persons Killed in 2016",
                      status = "primary",
                      solidHeader = TRUE,
                      highchartOutput("tab4_column_chart7", height = "320px")
                      ,
                      width = 12
                    )
                  ),
                  column(
                    6,
                    box(
                      title = "Frequency Plot of Number of Persons Killed in 2017",
                      status = "primary",
                      solidHeader = TRUE,
                      highchartOutput("tab4_column_chart8", height = "320px"),
                      width = 12
                    )
                  ))
        ),
        tabPanel(
          "Bar Plots: Weather Condition",
          fluidRow(column(
            6,
            box(
              title = " Weather Condition: Fine/Clear",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart21", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Weather Condition: Mist/Foggy",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart22", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Weather Condition: Cloudy",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart23", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Weather Condition: Rainy",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart24", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Weather Condition: Flooding",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart25", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Weather Condition: Hail/Sleet",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart26", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Weather Condition: Snow",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart27", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Weather Condition: Dust Storm",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart28", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Weather Condition: Other Extreme Weather Conditions",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart29", height = "320px")
              ,
              width = 12
            )
          ))
        ),
        
        tabPanel(
          "Bar Plots: Road Condition",
          fluidRow(column(
            6,
            box(
              title = "Road Condition: Surfaced",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart31", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Road Condition: Metalled",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart32", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Road Condition: Normal / Pucca",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart33", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Road Condition: Kutcha",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart34", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Road Condition: Dry",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart35", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Road Condition: Wet",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart36", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Road Condition: Good Surface",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart37", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Road Condition: Loose Surface",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart38", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Road Condition: Under Construction/Repairing",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart39", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Road Condition: Corrugated",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart40", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Road Condition: Slippery",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart41", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Road Condition: Snowy",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart42", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Road Condition: Muddy",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart43", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Road Condition: Oily",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart44", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Road Condition: Straight",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart45", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Road Condition: Slight Curve",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart46", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Road Condition: Flat",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart47", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Road Condition: Gentle Incline",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart48", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Road Condition: Humps",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart49", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Road Condition: Dip",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart50", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Road Condition: Pot Holes",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart51", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Road Condition: Speed breaker",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart52", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Road Condition: Steep Incline",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart53", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Road Condition: Sharp Curve",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart54", height = "320px"),
              width = 12
            )
          )
        ),
        fluidRow(column(
          6,
          box(
            title = "Road Condition: Earthern Shoulder Edge Drop",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("tab4_column_chart55", height = "320px")
            ,
            width = 12
            )
          ),
          column(
            6,
            box(
              title = "Road Condition: Others",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart56", height = "320px"),
              width = 12
            )
          ))
      ),
      
      tabPanel(
        "Bar Plots: Vehicle Condition",
        fluidRow(column(
          6,
          box(
            title = "Vehicle Condition: Defective Brakes",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("tab4_column_chart61", height = "320px")
            ,
            width = 12
          )
        ),
        column(
          6,
          box(
            title = "Vehicle Condition: Defective Steering",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("tab4_column_chart62", height = "320px"),
            width = 12
          )
        )),
        fluidRow(column(
          6,
          box(
            title = "Vehicle Condition: Defective Punctured/Burst Tyres",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("tab4_column_chart63", height = "320px")
            ,
            width = 12
          )
        ),
        column(
          6,
          box(
            title = "Vehicle Condition: Defective Bald Tyres",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("tab4_column_chart64", height = "320px"),
            width = 12
          )
        )),
        fluidRow(column(
          6,
          box(
            title = "Vehicle Condition: Defective Worn Out tyres",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("tab4_column_chart65", height = "320px")
            ,
            width = 12
          )
        ),
        column(
          6,
          box(
            title = "Vehicle Condition: Other Mechanical Defects",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("tab4_column_chart66", height = "320px"),
            width = 12
          )
        ))
      ),
        
        
        
        tabPanel(
          "Heat Map",
          fluidRow(column(
            6,
            box(
              title = "Number of Persons Killed in 2014",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_heatmap_chart1", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Number of Persons Killed in 2015",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_heatmap_chart2", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Number of Persons Killed in 2016",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_heatmap_chart3", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Number of Persons Killed in 2017",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_heatmap_chart4", height = "320px"),
              width = 12
            )
          ))
          ,
          fluidRow(column(
            6,
            box(
              title = "Number of Persons injured in 2014",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_heatmap_chart5", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Number of Persons injured in 2015",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_heatmap_chart6", height = "320px"),
              width = 12
            )
          )),
          
          fluidRow(column(
            6,
            box(
              title = "Number of Persons injured in 2016",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_heatmap_chart7", height = 320)
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Number of Persons injured in 2017",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_heatmap_chart8", height = 320),
              width = 12
            )
          ))
        ),
      
      
      
        tabPanel(
          "Line Graphs",
          fluidRow(column(
            6,
            box(
              title = "Persons injured 2014 vs Persons injured 2015 vs Persons injured 2016 vs Persons injured 2017",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_line_chart1", height = "320px")
              ,
              width = 12
            )
          ),column(
            6,
            box(
              title = "Persons Killed 2014 vs Persons Killed 2015 vs Persons Killed 2016 vs Persons Killed 2017",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_line_chart2", height = "320px")
              ,
              width = 12
            )
           )
          ),
          fluidRow(column(
            6,
            box(
              title = "Weather : Fine/Normal vs Mist/Foggy vs Cloudy vs Rainy",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_line_chart3", height = "320px")
              ,
              width = 12
            )
          ),column(
            6,
            box(
              title = "Weather : Hail/Sleet vs Snow vs Dust Storm vs Flooding vs Others",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_line_chart4", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Road : Surfaced vs Metalled vs Normal/Pucca vs Kutcha vs Dry",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_line_chart5", height = "320px")
              ,
              width = 12
            )
          ),column(
            6,
            box(
              title = "Road : Wet vs Good Surface vs Loose Surface vs Under Construction vs Corrugated",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_line_chart6", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Road : Slippery vs Snowy vs Muddy vs Oily vs Straight",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_line_chart7", height = "320px")
              ,
              width = 12
            )
          ),column(
            6,
            box(
              title = "Road : Slight Curve vs Flat vs Gentle Incline vs Humps vs Dip",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_line_chart8", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Road : Pot Holes vs Speed breaker vs Steep Incline vs Sharp Curve vs Earthern vs Others",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_line_chart9", height = "320px")
              ,
              width = 12
            )
          ),column(
            6,
            box(
              title = "Vehicle Condition: Defective Brakes vs Steering vs Punctured/Burst Tyres",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_line_chart10", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Vehicle Condition: Defective Bald Tyres vs Worn Out Tyres vs Other Mechanical Defects",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_line_chart11", height = "320px")
              ,
              width = 12
            )
          )
          )
        )
        
      ),
      width = 12
    ), width = 10)
  )
),
##Tab 5
tabPanel(
  "Conditons Causing Accidents",
  dashboardPage(
    dashboardHeader(disable = T),
    dashboardSidebar(disable = T),
    dashboardBody(
      tags$script(
        'window.onload = function() {
          function fixBodyHeight() {
          var el = $(document.getElementsByClassName("content-wrapper")[0]);
          var h = el.height();
          el.css("min-height", h + 50 + "px");
          };
          window.addEventListener("resize", fixBodyHeight);
          fixBodyHeight();
          };'
      ),
      fluidRow(
        box(
          # Radio button to switch between two Map Types
          radioButtons(
            "map.type.filter",
            "",
            choiceNames = mapply(
              top.states.names,
              top.states.icons,
              FUN = function(state, iconUrl) {
                tagList(tags$img(
                  src = iconUrl,
                  width = 35,
                  height = 35
                ),
                state)
              },
              SIMPLIFY = FALSE,
              USE.NAMES = FALSE
            ),
            choiceValues = top.states.values
          ),
          width = 2
        ),
        
        column(width = 1),
        
        # Initializing the Highcharter Map
        column(highchartOutput("tab2_bubble_map", height = "800px"),
               width = 6),
        
        # Right Panel different attribute filters
        box(
          radioButtons(
            "attribute.filters",
            "",
            choiceNames = mapply(
              map.view.options.names,
              map.view.options.icons,
              FUN = function(country, flagUrl) {
                tagList(tags$img(
                  src = flagUrl,
                  width = 0,
                  height = 0
                ),
                country)
              },
              SIMPLIFY = FALSE,
              USE.NAMES = FALSE
            ),
            choiceValues = map.view.options.values
          ),
          width = 3
        )
      )
    )
  )
),

####################################
#        TAB 6 PREDICTION          #
####################################

tabPanel(
  "Prediction",
  tags$head(tags$style(HTML(
    "
    div#checkGroup {
    font-size: 75%;
    }
    "
  ))),
  sidebarLayout(
    # Left Sidbar State Filter Panel
    sidebarPanel(
      h4("Select States"),
      selectInput(
        "tab5_dropdown_states",
        NULL,
        c(
          structure(
            by_state_order$state_code,
            names = as.character(by_state_order$state_name)
          )
        )
      ),
      checkboxGroupInput(
        "choice.checkbox.filter",
        label = NULL,
        choiceNames = input_choices,
        choiceValues = input_choices
      )
      ,
      width = 3
    ),
    mainPanel(box(
      tabsetPanel(
        tabPanel(
          fluidRow(column(
            8,
            box(
              title = "Predicted risk:",
              
              solidHeader = TRUE,
              width = 12
            )
          ),
          column(
            9,offset=2, 
            box(
              status = "primary",
              solidHeader = FALSE,
              span(textOutput("prediction"), style="font-size:30px;color:blue"),
              width = 30
            )
          )
          )
        )
      )
    ))
  )
),

 conditionalPanel("false", icon("crosshair"))
)


