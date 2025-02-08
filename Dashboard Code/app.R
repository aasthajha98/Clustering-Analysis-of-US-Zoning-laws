library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(RColorBrewer)
library(purrr)
library(DT)  # For interactive tables
library(arules)
library(arulesViz)
library(visNetwork)
library(bslib)



# Load the datasets
clustering_data <- read_csv("Clustering_analysis.csv")
feature_importance <- read_csv("clustering_feature_importance.csv")

# Convert Cluster to ordinal factor for both datasets
clustering_data$Cluster <- factor(clustering_data$Cluster, 
                                  levels = sort(unique(clustering_data$Cluster)),
                                  labels = paste("Cluster", seq_along(unique(clustering_data$Cluster))))

feature_importance$Cluster <- factor(feature_importance$Cluster,
                                     levels = sort(unique(feature_importance$Cluster)),
                                     labels = paste("Cluster", seq_along(unique(feature_importance$Cluster))))

# Create a high-contrast color palette function
create_high_contrast_palette <- function(clusters) {
  high_contrast_colors <- c(
    "#FF0000", "#0000FF", "#00FF00", "#FF00FF", 
    "#00FFFF", "#FFA500", "#800080", "#008000",
    "#000080", "#FFFF00"
  )
  
  palette_colors <- high_contrast_colors[1:min(length(clusters), length(high_contrast_colors))]
  colorFactor(palette = palette_colors, domain = clusters)
}

muni_data <- read_csv("nzlud_muni.csv")
binary <- muni_data[muni_data$zri > 1 & muni_data$type == "City", c(3, 5:10, 12:35, 38:38)]
basket <- na.omit(binary[, -1])
transactions <- as(as.matrix(basket), "transactions")

rules <- apriori(transactions, 
                 parameter = list(
                   supp = 0.2,   # Support threshold
                   conf = 0.7,   # Confidence threshold
                   minlen = 3,   # Minimum rule length
                   maxlen = 7   # Maximum rule length
                 )
)



# Remove redundant rules
rules <- rules[!is.redundant(rules)]

# Sort rules by lift
rules_sorted <- sort(rules, by = "lift")

subrules <- rules_sorted #head(rules_sorted, n = 20, by = "lift")


# UI for the Shiny app
ui <- 
  navbarPage(
    title = "Zoning Analysis Dashboard",
    theme = bs_theme(
      version = 5,  # Bootstrap version
      bootswatch = "flatly",  # A professional, clean theme
      primary = "#2c3e50",    # Dark blue-gray for primary color
      secondary = "#34495e"   # Slightly lighter blue-gray for secondary
    ),
  
  tabPanel( 
    title = "Overview",
    div(
      style = "padding: 15px;",
      h3("Dashboard Overview"),
      p("Welcome to the Zoning Analysis Dashboard. The following results are the product of a clustering analysis and association analysis 
      conducted on a compilation of zoning data, housing and rental data, and population and density data."), 
      p("This tool is meant to serve as a data exploration tool to researchers, students, city planners, and anyone else interested in teasing out patterns between cost of housing and zoning restrictions."),
      p("The clustering analysis is a K Means Clustering analysis that creates 8 clusters of American cities. 
        The association analysis uses a minimum support threshold of 0.2 and confidence threshold of 0.7. The minimum length of rules is 3 and the maximum length of rules is 7."),
      h4("Data Overview"),
      tags$ul(
        tags$li(HTML("<b>Zoning Data:</b> Zoning data is from the National Zoning and Land Use Database, a repository posted to GitHub by the Eviction Lab – an organization promoting research, journalism, and advocacy surrounding eviction and housing. The database includes information on lot size requirement, parking requirements, height restrictions and other key elements of zoning codes for 2,600 municipalities across the US. Many of the indicator fields are also combined into index variables to summarize the data such as their own Zoning Restrictiveness Index.")),
        
        tags$li(HTML("<b>Housing and Rental Data:</b> The data on housing mortgages and rent prices has been collected from the 2022 American Community Survey 5-year estimates from the United States Census Bureau using the census.data.gov tool. This data has been aggregated at the city level and was matched to the zoning data using Census GEOID codes for each of the cities.")), 
        
        tags$li(HTML("<b>Population and Density Data:</b> This data comes from the website called simplemaps.com's United States Cities Database. This is a free database online that included population and density related data for cities in the United States collected from the Census Bureau."))
        
        
      ),
        
      h3(HTML("<b>Tabs Overview:</b>")),
      tags$ul(
        tags$li(HTML("<b>Tab 1: Interactive Map with Clusters</b>: The first tab shows the results of the K Means Clustering analysis and allows for interaction between the clusters and other features.")),
        tags$li(HTML("<b>Tab 2: Feature Importance by Cluster</b>: This tab allows for exploration of the top most important features of each cluster and allows the user to draw conclusions for what each feature represents.")),
        tags$li(HTML("<b>Tab 3: Interactive Map for Data Exploration</b>: This tab is for those skeptical of my clustering analysis who wish for a clean slate to explore the data set and how different features vary amongst each other.")),
        tags$li(HTML("<b>Tab 4: Association Rules Analysis</b>: This tab serves to show the association between various zoning law indicies and how often they appear compared to one another.")),
        tags$li(HTML("<b>Tab 5: Association Analysis Legend</b>: This tab serves as a legend to the association analysis and provides deeper meaning on what each feature means."))
      ),
      h4(HTML("<b>Unintuitive Indices Explanation:</b>")),
      h5(HTML("<b>Explicit growth controls index (EGCI)</b> is the sum of the following indicators ")),
      tags$ul(
        tags$li(HTML("has annual restrictions on single-family permits it issues")),
        tags$li(HTML("has annual restrictions on multi-family permits it issues")),
        tags$li(HTML("has annual restrictions on single-family units it permits")),
        tags$li(HTML("has annual restrictions on multi-family units it permits")),
        tags$li(HTML("has annual restrictions on multi-family dwellings it permits")),
        tags$li(HTML("has annual restrictions on multi-family dwelling units it permits"))
      ),
      
      h5(HTML("<b>Open space requirements index</b>")),
      tags$ul(
        tags$li(HTML("Indicator for whether municipality requires developers to dedicate open space (or pay an in-lieu fee)"))
      ),
      h5(HTML("<b>Minimum lot size index (MLSI):</b> mutually exclusive coding to indicate the maximum minimum lot size across the municipality.")),
      tags$ul(
        tags$li(HTML("1 = half acre or less")),
        tags$li(HTML("2 = greater or equal to half-acre, less than 1 acre")),
        tags$li(HTML("3 = greater than or equal to one acre, less than two acres")),
        tags$li(HTML("4 = two or more acres"))
      ),
      h5(HTML("<b>Maximum permitted density index (MPDI):</b> mutually exclusive coding to indicate the highest maximum permitted density across the municipality.")),
      tags$ul(
        tags$li(HTML("1 = 31 or more units per acre")),
        tags$li(HTML("2 =  15-30 units per acre")),
        tags$li(HTML("3 = 8-14 units per acre")),
        tags$li(HTML("4 =  5-7 units per acre")),
        tags$li(HTML("5 = 0-4 units per acre"))
      ),
      h5(HTML("<b>Inclusionary zoning programmes index (IZPI):</b>")),
      tags$ul(
        tags$li(HTML("Indicator for whether municipality operates an inclusionary zoning program"))
      ),
      h5(HTML("<b>Zoning Restrictiveness Index (ZRI):</b>")),
      tags$ul(
        tags$li(HTML("Combines all land use policies into a single measure of exclusionary zoning"))
      )
    )
  ),
  # Map Tab
  tabPanel("Interactive Map with Clusters",
           tags$head(
             tags$style(HTML("
        .tooltip-custom { 
          background-color: rgba(255,255,255,0.9);
          border: 1px solid #ccc;
          padding: 10px;
          border-radius: 5px;
        }
        #map {
          height: 700px !important;
          width: 100% !important;
        }
        .sidebar {
          height: 700px;
          overflow-y: auto;
        }
        .cluster-table {
          margin: 20px;
          padding: 15px;
          border-radius: 5px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .dataTables_wrapper {
          padding: 15px;
          background: #fff;
          border-radius: 5px;
          margin-bottom: 20px;
        }
      "))
           ),
           fluidRow(
             column(12,
                    h3("Data Exploration Map with Clusters", 
                       style = "text-align: center; margin: 20px 0;"),
             )
           ),
           # Instruction Box
           fluidRow(
             column(
               width = 12, # Full width for the instructions
               div(
                 style = "border: 1px solid #ccc; padding: 15px; margin-top: 20px; background-color: #f9f9f9;",
                 h3("Instructions"),
                 p("This map shows the results of a K Means clustering analysis on data of cities including zoning characteristics, mortgage values, median rent, and population/density characteristics."),
                 p("Select a feature for bubble size in order to initiate visualization. "),
                 p("Use the additional features that change bubble size and add features to the tool tip in order to see how various characteristics vary across clusters."),
                 p("You can type in the name of a city into the search bar to view characteristics of a single city."),
                 h3("Summary"),
                 p("A preliminary overview of the clusters can shows us that Cluster 7 has cities with the most resticitve zoning laws. Cluster 6 shows the cities with the largest populations. Cluster 8 shows use the lowest cost of living cities."),
                 p("For more information on the most important features of each cluster please proceed to the next tab labeled Feature Importance by Cluster")
                 
               )
             )
           ),
           
           fluidRow(
             column(3, 
                    wellPanel(
                      class = "sidebar",
                      textInput(
                        inputId = "city_search",
                        label = "Search City",
                        placeholder = "Enter city name"
                      ),
                      
                      checkboxGroupInput(
                        inputId = "clusters",
                        label = "Select Clusters to Display:",
                        choices = levels(clustering_data$Cluster),
                        selected = levels(clustering_data$Cluster)
                      ),
                      
                      selectInput(
                        inputId = "size_feature",
                        label = "Select Feature for Bubble Size:",
                        choices = names(clustering_data)[
                          !names(clustering_data) %in% 
                            c("City", "lat", "lng", "Cluster", "State")
                        ],
                        selected = NULL
                      ),
                      
                      selectInput(
                        inputId = "tooltip_features",
                        label = "Select Additional Features for Tooltip:",
                        choices = names(clustering_data)[
                          !names(clustering_data) %in% 
                            c("City", "lat", "lng", "Cluster", "State")
                        ],
                        selected = NULL,
                        multiple = TRUE
                      )
                    )
             ),
             column(9, 
                    leafletOutput("map")
             )
           ),
           

  ),
  
  # Feature Importance Tab
  tabPanel("Cluster Deep Dive",
           fluidRow(
             column(12,
                    h3("Feature Importance by Cluster", 
                       style = "text-align: center; margin: 20px 0;"),
                    p("The following shows the most important features in each cluster.", style = "text-align: center; margin: 20px 0;"),
                    p("All figures are standardized", style = "text-align: center; margin: 20px 0;")
             )
           ),
           
           fluidRow(
             lapply(levels(feature_importance$Cluster), function(cluster) {
               column(3,  # 4 tables per row
                      div(class = "cluster-table",
                          h4(cluster, 
                             style = "text-align: center; color: #2c3e50;"),
                          DTOutput(paste0("table_", gsub(" ", "_", cluster)))
                      )
               )
             })
           ),
  ),
  
  
  tabPanel("Interactive Map for Data Exploration",
           tags$head(
             tags$style(HTML("
        .tooltip-custom { 
          background-color: rgba(255,255,255,0.9);
          border: 1px solid #ccc;
          padding: 10px;
          border-radius: 5px;
        }
        #map {
          height: 700px !important;
          width: 100% !important;
        }
        .sidebar {
          height: 700px;
          overflow-y: auto;
        }
        .cluster-table {
          margin: 20px;
          padding: 15px;
          border-radius: 5px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .dataTables_wrapper {
          padding: 15px;
          background: #fff;
          border-radius: 5px;
          margin-bottom: 20px;
        }
      "))
           ),
           fluidRow(
             column(12,
                    h3("Data Exploration Map without clusters ", 
                       style = "text-align: center; margin: 20px 0;"),
             )
           ),
           # Instruction Box
           fluidRow(
             column(
               width = 12, # Full width for the instructions
               div(
                 style = "border: 1px solid #ccc; padding: 15px; margin-top: 20px; background-color: #f9f9f9;",
                 h3("Instructions"),
                 p("Use the following tool to see how various zoning characteristics, mortgage and rent pricing, and density/population characterisitcs vary across cities without the clustering analysis."),
                 p("Choose features that change bubble size and bubble colors to view different feature distributions."), 
                 p("You can add features to the tool tip in order to see how various characteristics vary across cities."),
                 p("You can type in the name of a city into the search bar to view characteristics of a single city.")
               )
             )
           ),
           
           fluidRow(
             column(3,
                    wellPanel(
                      class = "sidebar",
                      textInput(
                        inputId = "city_search_2",
                        label = "Search City",
                        placeholder = "Enter city name"
                      ),
                      
                      selectInput(
                        inputId = "size_feature_2",
                        label = "Select Feature for Bubble Size:",
                        choices = names(clustering_data)[
                          !names(clustering_data) %in%
                            c("City", "lat", "lng", "Cluster", "State")
                        ],
                        selected = "Zoning Restrictiveness Index"  # Default size feature
                      ),
                      
                      selectInput(
                        inputId = "color_feature_2",
                        label = "Select Feature for Color Distribution:",
                        choices = names(clustering_data)[
                          !names(clustering_data) %in%
                            c("City", "lat", "lng", "Cluster", "State")
                        ],
                        selected = "Zoning Restrictiveness Index"  # Default color feature
                      ),
                      
                      selectInput(
                        inputId = "tooltip_features_2",
                        label = "Select Additional Features for Tooltip:",
                        choices = names(clustering_data)[
                          !names(clustering_data) %in%
                            c("City", "lat", "lng", "Cluster", "State")
                        ],
                        selected = NULL,
                        multiple = TRUE
                      )
                    )
             ),
             column(9,
                    leafletOutput("map2", height = "700px")
             )
           )
           
  ),
  
  
  # New Association Analysis Tab
  
  
  
  tabPanel("Association Rules Analysis",
           # Instruction Box
           fluidRow(
             column(
               width = 12, # Full width for the instructions
               div(
                 style = "border: 1px solid #ccc; padding: 15px; margin-top: 20px; background-color: #f9f9f9;",
                 h3("Instructions"),
                 p("Please allow a moment for visualization to load."),
                 p("Use the drop down menu to select association rules that you are interested in seeing the connections of further."),
                 p("For further details on what each zoning rule entails view the next tab containing more information."),
                 h3("Summary"),
                 p("Nodes represent association rules and edges represent their connections."),
                 p("This Association Analysis helps uncovers patterns in how municipalities structure their land use and development approval processes. Some insights to look out for are:"),
                 tags$ul(
                   tags$li("Which land use restrictions tend to occur together"),
                   tags$li("How different approval requirements are correlated"),
                   tags$li("How various permit and density restrictions might be linked")
                 ),
                 p("Connections with a lift greater than 1.3 are highlighted in red."),
                 p("We can see that there is an association between municipalities that allow accessory dwelling units that also operate inclusionary zoning programs.")
               )
             )
           ),
           
           fluidRow(
             column(9,
                    tabsetPanel(
                      tabPanel('Rule Network', visNetworkOutput("rule_network", width = "100%", height = "600px")),
                      tabPanel("Rule Table", DTOutput("rules_table"))
                    )
             )
           )
           

  ),
  tabPanel(
    title = "Association Analysis Legend",
    div(
      style = "padding: 15px;",
      h3("Association Analysis Legend"),
      p("Below are explanations for what each zoning indiciator used in the association annalysis standas for."),
      h4("Feature Explanations :"),
      tags$ul(
        tags$li(HTML("<b>restrict_sf_permit</b>: Indicator for whether municipality has annual restrictions on single-family permits it issues")),
        tags$li(HTML("<b>restrict_mf_permit</b>: Indicator for whether municipality has annual restrictions on multi-family permits it issues")),
        tags$li(HTML("<b>limit_sf_units</b>: Indicator for whether municipality has annual restrictions on single-family units it permits")),
        tags$li(HTML("<b>limit_mf_units</b>: Indicator for whether municipality has annual restrictions on multi-family units it permits")),
        tags$li(HTML("<b>limit_mf_dwellings</b>: Indicator for whether municipality has annual restrictions on multi-family dwellings it permits")),
        tags$li(HTML("<b>limit_mf_dwelling_units</b>: Indicator for whether municipality has annual restrictions on multi-family dwelling units it permits")),
        tags$li(HTML("<b>open_space</b>: Indicator for whether municipality requires developers to dedicate open space (or pay an in-lieu fee)")),
        tags$li(HTML("<b>inclusionary</b>: Indicator for whether municipality operates an inclusionary zoning program")),
        tags$li(HTML("<b>half_acre_less</b>: Indicator for whether municipality has minimum lot size of less than a half acre for any residential district")),
        tags$li(HTML("<b>half_acre_more</b>: Indicator for whether municipality has a minimum lot size of a half acre or more for any residential district")),
        tags$li(HTML("<b>one_acre_more</b>: Indicator for whether municipality has a minimum lot size of one acre or more for any residential district")),
        tags$li(HTML("<b>two_acre_more</b>: Indicator for whether municipality has a minimum lot size of two acres or more for any residential district")),
        tags$li(HTML("<b>max_den_cat1</b>: Indicator for whether municipality has a maximum permitted density restriction of 0-4 units for any residential district")),
        tags$li(HTML("<b>max_den_cat2</b>: Indicator for whether municipality has a maximum permitted density restriction of 5-7 units for any residential district")),
        tags$li(HTML("<b>max_den_cat3</b>: Indicator for whether municipality has a maximum permitted density restriction of 8-14 units for any residential district")),
        tags$li(HTML("<b>max_den_cat4</b>: Indicator for whether municipality has a maximum permitted density restriction of 15-30 units for any residential district")),
        tags$li(HTML("<b>max_den_cat5</b>: Indicator for whether municipality has a maximum permitted density restriction of 31 or more units for any residential district")),
        tags$li(HTML("<b>council_nz</b>: Indicator for whether municipality requires its council to approve a development in the absence of rezoning")),
        tags$li(HTML("<b>planning_nz</b>: Indicator for whether municipality requires its planning board to approve a development in the absence of rezoning")),
        tags$li(HTML("<b>countybrd_nz</b>: Indicator for whether municipality requires county board approval for a development in the absence of rezoning")),
        tags$li(HTML("<b>pubhlth_nz</b>: Indicator for whether municipality requires its public health board to approve a development in the absence of rezoning")),
        tags$li(HTML("<b>site_plan_nz</b>: Indicator for whether municipality requires its design review board to approve a development in the absence of rezoning")),
        tags$li(HTML("<b>env_rev_nz</b>: Indicator for whether municipality requires its environmental review board to approve a development in the absence of rezoning")),
        tags$li(HTML("<b>council_rz</b>: Indicator for whether municipality requires its council to approve a development in the case of a rezoning")),
        tags$li(HTML("<b>planning_rz</b>: Indicator for whether municipality requires its planning board to approve a development in the case of a rezoning")),
        tags$li(HTML("<b>zoning_rz</b>: Indicator for whether municipality requires its zoning board to approve a development in the case of a rezoning")),
        tags$li(HTML("<b>countybrd_rz</b>: Indicator for whether municipality requires county board approval for a development in the case of a rezoning")),
        tags$li(HTML("<b>countyzone_rz</b>: Indicator for whether municipality requires county zoning board approval for a development in the case of a rezoning")),
        tags$li(HTML("<b>townmeet_rz</b>: Indicator for whether municipality requires town meeting approval for a development in the case of a rezoning")),
        tags$li(HTML("<b>env_rev_rz</b>: Indicator for whether municipality requires its environmental review board to approve a development in the case of a rezoning")),
        tags$li(HTML("<b>adu</b>: Indicator for whether municipality allows accessory dwelling units anywhere in its jurisdiction"))      )
    )
  )
  
  
)

# Server logic
server <- function(input, output, session) {
  
  ############## Tab 1
  # Map Tab Logic
  filtered_data <- reactive({
    data <- clustering_data %>%
      filter(Cluster %in% input$clusters)
    
    if (!is.null(input$city_search) && input$city_search != "") {
      data <- data %>%
        filter(grepl(input$city_search, City, ignore.case = TRUE))
    }
    
    data
  })
  
  color_palette <- reactive({
    clusters <- levels(clustering_data$Cluster)
    create_high_contrast_palette(clusters)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = mean(clustering_data$lng), 
              lat = mean(clustering_data$lat), 
              zoom = 5)
  })
  
  observeEvent(input$reset_view, {
    leafletProxy("map") %>%
      setView(lng = mean(clustering_data$lng), 
              lat = mean(clustering_data$lat), 
              zoom = 5)
  })
  
  observe({
    req(input$clusters)
    
    data <- filtered_data()
    palette <- color_palette()
    
    data <- data %>%
      rowwise() %>%
      mutate(
        Tooltip = paste0(
          "<div class='tooltip-custom'>",
          "<b>City:</b> ", City, "<br>",
          "<b>State:</b> ", State, "<br>",
          "<b>Cluster:</b> ", as.character(Cluster), "<br>",
          paste(
            purrr::map_chr(input$tooltip_features, ~ paste0("<b>", ., ":</b> ", as.character(get(.)))),
            collapse = "<br>"
          ),
          "</div>"
        )
      ) %>%
      ungroup()
    
    if (!is.null(input$size_feature) && input$size_feature != "") {
      data <- data %>%
        mutate(bubble_size = scales::rescale(
          .data[[input$size_feature]], 
          to = c(3, 15)
        ))
    } else {
      data <- data %>%
        mutate(bubble_size = 5)
    }
    
    if (nrow(data) == 0) {
      leafletProxy("map") %>%
        clearMarkers() %>%
        clearControls()
      return()
    }
    
    leafletProxy("map", data = data) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        radius = ~bubble_size,
        color = ~palette(Cluster),
        fillColor = ~palette(Cluster),
        fillOpacity = 0.7,
        weight = 1,
        label = ~lapply(Tooltip, HTML),
        popup = ~Tooltip
      ) %>%
      addLegend("bottomright", 
                pal = palette, 
                values = ~Cluster, 
                title = "Clusters",
                opacity = 1,
                layerId = "cluster_legend")
  })
  
  ############# Tab 2
  
  # Feature Importance Tab Logic
  # Create a table for each cluster
  lapply(levels(feature_importance$Cluster), function(cluster) {
    output[[paste0("table_", gsub(" ", "_", cluster))]] <- renderDT({
      feature_importance %>%
        filter(Cluster == cluster) %>%
        select(Feature, Importance) %>%
        arrange(desc(Importance)) %>%
        datatable(
          options = list(
            pageLength = 5,
            lengthMenu = c(5, 10, 15),
            searching = FALSE,
            dom = 't<"bottom"p>',
            scrollY = "200px",
            scrollCollapse = TRUE
          ),
          rownames = FALSE
        ) %>%
        formatRound("Importance", digits = 3)
    })
  })
  
  
  
  # ############# Tab3
  # # Reactive function for filtered data
  filtered_data_map2 <- reactive({
    
    if (!is.null(input$city_search_2) && input$city_search_2 != "") {
      clustering_data <- clustering_data %>%
        filter(grepl(input$city_search_2, City, ignore.case = TRUE))
    }
    clustering_data
  })
  
  
  # Render Leaflet Map with initial data
  output$map2 <- renderLeaflet({
    # Determine default features (first non-standard columns)
    default_size_feature <- names(clustering_data)[
      !names(clustering_data) %in% c("City", "lat", "lng", "Cluster", "State")
    ][1]
    
    default_color_feature <- names(clustering_data)[
      !names(clustering_data) %in% c("City", "lat", "lng", "Cluster", "State")
    ][2]
    
    # Prepare data with bubble sizes and tooltips
    clustering_data_with_tooltips <- clustering_data %>%
      mutate(
        bubble_size = scales::rescale(
          .data[[default_size_feature]], 
          to = c(3, 15)  # Min and max bubble radius
        )
      ) %>%
      rowwise() %>%
      mutate(
        Tooltip = paste0(
          "<div class='tooltip-custom'>",
          "<b>City:</b> ", City, "<br>",
          "<b>State:</b> ", State, "<br>",
          "<b>Cluster:</b> ", as.character(Cluster), "<br>",
          "<b>", default_size_feature, ":</b> ", 
          round(.data[[default_size_feature]], 2), "<br>",
          "<b>", default_color_feature, ":</b> ", 
          round(.data[[default_color_feature]], 2),
          "</div>"
        )
      ) %>%
      ungroup()
    
    # Create initial palette based on default color feature
    initial_palette <- colorNumeric(
      palette = "BuPu", 
      domain = clustering_data_with_tooltips[[default_color_feature]]
    )
    
    # Initial map with bubbles
    leaflet(clustering_data_with_tooltips) %>%
      addTiles() %>%
      setView(lng = mean(clustering_data$lng), lat = mean(clustering_data$lat), zoom = 4) %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        color = ~initial_palette(get(default_color_feature)),
        radius = ~bubble_size,
        label = ~lapply(Tooltip, HTML),
        popup = ~Tooltip,
        opacity = 1,
        fillOpacity = 0.8
      ) %>%
      addLegend(
        "bottomleft",
        pal = initial_palette,
        values = ~get(default_color_feature),
        title = paste("Legend for", default_color_feature),
        opacity = 1
      ) 
  })
  
  # Update Map with Bubbles observe block
  observe({
    req(input$color_feature_2, input$size_feature_2)
    data2 <- filtered_data_map2()
    palette_2 <- colorNumeric(palette = "BuPu", domain = data2[[input$color_feature_2]])
    
    # Enhanced bubble sizing with more intuitive scaling
    data2 <- data2 %>%
      # Add bubble size calculation
      mutate(
        bubble_size = scales::rescale(
          .data[[input$size_feature_2]], 
          to = c(3, 15)  # Min and max bubble radius
        )
      ) %>%
      rowwise() %>%
      mutate(
        Tooltip = paste0(
          "<div class='tooltip-custom'>",
          "<b>City:</b> ", City, "<br>",
          "<b>State:</b> ", State, "<br>",
          "<b>Cluster:</b> ", as.character(Cluster), "<br>",
          "<b>", input$size_feature_2, ":</b> ", 
          round(.data[[input$size_feature_2]], 2), "<br>",
          paste(
            purrr::map_chr(input$tooltip_features_2, 
                           ~ paste0("<b>", ., ":</b> ", as.character(get(.)))),
            collapse = "<br>"
          ),
          "</div>"
        )
      ) %>%
      ungroup()
    
    leafletProxy("map2", data = data2) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        color = ~palette_2(get(input$color_feature_2)),
        radius = ~bubble_size,  # Use the pre-calculated bubble_size
        label = ~lapply(Tooltip, HTML),
        popup = ~Tooltip,
        opacity = 1,
        fillOpacity = 0.8
      ) %>%
      addLegend(
        "bottomleft",
        pal = palette_2,
        values = ~get(input$color_feature_2),
        title = paste("Legend for", input$color_feature_2),
        opacity = 1
      )
  })
  ###########################################
  # Association Rules Analysis
  
  # Rules Table
  output$rules_table <- renderDT({
    
    # Convert rules to a readable dataframe
    rules_df <- data.frame(
      Antecedents = labels(lhs(rules_sorted)),
      Consequents = labels(rhs(rules_sorted)),
      Support = quality(rules_sorted)$support,
      Confidence = quality(rules_sorted)$confidence,
      Lift = quality(rules_sorted)$lift
    )
    
    datatable(rules_df, 
              options = list(
                pageLength = 10,
                scrollX = TRUE
              )) %>%
      formatRound(c('Support', 'Confidence', 'Lift'), digits = 3)
  })
  
  
  output$rule_network <- renderVisNetwork({
    # Convert rules to a network format
    # Create nodes from unique items in the rules
    all_items <- unique(c(
      unlist(labels(lhs(subrules))), 
      unlist(labels(rhs(subrules)))
    ))
    
    # Create node dataframe
    nodes <- data.frame(
      id = seq_along(all_items),
      label = all_items,
      title = all_items  # Tooltip text
    )
    
    # Create edges dataframe with color based on lift
    edges_data <- data.frame(
      from = match(unlist(labels(lhs(subrules))), nodes$label),
      to = match(unlist(labels(rhs(subrules))), nodes$label),
      width = quality(subrules)$lift,  # Use lift for edge width
      color = ifelse(quality(subrules)$lift > 1.2, 
                     "#FF6347",   # Tomato red for high-lift rules
                     "#1E90FF"),  # Dodger blue for standard rules
      title = paste("Lift:", round(quality(subrules)$lift, 2),
                    "Support:", round(quality(subrules)$support, 2),
                    "Confidence:", round(quality(subrules)$confidence, 2))
    )
    
    # Create interactive network
    visNetwork(nodes, edges_data) %>%
      visOptions(
        highlightNearest = list(
          enabled = TRUE,
          degree = 1,
          hover = TRUE
        ),
        nodesIdSelection = TRUE
      ) %>%
      visPhysics(solver = "barnesHut",  # A stable solver
                 barnesHut = list(
                   gravitationalConstant = -2000,  # Strong repulsion
                   centralGravity = 0.1,  # Minimal central gravity
                   springLength = 200,    # Increased spring length
                   springConstant = 0.01, # Very low spring constant
                   damping = 0.9          # High damping to reduce movement
                 ),
                 stabilization = TRUE) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLayout(randomSeed = 123)
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
