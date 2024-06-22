#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

pacman::p_load(shiny,jsonlite,sf,tidyverse)

### MC2 data processing
mc2_data <- fromJSON("data/MC2/mc2.json")

# Load edges data to mc2_edges
mc2_edges <- as_tibble(mc2_data$links) %>% 
  distinct() 

# Correcting date data type using lubridate package
mc2_edges$time <- as_datetime(mc2_edges$time)
mc2_edges$"_last_edited_date" <- as_datetime(mc2_edges$"_last_edited_date")
mc2_edges$"_date_added" <- as_datetime(mc2_edges$"_date_added")
mc2_edges$date <- as.POSIXct(mc2_edges$date, format = "%Y-%m-%d")

# Updating field names
mc2_edges <- mc2_edges %>%
  rename("last_edited_by" = "_last_edited_by",
         "date_added" = "_date_added",
         "last_edited_date" = "_last_edited_date",
         "raw_source" = "_raw_source",
         "algorithm" = "_algorithm")

# Divide different events into different table
E_TransponderPing <- subset(mc2_edges,  mc2_edges$type == "Event.TransportEvent.TransponderPing")
E_HarborRpt <- subset(mc2_edges,  mc2_edges$type == "Event.HarborReport")
E_Tx <- subset(mc2_edges, mc2_edges$type == "Event.Transaction")

# Load nodes data to mc2_nodes
mc2_nodes <- as_tibble(mc2_data$nodes) %>%
  distinct()

# Correcting date data type using lubridate package
mc2_nodes$"_last_edited_date" <- as_datetime(mc2_nodes$"_last_edited_date")
mc2_nodes$"_date_added" <- as_datetime(mc2_nodes$"_date_added")
mc2_nodes$date <- as.POSIXct(mc2_nodes$date, format = "%Y-%m-%d")

# Updating field names
mc2_nodes <- mc2_nodes %>%
  rename("last_edited_by" = "_last_edited_by",
         "date_added" = "_date_added",
         "last_edited_date" = "_last_edited_date",
         "raw_source" = "_raw_source",
         "algorithm" = "_algorithm")

# Tidy column contents
mc2_nodes <- mc2_nodes %>%
  mutate(Activities = gsub("c[(]", "", Activities)) %>% 
  mutate(Activities = gsub("\"", "", Activities)) %>%
  mutate(Activities = gsub("[)]", "", Activities)) 

mc2_nodes <- mc2_nodes %>%
  mutate(fish_species_present = gsub("c[(]", "", fish_species_present)) %>% 
  mutate(fish_species_present = gsub("\"", "", fish_species_present)) %>%
  mutate(fish_species_present = gsub("[)]", "", fish_species_present)) 

# Divide different nodes into different dataset
N_fish <- subset(mc2_nodes,  mc2_nodes$type == "Entity.Commodity.Fish") %>%
  select_if(~ !any(is.na(.))) %>%
  select(-c(`type`, `raw_source`, `algorithm`, `Activities`, `fish_species_present`)) %>%
  rename(fish_species = name, 
         fish_id = id)

NL_City <- subset(mc2_nodes,  mc2_nodes$type == "Entity.Location.City") %>%
  select_if(~ !any(is.na(.))) %>%
  select(-c(`raw_source`, `algorithm`, `type`, `fish_species_present`)) %>%
  rename(city_name = Name, 
         city_id = id)


NL_Point <- subset(mc2_nodes,  mc2_nodes$type == "Entity.Location.Point") %>%
  select_if(~ !any(is.na(.))) %>%
  select(-c(`raw_source`, `algorithm`, `kind`, `fish_species_present`)) %>%
  rename(point_name = Name, 
         point_id = id)

NL_Region <- subset(mc2_nodes,  mc2_nodes$type == "Entity.Location.Region") %>%
  select_if(~ !any(is.na(.))) %>%
  select(-c(`raw_source`, `algorithm`, `type`, `Description`)) %>%
  rename(region_name = Name, 
         region_id = id, 
         region_kind = kind)

N_Delivery_doc <- subset(mc2_nodes,  mc2_nodes$type == "Entity.Document.DeliveryReport") %>%
  select_if(~ !any(is.na(.))) %>%
  rename(deliver_date = date,
         cargo_id = id) %>%
  select(-c(`algorithm`, `type`, `raw_source`, `Activities`, `fish_species_present`)) 

N_vessel <- mc2_nodes %>%
  filter(grepl("Entity.Vessel", type)) %>%
  mutate(vessel_type = case_when(
    grepl("FishingVessel", type, ignore.case = TRUE) ~ "Fishing",
    grepl("Ferry.Passenger", type, ignore.case = TRUE) ~ "Ferry_Passenger",
    grepl("Ferry.Cargo", type, ignore.case = TRUE) ~ "Ferry_Cargo",
    grepl("Research", type, ignore.case = TRUE) ~ "Research", 
    grepl("Other", type, ignore.case = TRUE) ~ "Other", 
    grepl("Tour", type, ignore.case = TRUE) ~ "Tour", 
    grepl("CargoVessel", type, ignore.case = TRUE) ~ "Cargo_Vessel"
  )) %>%
  select(-c(`algorithm`, `type`, `raw_source`, `Activities`, `fish_species_present`)) %>%
  mutate(company = ifelse(is.na(company), "Unknown", company)) %>% # Handle NA values by replacing NA with unknown
  rename(vessel_id = id, 
         vessel_name = Name,
         vessel_company = company) %>%
  select_if(~ !any(is.na(.)))

# Rename target column as vessel_id
E_TransponderPing <- E_TransponderPing %>%
  rename(vessel_id = target)

# Join data tables to include vessel_type, vessel_company in transponder ping data and filter only Fishing vessel type
E_Tping_Fishing <- E_TransponderPing %>%
  left_join(N_vessel %>% select(vessel_id, vessel_type, vessel_company), by = "vessel_id") %>%
  filter(vessel_type == "Fishing")

### Oceanus Geographical Data Processing
# Import into R
OceanusGeography = st_read("data/MC2/OceanusGeography.geojson") %>%
  st_transform(crs = 4326)

# Save OceanusGeography into rds format for future use
write_rds(OceanusGeography, "data/rds/OceanusGeography.rds")

# Import into R
OceanusLocations <- st_read(dsn = "data/shp",
                            layer = "Oceanus Geography")

# Save OceanusLocations into rds format for future use
write_rds(OceanusLocations, "data/rds/OceanusLocations.rds")

# Create vessel movement data table
vessel_movement_data <- E_TransponderPing %>%
  select(time, dwell, source, vessel_id)

# Tidy source column
vessel_movement_data <- vessel_movement_data%>%
  mutate(source = gsub("^City of", "", source)) %>%
  mutate(source = gsub("^\\s+", "", source))

# Add X, Y coordinates to vessel movement data table
coords <- st_coordinates(OceanusLocations)

OceanusLocations_df <- OceanusLocations %>%
  st_drop_geometry()

OceanusLocations_df$XCOORD <- coords[, "X"]
OceanusLocations_df$YCOORD <- coords[, "Y"]

OceanusLocations_df <- OceanusLocations_df %>%
  select(Name, X.Kind, XCOORD, YCOORD) %>%
  rename(Loc_Type = X.Kind)

vessel_movement_data <- vessel_movement_data %>%
  left_join(OceanusLocations_df,
            by = c("source" = "Name"))

# save data as rds format
write_rds(vessel_movement_data, "data/rds/vessel_movement_data.rds")

vessel_movement_sf <- vessel_movement_data %>%
  st_as_sf(coords = c("XCOORD", "YCOORD"), 
           crs = 4326)

vessel_movement_sf <- vessel_movement_sf %>%
  arrange(vessel_id, time)

vessel_trajectory <- vessel_movement_sf %>%
  group_by(vessel_id) %>%
  summarize(do_union = FALSE) %>%
  st_cast("LINESTRING")

# save data as rds format
write_rds(vessel_trajectory, "data/rds/vessel_trajectory.rds")


# Define UI
ui <- fluidPage(
  titlePanel("Vessel Movement"),
  sidebarLayout(
    sidebarPanel(
      selectInput("vessel_name", "Select Vessel ID:",
                  choices = unique(vessel_trajectory$vessel_id),
                  selected = unique(vessel_trajectory$vessel_id)[1])
    ),
    mainPanel(
      plotOutput("vessel_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  filtered_data <- reactive({
    vessel_trajectory %>%
      filter(vessel_id == input$vessel_name)
  })
  
  output$vessel_plot <- renderPlot({
    ggplot() +
      geom_sf(data = OceanusGeography) +
      geom_sf(data = st_as_sf(filtered_data(), coords = c("XCOORD", "YCOORD"), crs = 4326), 
              aes(color = factor(vessel_id)), 
              size = 1) +
      geom_text(data = OceanusLocations_df, 
                aes(x = XCOORD, y = YCOORD, label = Name), 
                size = 3, hjust = 1, vjust = 1) +
      theme_minimal() +
      labs(title = "Trajectories of Vessels", 
           x = "Longitude", y = "Latitude", color = "ID")
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
