library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(sf)
library(maps)

data <- read.csv(file("../data/cleaned_arm_investments.csv"), stringsAsFactors = FALSE)

filtered_data <- data %>% filter(country != "Other countries")

# Select only the 'country' column and columns ending with '_direct'
direct_columns <- grep("_direct$", names(filtered_data), value = TRUE)

# Calculate the sum of all '_direct' columns grouped by 'country'
result <- filtered_data %>%
  select(country, all_of(direct_columns)) %>%
  group_by(country) %>%
  summarise(Direct_Amount = sum(across(everything(), ~ sum(as.numeric(.), na.rm = TRUE)))) %>%
  filter(Direct_Amount != 0)


# world <- map_data("world")
dataset <- result %>%
  mutate(country_code = countrycode(country, "country.name", "iso3c"))

world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
ui <- fluidPage(
  titlePanel("World Map with FDI"),
  leafletOutput("world_map", height = "600px")
)

server <- function(input, output, session) {
  output$world_map <- renderLeaflet({
    # Merge the dataset with world map data
    world_map_data <- world_map %>%
      left_join(dataset, by = c("iso_a3" = "country_code"))
    
    # Create the leaflet map
    leaflet(world_map_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ifelse(!is.na(Direct_Amount), "blue", "white"),
        fillOpacity = 0.7,
        color = "black",
        weight = 1,
        label = ~ifelse(!is.na(Direct_Amount),
                        paste(name, "Amount: ", Direct_Amount, sep = ""),
                        paste(name, ": No data")),
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", "color" = "black"),
          textsize = "15px",
          direction = "auto"
        )
      )
  })
}

shinyApp(ui, server)

