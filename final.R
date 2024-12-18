#US MAP

# Load required libraries
library(sf)
library(ggplot2)

# Read the shapefile
shapefile_path <- "C:/Users/velas/Documents/STA9750-2024-FALL/cb_2018_us_state_500k.shp"
us_states <- st_read(shapefile_path)

# Filter out Hawaii and Alaska
# Assuming the shapefile has a column called 'NAME' with state names
mainland_states <- us_states[!us_states$NAME %in% c("Hawaii", "Alaska","Puerto Rico","United States Virgin Islands","Commonwealth of the Northern Mariana Islands","Guam","American Samoa"), ]



library(readxl)
library(dplyr)
####Miles
file_path <- "C:/Users/velas/Documents/STA9750-2024-FALL/2016cityandcountymiles.xlsx"

# Read the sheet
miles <- read_excel(path = file_path, sheet = "Sheet1")

miles_clean <- miles %>%
  select(state_abbr, population, `vehicle miles traveled (miles)`) %>% # Select specific columns
  group_by(state_abbr) %>%
  summarise(
    total_population = sum(population, na.rm = TRUE),
    total_miles = sum(`vehicle miles traveled (miles)`, na.rm = TRUE)
  ) %>%
  mutate(miles_per_capita = total_miles / total_population)

miles_map <-miles_clean %>%
  select(state_abbr,miles_per_capita)

miles_t10 <-miles_clean %>%
  arrange(desc(miles_per_capita))

miles_b10 <-miles_clean %>%
  arrange(miles_per_capita)

print(miles_t10)
print(miles_b10)


# Perform the join while ensuring geometry is preserved
miles_map_usa <- mainland_states %>%
  inner_join(miles_map, by = c("STUSPS" = "state_abbr"))


# Plot the mainland United States and color by miles_per_capita
ggplot(data = miles_map_usa) +
  geom_sf(aes(fill = miles_per_capita), color = "black") +  # Map fill to miles_per_capita
  scale_fill_viridis_c(option = "plasma", name = "Miles per Capita") +  # Use a color gradient
  theme_minimal() +
  labs(
    title = "Miles per Capita by State"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.position = "right"  # Place legend on the right
  )



####power generation
file_path_power <- "C:/Users/velas/Documents/STA9750-2024-FALL/EIA_powergeneration.xlsx"

# Read the sheet
power <- read_excel(path = file_path_power, sheet = "Sheet1")

print(power)

# Summarize the dataset by Year, State Code, and Renewable columns
power_year <- power %>%
  group_by(Year, Renewable) %>%
  summarize(Total_Nameplate_Capacity = sum(`Nameplate Capacity (Megawatts)`, na.rm = TRUE)) %>%
  ungroup()


### Energy production Graphs - Annual
library(ggplot2)

# Create the plot
ggplot(data = power_year, aes(x = Year, y = Total_Nameplate_Capacity, color = as.factor(Renewable), group = Renewable)) +
  geom_line(size = 1) +                           # Draw lines for each group
  geom_point(size = 2) +                          # Add points for better visibility
  scale_color_manual(values = c("red", "green"),  # Customize colors for renewable 0 and 1
                     labels = c("Non-Renewable", "Renewable")) +
  labs(title = "Annual Megawatts Produced in the United States",
       x = "Year",
       y = "Megawatts",
       color = "Energy Type") +
  theme_minimal() +                              
  theme(plot.title = element_text(hjust = 0.5)) # Center the title




### interactive map - renewables per state
library(plotly)

power_2023 <- power %>%
  filter(Year == 2023) %>%
  group_by(Year, `State Code`, Renewable) %>%
  summarize(Total_Nameplate_Capacity = sum(`Nameplate Capacity (Megawatts)`, na.rm = TRUE)) %>%
  ungroup()

print(power_2023)

# Summarize data by State Code and Renewable
state_summary <- power_2023 %>%
  mutate(Renewable = ifelse(Renewable == 1, "Renewable", "Non-Renewable")) %>%
  group_by(`State Code`, Renewable) %>%
  summarize(Total_Nameplate_Capacity = sum(Total_Nameplate_Capacity, na.rm = TRUE)) %>%
  ungroup()

# Create the interactive pie chart
pie_chart <- state_summary %>%
  plot_ly(
    labels = ~ifelse(Renewable == 1, "Renewable", "Non-Renewable"),
    values = ~Total_Nameplate_Capacity,
    type = "pie",
    textinfo = "label+percent",
    insidetextorientation = "radial"
  ) %>%
  layout(
    title = list(text = "Energy Production by Type"),
    updatemenus = list(
      list(
        type = "dropdown",
        active = 0,
        buttons = lapply(unique(state_summary$`State Code`), function(state) {
          list(
            label = state,
            method = "restyle",
            args = list(
              list(
                labels = list(state_summary$Renewable[state_summary$`State Code` == state]),
                values = list(state_summary$Total_Nameplate_Capacity[state_summary$`State Code` == state])
              )
            )
          )
        })
      )
    )
  )
# Step 4: Display the pie chart
pie_chart


### Renewable percentage
library(dplyr)

# Calculate the percentage of renewable energy per state
power_2023_percent <- power_2023 %>%
  group_by(`State Code`) %>%
  mutate(
    Total_State_Capacity = sum(Total_Nameplate_Capacity),
    Renewable_Percentage = ifelse(Renewable == 1, (Total_Nameplate_Capacity / Total_State_Capacity) * 100, 0)
  ) %>%
  ungroup()

power_2023_percent <- power_2023_percent %>%
  filter(Renewable == 1) %>%
  select(`State Code`,`Renewable_Percentage`)

# View the updated dataset
print(power_2023_percent)
print(mainland_states)

power_2023_percent_map <- mainland_states %>%
  left_join(power_2023_percent, by = c("STUSPS" = "State Code"))

print(power_2023_percent_map)

##USA MAP RENEWABLE ENERGY
library(ggplot2)

# Create a choropleth map using Renewable_Percentage as the fill
ggplot(data = power_2023_percent_map) +
  geom_sf(aes(fill = Renewable_Percentage), color = "black", lwd = 0.2) + # Fill by Renewable_Percentage
  scale_fill_viridis_c(option = "plasma", name = "Renewable (%)") +      # Use a color scale for percentages
  labs(
    title = "Renewable Energy Percentage by State (2023)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Center and style title
    plot.caption = element_text(size = 10)             # Style caption
  )



### Air quality
# Replace 'path_to_your_file' with the actual path to your CSV file
air_quality_data <- read.csv("C:/Users/velas/Documents/STA9750-2024-FALL/air_quality_data_years.csv", stringsAsFactors = FALSE)

air_quality<- air_quality_data %>%
  filter(Year ==2020) %>%
  group_by(State) %>%
  summarize(Average_Value = mean(Value, na.rm = TRUE)) %>%
  ungroup()

print(air_quality)

air_quality_map <- mainland_states %>%
  left_join(air_quality, by = c("NAME" = "State"))

print(air_quality_map)

library(ggplot2)

# Create the map using Value as the fill
ggplot(data = air_quality_map) +
  geom_sf(aes(fill = Average_Value), color = "black", lwd = 0.2) +  # Use Value for fill
  scale_fill_viridis_c(option = "plasma", name = "Air Quality Index (µg/m³)") +  # Add color scale for fill
  labs(
    title = "Air Quality Values by State",
    caption = "Source: https://ephtracking.cdc.gov/DataExplorer/"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Center and style the title
    plot.caption = element_text(size = 10)             # Style the caption
  )



###Energy Pricess
### Air quality

####Miles
energy_price_path <- "C:/Users/velas/Documents/STA9750-2024-FALL/elect_sales_revenue.xlsx"

# Read the sheet
energy_price <- read_excel(path = energy_price_path, sheet = "State-YTD-States")

energy_price_usa <- energy_price %>%
  filter(State == 'US') %>%
  filter(Year !=2024)
print(energy_price_usa)

energy_price_state<- energy_price %>%
  filter(Year == '2023')


##Energy prices rising. 
# Create a line plot with points and trend line
ggplot(energy_price_usa, aes(x = Year, y = `Cents/kWh`, group = 1)) +
  geom_line(color = "blue", size = 1) +  # Add trend line
  geom_point(color = "red", size = 2) + # Add points
  labs(
    title = "Electricity Prices Over Time",
    x = "Year",
    y = "Cents per kWh"

  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

##USA MAP ELEC PRICE
energy_price_state_map <- mainland_states %>%
  left_join(energy_price_state, by = c("STUSPS" = "State"))
print(energy_price_state_map)


library(ggplot2)

# Create the map using Cents/KWh as the fill the USA map
ggplot(data = energy_price_state_map) +
  geom_sf(aes(fill = `Cents/kWh`), color = "black", lwd = 0.2) +  # Use Cents/kWh for fill
  scale_fill_viridis_c(option = "plasma", name = "Cents per kWh") +  # Add a color gradient
  labs(
    title = "Electricity Prices by State (2023)",
    caption = "Source: https://www.eia.gov/electricity/data/eia861m/"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Center and style the title
    plot.caption = element_text(size = 10)             # Style the caption
  )




#Scoring for formulas 
state2abr <-mainland_states %>%
  st_drop_geometry() %>%
  select(NAME,STUSPS)
#energy price
energy_score <- energy_price_state %>%
  select(State, `Cents/kWh`) %>%
  mutate(price_score = `Cents/kWh` / max(`Cents/kWh`, na.rm = TRUE)) %>%
  select(-`Cents/kWh`)

#avg miles driven
miles_score <- miles_map %>%
  mutate(miles_score = miles_per_capita / max(miles_per_capita, na.rm = TRUE)) %>%
  select(-miles_per_capita)

#air quality score
air_score<- air_quality %>%
  mutate(air_quality_score = Average_Value / max(Average_Value, na.rm = TRUE)) %>%
  select(-Average_Value)

formula <- state2abr %>%
  left_join(energy_score, by = c("STUSPS" = "State"))%>%
  left_join(miles_score, by = c("STUSPS" = "state_abbr"))%>%
  left_join(air_score, by = c("NAME" = "State")) %>%
  mutate(score = (price_score*33)+(miles_score*34)+(air_quality_score*33))

print(formula)

formula_t5 <- formula %>%
  select(NAME, STUSPS,score) %>%
  arrange(desc(score)) %>%  
  head(5)              

formula_b5 <- formula %>%
  select(NAME, STUSPS,score) %>%
  arrange(score) %>%  
  head(5) 

#formula map usa
formula1 <- formula%>%
  select(-NAME,-price_score,-miles_score,-air_quality_score)
  
formula_map <- mainland_states %>%
  left_join(formula1, by = c("STUSPS" = "STUSPS"))


library(ggplot2)

# Plot the map with fill based on the score column
ggplot(data = formula_map) +
  geom_sf(aes(fill = score), color = "black") +  # Fill color based on score and black border
  scale_fill_viridis_c(option = "plasma", name = "Score") +  # Use a color gradient
  theme_minimal() +
  labs(
    title = "Beneficiary Scores by State"

  ) +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.position = "right"  # Place the legend on the right
  )


