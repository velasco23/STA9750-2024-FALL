# Load required libraries
library(dplyr)
library(plotly)
library(htmlwidgets)
library(readxl)

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
    labels = ~Renewable,
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

# Save the plot as a standalone HTML file
saveWidget(as_widget(pie_chart), "pie_chart.html", selfcontained = TRUE)

# Render the saved HTML file in Quarto