library(httr)
library(sf)
library(utils)

# url and string for file
base_url <- "https://cdmaps.polisci.ucla.edu/shp/"
file_names <- sprintf("districts%03d.zip", 1:114)  # Generates file names from districts001.zip to districts114.zip

# temp directory
download_dir <- "congressional_shapefiles"
if (!dir.exists(download_dir)) dir.create(download_dir)

# download & unzip 
download_and_load_shapefile <- function(file_name) {
  local_zip_path <- file.path(download_dir, file_name)
  url <- paste0(base_url, file_name)
  
  # checking if already downloaded
  if (!file.exists(local_zip_path)) {
    cat("Downloading", file_name, "...\n")
    GET(url, write_disk(local_zip_path, overwrite = TRUE))
  } else {
    cat(file_name, "already exists, skipping download.\n")
  }
  
  # Unzip and load the shapefile
  unzip_dir <- file.path(download_dir, sub(".zip$", "", file_name))  # Unzipped folder
  if (!dir.exists(unzip_dir)) {
    unzip(local_zip_path, exdir = unzip_dir)
  }
  
  # loadthe .shp file
  district_shapes_dir <- file.path(unzip_dir, "districtShapes")
  shp_file <- list.files(district_shapes_dir, pattern = "\\.shp$", full.names = TRUE)
  
  if (length(shp_file) > 0) {
    shapefile_data <- st_read(shp_file)
    return(shapefile_data)
  } else {
    message("No shapefile found in 'districtShapes' for", file_name)
    return(NULL)
  }
}

# loop through and see if downloaded is needed
shapefiles_list <- list()
for (file_name in file_names) {
  shapefile_data <- download_and_load_shapefile(file_name)
  if (!is.null(shapefile_data)) {
    shapefiles_list[[file_name]] <- shapefile_data
  }
}


 

  
  
  library(httr)
  library(sf)
  library(utils)
  
  # url and string for file
  base_url <- "https://www2.census.gov/geo/tiger/TIGER2024/CD/"
  file_names <- sprintf("tl_2024_%02d_cd119.zip", 1:78)  
  
  # temp directpry
  download_dir <- "census_shapefiles"
  if (!dir.exists(download_dir)) dir.create(download_dir)
  
  # Unzip and load the shapefile
  download_and_load_shapefile <- function(file_name) {
    local_zip_path <- file.path(download_dir, file_name)
    url <- paste0(base_url, file_name)
    
    # Check if file already exists to avoid re-downloading
    if (!file.exists(local_zip_path)) {
      cat("Downloading", file_name, "...\n")
      GET(url, write_disk(local_zip_path, overwrite = TRUE))
    } else {
      cat(file_name, "already exists, skipping download.\n")
    }
    
    # Unzip and load the shapefile
    unzip_dir <- file.path(download_dir, sub(".zip$", "", file_name))  # Unzipped folder
    if (!dir.exists(unzip_dir)) {
      unzip(local_zip_path, exdir = unzip_dir)
    }
    
    # Load the .shp file from the unzipped directory
    shp_file <- list.files(unzip_dir, pattern = "\\.shp$", full.names = TRUE)
    
    if (length(shp_file) > 0) {
      census_data <- st_read(shp_file)
      return(census_data)
    } else {
      message("No shapefile found in unzipped directory for", file_name)
      return(NULL)
    }
  }
  
  # Loop through each file name, download, and load the shapefile if necessary
  census_shapefiles <- list()
  for (file_name in file_names) {
    census_data <- download_and_load_shapefile(file_name)
    if (!is.null(census_data)) {
      census_shapefiles[[file_name]] <- census_data
    }
  }
  
  
#datasets  
glimpse(shapefiles_list[[11]]) #congressional Boundaries files 1976 to 2022
glimpse(census_shapefiles[[2]]) # congressional Boundaries files 2014 to present

# Read the CSV files
house_of_reps <- read.csv("C:/Users/velas/OneDrive/Documents/STA9750-2024-FALL/1976-2022-house.csv")
presidents <- read.csv("C:/Users/velas/OneDrive/Documents/STA9750-2024-FALL/1976-2020-president.csv")
glimpse(house_of_reps)
glimpse(presidents)

#Task 3
#Q1

library(dplyr)
library(ggplot2)
library(tidyverse)
# Filter data for 1976 and 2022, count House seats per state for each year, and calculate the difference
house_count_by_state <- house_of_reps |>
  filter(year == 1976 | year == 2022) |>
  group_by(year, state) |>
  summarise(number_of_reps = n_distinct(district), .groups = "drop") |>
  pivot_wider(names_from = year, values_from = number_of_reps, names_prefix = "year_") |>
  mutate(difference = year_2022 - year_1976)

# Plot the difference graph q1
ggplot(house_count_by_state, aes(x = reorder(state, difference), y = difference)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Difference in Number of House Representatives per State (1976 vs 2022)",
    x = "State",
    y = "Difference in Number of Representatives"
  ) +
  theme_minimal()

#Q2
library(dplyr)


# Filter for New York State & general elections only
ny_elections <- house_of_reps |>
  filter(state == "NEW YORK" & stage == "GEN")
print(ny_elections)
# fusion voting results
fusion_results <- ny_elections |>
  group_by(year, district, candidate) |>
  summarise(
    total_fusion_votes = sum(candidatevotes), 
    .groups = "drop"
  )
print(fusion_results)
# Popular voting for major parties results
major_party_votes <- ny_elections |>
  filter(party %in% c("DEMOCRAT", "REPUBLICAN")) |>
  group_by(year, district, candidate) |>
  summarise(
    major_party_votes = sum(candidatevotes),  
    party = first(party),
    .groups = "drop"
  )

# Find the winners under both systems
# 1. Fusion method
fusion_winner <- fusion_results |>
  group_by(year, district) |>
  slice_max(total_fusion_votes, n = 1, with_ties = FALSE) |>
  select(year, district, fusion_winner = candidate, fusion_votes = total_fusion_votes)

# 2. Major party method
major_party_winner <- major_party_votes |>
  group_by(year, district) |>
  slice_max(major_party_votes, n = 1, with_ties = FALSE) |>
  select(year, district, major_winner = candidate, major_party_votes)

# comparing results from two methods mentioned above
comparison <- fusion_winner |>
  inner_join(major_party_winner, by = c("year", "district")) |>
  filter(fusion_winner != major_winner) |>
  select(year, district, fusion_winner, fusion_votes, major_winner, major_party_votes)


print(comparison)
nrow(comparison)
# comparison table
library(knitr)
library(DT)

kable(comparison, caption = "Comparison of Fusion Voting vs Major Party Voting Results")

# Display the comparison table as an interactive datatable
datatable(
  comparison,
  caption = "Comparison of Fusion Voting vs Major Party Voting Results",
  options = list(pageLength = 10, autoWidth = TRUE),
  rownames = FALSE
)



#question 3

library(dplyr)
library(ggplot2)
library(tidyr)


# Summarize houose of reps by state, year, and party
house_of_reps_q3 <- house_of_reps %>%
  group_by(year, state, party) %>%
  summarise(total_congressional_votes = sum(totalvotes), .groups = "drop")


# Summarize presidential votes by state, year, and party
presidents_q3 <- presidents %>%
  group_by(year, state, party_simplified) %>%
  summarise(presidential_votes = sum(totalvotes), .groups = "drop") %>%
  rename(party = party_simplified)

# Merge presidential & house of reps data
comparison_data <- presidents_q3 %>%
  inner_join(house_of_reps_q3, by = c("year", "state", "party"))

# measure difference
comparison_data <- comparison_data %>%
  mutate(vote_difference = presidential_votes - total_congressional_votes)

print(comparison_data)
print(comparison_long)

# Assuming `house_of_reps` and `presidents` datasets are already summarized and loaded as in the previous step

# Combine presidential results and HOR results
comparison_long <- presidents_q3 %>%
  rename(total_presidential_votes = presidential_votes) %>%
  inner_join(house_of_reps_q3, by = c("year", "state", "party")) %>%
  pivot_longer(
    cols = c(total_presidential_votes, total_congressional_votes),
    names_to = "vote_type",
    values_to = "votes"
  )

# bar graph without trend line
ggplot(comparison_long, aes(x = factor(year), y = votes, fill = vote_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ party) +
  labs(
    title = "Presidential vs Congressional Votes by Year and Party",
    x = "Year",
    y = "Total Votes",
    fill = "Vote Type"
  ) +
  scale_fill_manual(values = c("total_presidential_votes" = "steelblue", "total_congressional_votes" = "seagreen1")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(dplyr)
library(ggplot2)

# Assuming `comparison_data` has been created as in previous steps

library(dplyr)
library(ggplot2)
library(tidyr)

# Assuming `comparison_long` has been created as in previous steps

# bar graph with trend line
ggplot(comparison_long, aes(x = factor(year), y = votes, fill = vote_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_smooth(aes(group = vote_type, color = vote_type), method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~ party) +
  labs(
    title = "Presidential vs Congressional Votes by Year and Party with Trend Lines",
    x = "Year",
    y = "Total Votes",
    fill = "Vote Type",
    color = "Trend Line"
  ) +
  scale_fill_manual(values = c("total_presidential_votes" = "steelblue", "total_congressional_votes" = "seagreen1")) +
  scale_color_manual(values = c("total_presidential_votes" = "blue", "total_congressional_votes" = "seagreen")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#task 4
library(sf)

# Define the URL and the correct filename for the ZIP file
url <- "https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=Shapefile"
zip_file <- "Borough Boundaries.zip"  # Use the correct name

# Download the ZIP file if it doesn't already exist
if (!file.exists(zip_file)) {
  download.file(url, destfile = zip_file, mode = "wb")
}

# Create a temporary directory and unzip the file
temp_dir <- tempdir()
unzip(zip_file, exdir = temp_dir)

# Find the .shp file within the extracted contents
shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)

# Check if the .shp file was found, then load it into R
if (length(shp_file) > 0) {
  nyc_sf <- read_sf(shp_file)
  print(nyc_sf)
} else {
  stop("The shapefile (.shp) was not found in the extracted contents. Please check the ZIP file.")
}
#outline nyc map
ggplot(nyc_sf, 
       aes(geometry=geometry)) + 
  geom_sf()

#nyc map color
ggplot(nyc_sf, 
       aes(geometry=geometry, 
           fill = shape_area)) + 
  geom_sf()



ggplot(census_shapefiles[[5]], 
       aes(geometry=geometry)) + 
  geom_sf()




library(sf)
library(dplyr)
library(ggplot2)
#2000 results for president
presidents_2000 <- presidents %>%
  filter(year == 2000) %>%
  group_by(state) %>%
  slice_max(candidatevotes, n = 1, with_ties = FALSE) %>%
  select(state, candidate, party_simplified,candidatevotes)

#electral college number per state
house_of_reps_2000 <- house_of_reps |>
  filter(year == 2000) |>
  group_by(state) |>
  summarize(district_count = n_distinct(district))
print(house_of_reps_2000)

#combine elec college & presidents
combined_data_2000 <- house_of_reps_2000 %>%
  inner_join(presidents_2000, by = "state")  %>%
  mutate(elec_college = district_count+2)





#Create look up table for state and STATEFP code (we will join by state later) to SHP files
state_lookup <- data.frame(
  STATEFP = c("01", "02", "04", "05", "06", "08", "09", "10", "12", "13", "15", "16", "17", "18", "19", "20", 
              "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", 
              "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", 
              "55", "56"),
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
                 "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", 
                 "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
                 "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                 "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
                 "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
                 "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  state_abbr = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", 
                 "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", 
                 "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", 
                 "WI", "WY")
)

# Combine all shapefiles 
all_states <- bind_rows(census_shapefiles[c(1:56)])
all_states <- all_states %>%
  left_join(state_lookup, by = "STATEFP")


all_states_outline <- all_states %>%
  group_by(STATEFP) %>%
  summarize(geometry = st_union(geometry)) 

all_states_outline <- all_states_outline %>%
  left_join(state_lookup, by = "STATEFP") 

all_states_outline <- all_states_outline %>%
  mutate(state = str_to_upper(state))

location_and_election <- all_states_outline %>%
  full_join(combined_data_2000, by = "state") %>%
  drop_na()


library(sf)
library(dplyr)
library(ggplot2)
#graph all of USA
ggplot(data = location_and_election) +
  geom_sf(aes(fill = party_simplified), color = "black") +  # Color by winner (Bush or Gore)
  scale_fill_manual(values = c("REPUBLICAN" = "red", "DEMOCRAT" = "blue"), name = "Winner") +
  geom_text(aes(label = elec_college, geometry = geometry), size = 3, stat = "sf_coordinates") +
  labs(title = "2000 U.S. Presidential Election Results by State") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Main land graph
library(sf)
library(dplyr)
library(ggplot2)
#exclude hawaii & alasks
common_crs <- st_crs(4326)
contiguous_us <- location_and_election %>%
  filter(!state %in% c("ALASKA", "HAWAII")) %>%
  st_transform(common_crs)
glimpse(contiguous_us)
# Main land graph
ggplot(data = contiguous_us) +
  geom_sf(aes(fill = party_simplified), color = "white") +  # Color by winner (Bush or Gore)
  scale_fill_manual(values = c("REPUBLICAN" = "red", "DEMOCRAT" = "blue"), name = "Winner") +
  geom_text(aes(label = elec_college, geometry = geometry), size = 5, stat = "sf_coordinates", color = "white") +
  labs(title = "2000 U.S. Presidential Election Results by State") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Alaska: scale down and reposition
alaska <- location_and_election %>%
  filter(state == "ALASKA") %>%
  st_transform(common_crs) %>%
  mutate(geometry = st_geometry(.) * 0.35 + c(55, -30))  # Scale and reposition Alaska
glimpse(alaska)
# Hawaii: scale up and reposition
hawaii <- location_and_election %>%
  filter(state == "HAWAII") %>%
  st_transform(common_crs) %>%
  mutate(geometry = st_geometry(.) * 1.5 + c(70, -20))   # Scale and reposition Hawaii
glimpse(hawaii)
# Combine the parts together
location_and_election_inset <- bind_rows(contiguous_us, alaska, hawaii)
glimpse(location_and_election_inset)
# Plot the map with Alaska and Hawaii inset
ggplot() +
  geom_sf(data = location_and_election_inset, aes(fill = winner), color = "black") +
  scale_fill_manual(values = c("Bush" = "red", "Gore" = "blue"), name = "Winner") +
  geom_text(data = location_and_election_inset, 
            aes(label = electoral_votes, geometry = geometry), 
            size = 3, stat = "sf_coordinates") +
  labs(title = "2000 U.S. Presidential Election Results by State") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(presidents)
#faceted graph of elections
presidents_all <- presidents %>%
  group_by(state,year) %>%
  slice_max(candidatevotes, n = 1, with_ties = FALSE) %>%
  select(state, candidate, party_simplified,candidatevotes)

glimpse(presidents_all)

house_of_reps_all <- house_of_reps |>
  group_by(state,year) |>
  summarize(district_count = n_distinct(district))
print(house_of_reps_all)

combined_data_all <-presidents_all%>%
  inner_join(house_of_reps_all, by = c("state","year"))  %>%
  mutate(elec_college = district_count+2)

print(combined_data_all)

location_and_election_all <- all_states_outline %>%
  full_join(combined_data_all, by = "state") %>%
  drop_na()
print(location_and_election_all)

contiguous_us_all <- location_and_election_all %>%
  filter(!state %in% c("ALASKA", "HAWAII")) %>%
  st_transform(common_crs)
glimpse(contiguous_us_all)

ggplot(data = contiguous_us_all) +
  geom_sf(aes(fill = party_simplified), color = "black") +  # Color by winner (Bush or Gore)
  scale_fill_manual(values = c("REPUBLICAN" = "red", "DEMOCRAT" = "blue"), name = "Winner") +
  geom_text(aes(label = elec_college, geometry = geometry), size = 3, stat = "sf_coordinates") +
  labs(title = "2000 U.S. Presidential Election Results by State") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~ year)  # Facet by year


library(httr)
library(sf)
library(dplyr)
library(utils)

# url and string for file
base_url <- "https://cdmaps.polisci.ucla.edu/shp/"
file_names <- sprintf("districts%03d.zip", 1:114)  # Generates file names from districts001.zip to districts114.zip

# temp directory
download_dir <- "congressional_shapefiles"
if (!dir.exists(download_dir)) dir.create(download_dir)

# download & unzip 
download_and_load_shapefile <- function(file_name, sample_fraction = 0.1) {  # Add sample_fraction parameter
  local_zip_path <- file.path(download_dir, file_name)
  url <- paste0(base_url, file_name)
  
  # checking if already downloaded
  if (!file.exists(local_zip_path)) {
    cat("Downloading", file_name, "...\n")
    GET(url, write_disk(local_zip_path, overwrite = TRUE))
  } else {
    cat(file_name, "already exists, skipping download.\n")
  }
  
  # Unzip and load the shapefile
  unzip_dir <- file.path(download_dir, sub(".zip$", "", file_name))  # Unzipped folder
  if (!dir.exists(unzip_dir)) {
    unzip(local_zip_path, exdir = unzip_dir)
  }
  
  # load the .shp file
  district_shapes_dir <- file.path(unzip_dir, "districtShapes")
  shp_file <- list.files(district_shapes_dir, pattern = "\\.shp$", full.names = TRUE)
  
  if (length(shp_file) > 0) {
    shapefile_data <- st_read(shp_file)
    
    # Sub-sample the shapefile data (e.g., take 10% of rows)
    shapefile_data <- shapefile_data %>% sample_frac(sample_fraction)
    
    return(shapefile_data)
  } else {
    message("No shapefile found in 'districtShapes' for", file_name)
    return(NULL)
  }
}

# Loop through and process the shapefiles with sub-sampling
shapefiles_list <- list()
for (file_name in file_names) {
  shapefile_data <- download_and_load_shapefile(file_name, sample_fraction = 0.1)  # 10% sample
  if (!is.null(shapefile_data)) {
    shapefiles_list[[file_name]] <- shapefile_data
  }
}
