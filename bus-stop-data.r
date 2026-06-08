library(dplyr)
library(tidyr)
library(tidyverse)
library(pdftools)
library(dplyr)
library(stringr)

# scrape table from url of removed stops. stop tables for DC stops are on pages 3-10
wmata_table <- pdf_text("https://www.wmata.com/initiatives/plans/Better-Bus/upload/Bus-Stop-Consolidation_2025-Better-Bus-Network.pdf")

# function to process the tables from pages 3 to 10 and return the results in a vector called DC_removed_stops
process_pages <- function(wmata_table, start_page = 3, end_page = 10) 
{
  results <- list()
  for (page in start_page:end_page) {
    removed_stops <- wmata_table[page]
    removed_stops <- strsplit(removed_stops, "\n")
    removed_stops <- removed_stops[[1]]
    results[[page]] <- removed_stops
  }
  return(results)
}
DC_removed_stops <- process_pages(wmata_table)

# Each page of the pdf has a different number of rows needed in them. Establish rows with bus stops in them.
page_ranges <- list(
  "3" = c(17, 41),
  "4" = c(4, 43),
  "5" = c(4, 43),
  "6" = c(4, 42),
  "7" = c(4, 43),
  "8" = c(4, 42),
  "9" = c(4, 43),
  "10" = c(4, 18)
)
# Function to capture only the rows with stop information in them
extract_rows <- function(wmata_table, page_ranges) {
  results <- list()
  
  for (page in names(page_ranges)) {
    page_num <- as.numeric(page)
    rows <- wmata_table[page_num]
    rows <- strsplit(rows, "\n")[[1]]
    start_row <- page_ranges[[page]][1]
    end_row <- page_ranges[[page]][2]
    extracted_rows <- rows[start_row:end_row]
    results[[page_num]] <- extracted_rows
  }
  return(results)
}

DC_removed_stops <- extract_rows(wmata_table, page_ranges)
# Unlist so all stops are in one vector
DC_removed_stops <- unlist(DC_removed_stops)
# seperate the data into 3 columns
DC_removed_stops <- str_split_fixed(DC_removed_stops, " {2,}", 3)

# This table has the data needed, but there are a few formatting issues to clean. If data in one cell 
# takes up two lines of text, it is read in as a seperate row. It needs to be appended to the previous row.
# We will convert to data frame, remove blank rows, remove the routes column (not needed for analysis), and
# append the rows that need to be fixed with a loop function.

DC_removed_stops <- as.data.frame(DC_removed_stops) |>
  filter(!V1 == "") |>
  select(-V2)  
# new V1 column for the updated values
DC_removed_stops$V1_updated <- DC_removed_stops$V1

# Loop
for (i in 1:nrow(DC_removed_stops)) {
  # Check if V3 is blank
  if (DC_removed_stops$V3[i] == "") {
    # If it's not the first row, append V1 to the previous row's V1_updated
    if (i > 1) {
      DC_removed_stops$V1_updated[i-1] <- paste(DC_removed_stops$V1_updated[i-1], DC_removed_stops$V1[i], sep = " ")
    }
  }
}

# Filter out the rows where V3 is blank
DC_removed_stops <- DC_removed_stops %>% filter((!V3 == "")) |>
  select(-V1) |>
  rename(stop_location = V1_updated,
         stop_id = V3) 

# We now have our cleaned list of stops that were phased out in the Better Bus redesign.
head(DC_removed_stops)

# load list of all metro stops from working directory or download from https://opendata.dc.gov/datasets/DCGIS::metro-bus-stops/about
all_bus_stops <- read_csv("Metro_Bus_Stops.csv")

# keep variables of interest, filter to only include DC stops, rename columns
all_bus_stops <- all_bus_stops |>
  select(
    BSTP_LON,
    BSTP_LAT,
    BSTP_MSG_TEXT,
    REG_ID,
    WARD_ID,
    ANC_ID,
    SMD_ID # maybe exclude
  ) |>
  filter(WARD_ID < 99) |>
  rename(
    stop_x = BSTP_LON,
    stop_y = BSTP_LAT,
    stop_location = BSTP_MSG_TEXT,
    stop_id = REG_ID,
    ward = WARD_ID,
    anc = ANC_ID,
    smd = SMD_ID # maybe exclude
  )

# create flag for current stop status
all_bus_stops <- all_bus_stops |>
  mutate(
    stop_status = ifelse(stop_id %in% DC_removed_stops$stop_id,
                         "removed",
                         "kept"))

# view stop list
head(all_bus_stops)

write.csv(all_bus_stops,'all_bus_stops.csv')

wards_data <- read.csv('wards_info.csv')

wards_data <- wards_data %>%
  mutate(
    Name = as.integer(str_extract(Name, "\\d+")
  )) %>%
  rename(ward = Name)

wards_metro_data <- left_join(metrodata, wards_data)

names(wards_metro_data)[99] <- "median_income"
names(wards_data)[91] <- "median_income"
names(wards_data)[13] <- "public_commuters"

kept_removed_counts <- all_bus_stops %>%
  count(ward, stop_status)

kept_removed_counts <- pivot_wider(kept_removed_counts,
             names_from = "stop_status",
             values_from = "n")            

wards_data <- full_join(wards_data, kept_removed_counts)

wards_data <- wards_data %>%
  mutate(perc_removed = removed/(kept + removed))


wards_data_simplified <- wards_data %>%
  select(ward, public_commuters, median_income, kept, removed, perc_removed) %>%
  mutate(
    median_income = str_replace_all(median_income, ",",""),
    public_commuters = str_replace_all(public_commuters, ",","")) %>%
  mutate(
    median_income = as.numeric(median_income),
    public_commuters = as.numeric(public_commuters)
  )

class(wards_data_simplified$public_commuters)
summary(wards_data_simplified$median_income)


wards_data_simplified %>%
  ggplot(aes(x = median_income, y = perc_removed)) +
  geom_point() +
  geom_smooth(method=lm)

wards_data_simplified %>%
  ggplot(aes(x = ward, y = perc_removed)) +
  geom_bar(stat = "identity")

model <- lm(perc_removed ~ median_income, data = wards_data_simplified)
broom::tidy(model)

wards_data_simplified %>%
  ggplot(aes(x = median_income, y = perc_removed)) +
  geom_point() +
  geom_smooth(method=lm) +
  theme_classic()
