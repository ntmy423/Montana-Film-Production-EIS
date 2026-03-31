#
#title: "Spending by county shapefile"
#author: "My Nguyen"
#date: "2026-03-23"
#

library(sf)
library(dplyr)
library(readr)
library(stringr)


# 1. FILE PATHS
county_shp_path <- "C:/Users/mnguyen/OneDrive - Econsult Solutions Inc/Montana Film Industry EIS - General/2025-2026/6-Report/Maps/Shapefiles/MontanaCounties_shp/MontanaCounties_shp/County.shp"
spending_csv_path <- "C:/Users/mnguyen/OneDrive - Econsult Solutions Inc/Montana Film Industry EIS - General/2025-2026/6-Report/Maps/Spending by county_2024&2025.csv"
output_folder <- "C:/Users/mnguyen/OneDrive - Econsult Solutions Inc/Montana Film Industry EIS - General/2025-2026/6-Report/Maps/Shapefiles/output"

dir.create(output_folder, showWarnings = FALSE)

# 2. READ COUNTY SHAPEFILE
list.files(
  "C:/Users/mnguyen/OneDrive - Econsult Solutions Inc/Montana Film Industry EIS - General/2025-2026/6-Report/Maps/Shapefiles/MontanaCounties_shp/MontanaCounties_shp",
  pattern = "\\.shp$",
  full.names = TRUE
)

mt_counties <- st_read(county_shp_path, quiet = TRUE)

mt_counties <- mt_counties %>%
  mutate(
    county_clean = NAME %>%
      str_to_title() %>%
      str_squish()
  )

# 3. READ SPENDING CSV
spending_raw <- read_csv(spending_csv_path, show_col_types = FALSE)

spending <- spending_raw %>%
  mutate(
    county_clean = County %>%
      str_to_title() %>%
      str_squish(),
    spend_m = `Total Production Spend ($M)` %>%
      str_replace_all("\\$", "") %>%
      str_replace_all(",", "") %>%
      str_squish() %>%
      as.numeric()
  ) %>%
  select(county_clean, spend_m)

# 4. JOIN SPENDING TO COUNTY POLYGONS
mt_spending <- mt_counties %>%
  left_join(spending, by = "county_clean")

# 5. CHECK FOR MISMATCHES
cat("\nCounties in CSV not matched to shapefile:\n")
print(anti_join(spending, st_drop_geometry(mt_counties), by = "county_clean"))

cat("\nCounties in shapefile with no CSV match:\n")
print(anti_join(st_drop_geometry(mt_counties), spending, by = "county_clean"))

# 6. OPTIONAL CATEGORY FIELD
mt_spending <- mt_spending %>%
  mutate(
    spend_cat = case_when(
      is.na(spend_m) ~ "Data not available",
      spend_m < 1 ~ "Less than $1m",
      spend_m < 3 ~ "$1 - $3m",
      spend_m < 9 ~ "$3 - $9m",
      spend_m < 20 ~ "$9 - $20m",
      spend_m <= 35 ~ "$20 - $35m",
      TRUE ~ "More than $35m"
    )
  )

# 7. EXPORT SHAPEFILE
mt_spending_export <- mt_spending %>%
  select(NAME, County, FIPS, spend_m, spend_cat, geometry) %>%
  rename(
    spendcat = spend_cat
  )

st_write(
  mt_spending_export,
  file.path(output_folder, "mt_total_production_spend.shp"),
  delete_dsn = TRUE,
  quiet = TRUE
)

cat("\nDone. Shapefile saved here:\n")
cat(file.path(output_folder, "mt_total_production_spend.shp"), "\n")