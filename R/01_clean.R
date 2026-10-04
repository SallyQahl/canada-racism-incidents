# =============================================================================
# 01_clean.R — Clean the raw incident file and build analysis-ready tables
#
# Run from the repository root:   Rscript R/01_clean.R
#
# Input : data/raw/incidents_raw.csv
#         data/reference/province_population.csv
#         data/reference/canada_provinces.geojson
# Output: app/data/incidents.rds        one row per incident
#         app/data/labels_long.rds      one row per incident x label (multi-label fields)
#         app/data/provinces.rds        province polygons joined to counts + rates
#         data/processed/incidents_clean.csv  (flat copy for non-R users)
# =============================================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr)
  library(stringr); library(lubridate); library(sf)
})

raw <- read_csv("data/raw/incidents_raw.csv", show_col_types = FALSE,
                name_repair = "unique_quiet", na = c("", "NA", "N/A"))

# -----------------------------------------------------------------------------
# 1. Core incident table
# -----------------------------------------------------------------------------
# Notes:
# * "Name of Victim(s)" and "Notes" are 100% empty -> dropped.
# * The first column is a row index from an earlier export -> dropped.
# * Missing province / city become an explicit "Not specified" level instead of
#   silently disappearing (this is what produced "ontario_na" in the old app).
# * "Female, Male" means the incident involved victims of both sexes.

incidents <- raw |>
  select(-any_of(c("...1", "Name of Victim(s)", "Notes"))) |>
  rename(
    incident_id   = `Incident ID`,
    url           = `Article URL`,
    description   = `Incident Description`,
    date          = `Incident Date`,
    province      = Province,
    city          = `City or Region`,
    location      = `Detailed Location`,
    category      = `Incident Category`,
    victim_gender = `Gender of Victim(s)`,
    community     = `Ethnic Community`,
    identity      = `Identity-Based`,
    context       = Context,
    incident_type = `Type of Incident`
  ) |>
  mutate(
    date        = ymd(date),
    year        = year(date),
    month       = floor_date(date, "month"),
    province    = coalesce(str_squish(province), "Not specified"),
    city        = coalesce(str_squish(city), "Not specified"),
    source      = str_remove(str_extract(url, "(?<=://)[^/]+"), "^www\\."),
    criminal    = str_starts(category, "Criminal"),
    victim_gender = case_when(
      is.na(victim_gender)            ~ "Not reported",
      victim_gender == "Female, Male" ~ "Female & male",
      TRUE                            ~ victim_gender
    )
  )

# Country-of-origin detail columns (e.g. "Nigeria" under Black/African/Caribbean)
origin_cols <- c("Black/African/Caribbean", "Central Asian", "East Asian",
                 "Indigenous", "Latin American", "Middle Eastern",
                 "South Asian", "Southeast Asian")

# -----------------------------------------------------------------------------
# 2. Multi-label fields -> long format
# -----------------------------------------------------------------------------
# Four fields store several labels in one cell, e.g. "Hate speech: Other, Vandalism".
# Each label also has a parent ("Hate speech") and an optional sub-type ("Other").
# Counting the raw strings (as the old app did) splits one label across many
# combinations; the long table counts every label once per incident.

split_labels <- function(df, col, field_name) {
  df |>
    select(incident_id, value = all_of(col)) |>
    filter(!is.na(value)) |>
    separate_longer_delim(value, delim = ", ") |>
    mutate(
      field  = field_name,
      label  = str_squish(value),
      parent = str_squish(str_remove(label, ":.*$")),
      sub    = str_squish(str_extract(label, "(?<=:).*$"))
    ) |>
    select(incident_id, field, label, parent, sub) |>
    distinct()
}

labels_long <- bind_rows(
  split_labels(incidents, "incident_type", "Type of incident"),
  split_labels(incidents, "context",       "Context"),
  split_labels(incidents, "identity",      "Identity-based"),
  split_labels(incidents, "community",     "Targeted community")
) |>
  # "Religious discrimination: Islamophobia" reads better as just "Islamophobia"
  mutate(short = if_else(parent == "Religious discrimination" & !is.na(sub), sub, parent))

incidents <- incidents |> select(-all_of(origin_cols))

# -----------------------------------------------------------------------------
# 3. Province polygons + per-capita rates
# -----------------------------------------------------------------------------
# The boundary file names Yukon "Yukon Territory"; normalising names here is the
# fix for provinces showing as "No Data" on the old map.

norm_prov <- function(x) {
  x |>
    str_squish() |>
    str_replace("^Yukon Territory$", "Yukon") |>
    str_replace("^Qu[eé]bec$", "Quebec")
}

pop <- read_csv("data/reference/province_population.csv", show_col_types = FALSE)

prov_counts <- incidents |>
  filter(province != "Not specified") |>
  mutate(province = norm_prov(province)) |>
  count(province, name = "incidents")

provinces <- st_read("data/reference/canada_provinces.geojson", quiet = TRUE) |>
  transmute(province = norm_prov(name)) |>
  st_make_valid() |>
  st_simplify(dTolerance = 2000, preserveTopology = TRUE) |>   # smaller, faster map
  left_join(pop, by = "province") |>
  left_join(prov_counts, by = "province") |>
  mutate(
    incidents   = coalesce(incidents, 0L),
    per_100k    = incidents / population_2021 * 1e5
  )

# Fail loudly if any province failed to join — never ship a silent "No Data"
stopifnot(!any(is.na(provinces$population_2021)))
stopifnot(sum(provinces$incidents) == sum(prov_counts$incidents))

# -----------------------------------------------------------------------------
# 4. Save
# -----------------------------------------------------------------------------
dir.create("app/data", showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

saveRDS(incidents,   "app/data/incidents.rds")
saveRDS(labels_long, "app/data/labels_long.rds")
saveRDS(provinces,   "app/data/provinces.rds")
write_csv(incidents, "data/processed/incidents_clean.csv")
write_csv(labels_long, "data/processed/labels_long.csv")

message(sprintf(
  "Cleaned %d incidents (%s to %s); %d labels in long table; %d province polygons.",
  nrow(incidents), min(incidents$date, na.rm = TRUE), max(incidents$date, na.rm = TRUE),
  nrow(labels_long), nrow(provinces)
))
