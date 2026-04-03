# ==============================================================================
# 01_data.R — Tesla Trend Analysis (DK) · opdateret arkitektur
# Formål:   Hent, filtrer, segmenter og aggreger Bilbasen-data fra BigQuery
# Kilde:    imposing-yen-426717-u4.bilinfo.staging_bilinfo_single
# Output:   data/market_data.rds
#           data/weekly_volume.rds
#           data/weekly_price.rds
#           data/events.rds
#           data/params_this_run.rds
# Køres:    Manuelt — output valideres før 02_charts.R køres
# ==============================================================================

library(bigrquery)
library(dplyr)
library(lubridate)
library(stringr)
library(glue)

# ------------------------------------------------------------------------------
# 0. Parametre — alle metodiske valg samlet ét sted
#    Næste opdatering: juster end_date
# ------------------------------------------------------------------------------

project    <- "imposing-yen-426717-u4"
start_date <- as.Date("2024-07-01")   # Som original — ustabil dækning før
end_date   <- Sys.Date()
price_min  <- 10000                   # DKK — fjerner leasing/fejlposter
price_top  <- 0.995                   # Øverste 0.5% trimmes

# Julperioder ekskluderes begge år — anomalt lav aktivitet
holiday_start1 <- as.Date("2024-12-20")
holiday_end1   <- as.Date("2025-01-05")
holiday_start2 <- as.Date("2025-12-20")
holiday_end2   <- as.Date("2026-01-05")

# BYD inkluderes kun fra denne dato — utilstrækkeligt volumen før
byd_start <- as.Date("2025-01-01")

# Hyundai Ioniq 5 — stabil fra sep 2024, men inkluderes fra start_date
# med eksplicit note om lav volumen i jul-aug 2024
# Den allersidste uge i datasættet kan være ufuldstændig — håndteres i 02_charts.R

# Anomalidetektion — uger over denne multipel af median ekskluderes
weekly_cap_multiplier <- 4

output_dir <- "data"
if (!dir.exists(output_dir)) dir.create(output_dir)

# ------------------------------------------------------------------------------
# 1. Hent rådata fra BigQuery
# ------------------------------------------------------------------------------
# FLOW-baseret analyse: tidsaksen er listing creation date (CreatedDate)
# — ikke lagerøjebliksbillede (stock). Se narrative_template.md.
# CreatedDate er STRING i formatet 'dd-mm-yyyy HH:MM:SS' i denne tabel.

sql <- glue("
  SELECT
    TRIM(Make)                                                       AS make,
    TRIM(Model)                                                      AS model,
    TRIM(Variant)                                                    AS variant,
    TRIM(FuelType)                                                   AS fuel_type,
    Price                                                            AS price,
    PriceType                                                        AS price_type,
    DATE(PARSE_TIMESTAMP('%d-%m-%Y %H:%M:%S', CreatedDate))         AS date_added
  FROM `imposing-yen-426717-u4.bilinfo.staging_bilinfo_single`
  WHERE
    PriceType = 'RetailPrice'
    AND Price >= {price_min}
    AND Make IS NOT NULL
    AND Model IS NOT NULL
    AND DATE(PARSE_TIMESTAMP('%d-%m-%Y %H:%M:%S', CreatedDate))
        BETWEEN '{start_date}' AND '{end_date}'
", .open = "{", .close = "}")

raw <- bq_project_query(project, sql) |>
  bq_table_download(quiet = FALSE)

# ------------------------------------------------------------------------------
# 2. Sanity check 1 — rådata
# ------------------------------------------------------------------------------

cat("\n--- Sanity check 1: rådata ---\n")
cat("Rækker hentet:      ", nrow(raw), "\n")
cat("Periode:            ", as.character(min(raw$date_added)),
    "→", as.character(max(raw$date_added)), "\n")
cat("Antal brands:       ", n_distinct(raw$make), "\n")
cat("Manglende price:    ", sum(is.na(raw$price)), "\n")
cat("Manglende fuel:     ", sum(is.na(raw$fuel_type)), "\n")

# ------------------------------------------------------------------------------
# 3. Pris-trim, julfilter og segmentering
# ------------------------------------------------------------------------------

upper_cutoff <- quantile(raw$price, price_top, na.rm = TRUE)
cat("\nPris-cutoff (", price_top * 100, "%-percentil):", upper_cutoff, "DKK\n")

market_data <- raw |>
  filter(
    price <= upper_cutoff,
    # Ekskludér julperioder begge år
    !(date_added >= holiday_start1 & date_added <= holiday_end1),
    !(date_added >= holiday_start2 & date_added <= holiday_end2)
  ) |>
  mutate(
    week = floor_date(date_added, "week"),
    
    fuel_group = case_when(
      str_to_lower(fuel_type) %in% c("el", "electric") ~ "Electric",
      TRUE                                              ~ "Fossil"
    ),
    
    segment = case_when(
      # Primært analysesubjekt
      make == "Tesla"                                          ~ "Tesla",
      
      # Primære referencer — samme prissegment, forhandlersalg
      make == "VW"      & str_detect(model, "^ID\\.")         ~ "VW ID",
      make == "Hyundai" & model == "Ioniq 5"                  ~ "Hyundai Ioniq 5",
      
      # Sekundær reference
      make == "Skoda"   & str_detect(model,
                                     regex("Enyaq", ignore_case = TRUE)) ~ "Skoda Enyaq",
      
      # BYD — ny aktør, kun fra byd_start
      make == "BYD" & date_added >= byd_start                 ~ "BYD",
      
      # Markedssegmenter
      fuel_group == "Electric"                                ~ "Electric vehicles",
      fuel_group == "Fossil"                                  ~ "Fossil vehicles",
      TRUE                                                    ~ "Other"
    )
  )

# ------------------------------------------------------------------------------
# 4. Sanity check 2 — segmenter
# ------------------------------------------------------------------------------

cat("\n--- Sanity check 2: segment-fordeling ---\n")
market_data |>
  count(segment, sort = TRUE) |>
  print()

brand_check <- market_data |>
  filter(segment %in% c("Tesla", "VW ID", "Hyundai Ioniq 5",
                        "Skoda Enyaq", "BYD")) |>
  group_by(segment) |>
  summarise(
    n_total   = n(),
    n_weeks   = n_distinct(week),
    median_wk = median(as.numeric(table(week))),
    .groups   = "drop"
  )

cat("\n--- Brand-niveau ---\n")
print(brand_check)

for (seg in c("Tesla", "VW ID", "Hyundai Ioniq 5")) {
  med <- brand_check$median_wk[brand_check$segment == seg]
  if (length(med) == 0 || med < 10)
    warning(paste(seg, "— median/uge under 10. Tjek segmentfilter."))
}

# ------------------------------------------------------------------------------
# 5. Ugentlig volumenaggregering
# ------------------------------------------------------------------------------

volume_brands <- market_data |>
  filter(segment %in% c(
    "Tesla", "VW ID", "Hyundai Ioniq 5", "Skoda Enyaq", "BYD",
    "Electric vehicles", "Fossil vehicles"
  )) |>
  count(week, segment)

volume_total <- market_data |>
  count(week) |>
  mutate(segment = "Total market")

weekly_volume <- bind_rows(volume_brands, volume_total) |>
  arrange(segment, week)

# ------------------------------------------------------------------------------
# 6. Anomalidetektion — ugentlig volumencap per segment
# Uger med n > 4x segmentets median ekskluderes
# Begrundelse: batch-imports giver urealistiske volumenstigninger der
# forvrænger breakpoint-estimater og LOESS-kurver
# ------------------------------------------------------------------------------

segment_medians <- weekly_volume |>
  group_by(segment) |>
  summarise(median_n = median(n), .groups = "drop")

weekly_volume <- weekly_volume |>
  left_join(segment_medians, by = "segment") |>
  mutate(anomaly_flag = n > weekly_cap_multiplier * median_n)

anomalies <- weekly_volume |> filter(anomaly_flag)
if (nrow(anomalies) > 0) {
  cat("\n--- ADVARSEL: Anomale uger detekteret og ekskluderet ---\n")
  print(anomalies |> select(week, segment, n, median_n))
}

weekly_volume <- weekly_volume |>
  filter(!anomaly_flag) |>
  select(-median_n, -anomaly_flag)

# Fjern sidste uge hvis den er ufuldstændig (under 4 dage data)
weekly_volume <- weekly_volume |>
  filter(week < floor_date(end_date - days(3), "week"))

# ------------------------------------------------------------------------------
# 7. Ugentlig prisaggregering
# IQR og SD inkluderes til event-volatilitetsanalysen (Musk/Trump)
# ------------------------------------------------------------------------------

weekly_price <- market_data |>
  filter(segment %in% c(
    "Tesla", "VW ID", "Hyundai Ioniq 5", "Skoda Enyaq",
    "Electric vehicles", "Fossil vehicles"
  )) |>
  group_by(week, segment) |>
  summarise(
    n            = n(),
    median_price = median(price, na.rm = TRUE),
    iqr_price    = IQR(price, na.rm = TRUE),
    sd_price     = sd(price, na.rm = TRUE),
    .groups      = "drop"
  )

# ------------------------------------------------------------------------------
# 8. Sanity check 3 — ugentlig dækning efter anomalifjernelse
# ------------------------------------------------------------------------------

cat("\n--- Sanity check 3: ugentlig dækning (efter anomalifjernelse) ---\n")
weekly_volume |>
  group_by(segment) |>
  summarise(
    uger     = n(),
    min_n    = min(n),
    median_n = median(n),
    max_n    = max(n),
    .groups  = "drop"
  ) |>
  arrange(desc(median_n)) |>
  print()

# ------------------------------------------------------------------------------
# 9. Event-markører
# ------------------------------------------------------------------------------
# Faktuelle datoer — postulerer ikke kausalitet.
# Bruges som kontekst og til event-volatilitetsanalysen.

events <- tibble::tibble(
  label = c(
    "Musk backs Trump",
    "Trump elected",
    "Trump inaugurated"
  ),
  date = as.Date(c(
    "2024-07-13",
    "2024-11-05",
    "2025-01-20"
  )),
  source = c(
    "Reuters, 2024-07-13",
    "AP News, 2024-11-05",
    "Official record"
  )
)

# ------------------------------------------------------------------------------
# 10. Gem output + reproducerbarhed
# ------------------------------------------------------------------------------

saveRDS(market_data,   file.path(output_dir, "market_data.rds"))
saveRDS(weekly_volume, file.path(output_dir, "weekly_volume.rds"))
saveRDS(weekly_price,  file.path(output_dir, "weekly_price.rds"))
saveRDS(events,        file.path(output_dir, "events.rds"))

params_this_run <- list(
  run_date              = Sys.time(),
  start_date            = start_date,
  end_date              = end_date,
  price_min             = price_min,
  price_top             = price_top,
  holiday_start1        = holiday_start1,
  holiday_end1          = holiday_end1,
  holiday_start2        = holiday_start2,
  holiday_end2          = holiday_end2,
  byd_start             = byd_start,
  weekly_cap_multiplier = weekly_cap_multiplier,
  source                = "imposing-yen-426717-u4.bilinfo.staging_bilinfo_single",
  n_rows_raw            = nrow(raw),
  n_rows_clean          = nrow(market_data),
  n_anomalies           = nrow(anomalies)
)
saveRDS(params_this_run, file.path(output_dir, "params_this_run.rds"))

cat("\n✓ Alle filer gemt i", output_dir, "\n")
cat("  market_data.rds   —", nrow(market_data), "rækker\n")
cat("  weekly_volume.rds —", nrow(weekly_volume), "rækker\n")
cat("  weekly_price.rds  —", nrow(weekly_price), "rækker\n")
cat("  events.rds        —", nrow(events), "events\n")
cat("  params_this_run.rds\n")
cat("\nGå igennem sanity checks ovenfor inden du kører 02_charts.R\n")