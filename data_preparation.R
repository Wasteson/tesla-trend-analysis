# 📦 Pakker
library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(segmented)
library(purrr)
library(tibble)
library(gridExtra)
library(grid)
library(scales)  # for number_format()
library(knitr)
library(tidyr)
library(ggrepel)
library(zoo)

# 📁 Filsti – tilpass ved behov
data_path <- "/Users/oystein/Desktop/wasteson/TrackSights/data/bilbasen/tesla-insight/Bilbasen biler 1903.xlsx"



# 🗓️ Datoperiode og filtre
start_date     <- as.Date("2024-07-01")
holiday_start  <- as.Date("2024-12-20")
holiday_end    <- as.Date("2025-01-05")
min_price      <- 10000

# 📥 Last inn data
raw_data <- read_excel(data_path)

# 🧹 Rens og strukturer
market_data <- raw_data %>%
  mutate(
    date = dmy_hms(CreatedDate),
    week = floor_date(date, "week")
  ) %>%
  filter(
    date >= start_date,
    !(date >= holiday_start & date <= holiday_end),
    PriceType == "RetailPrice",
    !is.na(Price), Price >= min_price
  ) %>%
  mutate(
    fuel_group = case_when(
      str_to_lower(FuelType) %in% c("el", "electric") ~ "Electric",
      TRUE ~ "Fossil"
    ),
    segment = case_when(
      make == "Tesla" ~ "Tesla",
      make == "VW" & str_detect(model, "^ID\\.") ~ "VW ID",
      make == "Skoda" & str_detect(model, regex("Enyaq", ignore_case = TRUE)) ~ "Skoda Enyaq",
      fuel_group == "Electric" ~ "Electric vehicles",
      fuel_group == "Fossil" ~ "Fossil vehicles",
      TRUE ~ "Other"
    )
  )

# 📦 Output: objektet `market_data` er klart til analyse

# 🧹 Fjern ekstreme outliers (øverste 0.5 %)
upper_cutoff <- quantile(market_data$Price, 0.995, na.rm = TRUE)

market_data <- market_data %>%
  filter(Price <= upper_cutoff)


# datarens og strukturering
# 🔧 Bearbeid og strukturer data
market_data <- raw_data %>%
  mutate(
    date = dmy_hms(CreatedDate),
    week = floor_date(date, "week"),
    fuel_group = case_when(
      str_to_lower(FuelType) %in% c("el", "electric") ~ "Electric",
      TRUE ~ "Fossil"
    ),
    segment = case_when(
      make == "Tesla" ~ "Tesla",
      make == "VW" & str_detect(model, "^ID\\.") ~ "VW ID",
      make == "Skoda" & str_detect(model, regex("Enyaq", ignore_case = TRUE)) ~ "Skoda Enyaq",
      fuel_group == "Electric" ~ "Electric vehicles",
      fuel_group == "Fossil" ~ "Fossil vehicles",
      TRUE ~ "Other"
    )
  ) %>%
  # 🎯 Filtrer på ønsket periode og pris
  filter(
    date >= start_date,
    PriceType == "RetailPrice",
    !is.na(Price),
    Price >= min_price,
    !(date >= holiday_start & date <= holiday_end)
  )


# Graf 1: "Weekly retail volume – Total vs Electric vs Fossil"
# 📈 Ukentlig volum for hver hovedkategori
volume_primary <- market_data %>%
  filter(segment %in% c("Electric vehicles", "Fossil vehicles")) %>%
  count(week, segment)

# 📦 Totalmarked (samme filter som over)
volume_total <- market_data %>%
  count(week) %>%
  mutate(segment = "Total market")

# 🔗 Kombinér og harmoniser datoformat
volume_combined <- bind_rows(volume_primary, volume_total) %>%
  mutate(week = as.POSIXct(week))

# 📍 Politisk event-data
events <- tibble(
  label = c("Musk backs Trump", "Trump elected", "Trump inaugurated"),
  date = as.POSIXct(c("2024-07-13", "2024-11-05", "2025-01-15"))
)

holiday_start <- as.POSIXct(holiday_start)
holiday_end   <- as.POSIXct(holiday_end)


# 🎨 Mørk visualisering
ggplot(volume_combined, aes(x = week, y = n, color = segment)) +
  geom_line(linewidth = 1.2) +
  annotate("rect",
           xmin = holiday_start, xmax = holiday_end,
           ymin = -Inf, ymax = Inf,
           fill = "gray30", alpha = 0.3) +
  # 🎄 Vertikal tekst i julefeltet
  annotate("text",
           x = holiday_start + (holiday_end - holiday_start) / 2,
           y = max(volume_combined$n) * 0.8,
           label = "Christmas holiday\n(low activity)",
           color = "gray80", size = 3.5, angle = 90, hjust = -0.1) +
  geom_vline(data = events, aes(xintercept = date),
             linetype = "dashed", color = "white") +
  geom_text(data = events, aes(x = date, y = Inf, label = label),
            angle = 90, vjust = -0.5, hjust = 1.1,
            size = 3.5, color = "white", inherit.aes = FALSE) +
  scale_color_manual(values = c(
    "Electric vehicles" = "#FFDD57",
    "Fossil vehicles" = "#FF6B6B",
    "Total market" = "#00c0c7"
  )) +
  labs(
    title = "Weekly retail volume – Market overview",
    subtitle = "Actual listings per week, from July 2024",
    x = "Week", y = "Number of listings", color = "Segment"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.background = element_rect(fill = "#1e1e1e"),
    legend.key = element_rect(fill = "#1e1e1e")
  )

ggsave(
  filename = "Weekly retail volume – Market overview.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)


# Graf 2: "Weekly retail volume – Tesla vs VW ID vs Skoda Enyaq"
# 📈 Ukentlig volum for valgte elbilsegmenter
volume_ev_segments <- market_data %>%
  filter(segment %in% c("Tesla", "VW ID", "Skoda Enyaq")) %>%
  count(week, segment) %>%
  mutate(week = as.POSIXct(week))

# 🎨 Visualisering (mørkt tema + julefelt + events)
ggplot(volume_ev_segments, aes(x = week, y = n, color = segment)) +
  geom_line(linewidth = 1.2) +
  
  # 🕯️ Grå felt for juleperioden
  annotate("rect",
           xmin = holiday_start, xmax = holiday_end,
           ymin = -Inf, ymax = Inf,
           fill = "gray30", alpha = 0.3) +
  annotate("text",
           x = holiday_start + (holiday_end - holiday_start) / 2,
           y = max(volume_ev_segments$n) * 0.8,
           label = "Christmas holiday\n(low activity)",
           color = "gray80", size = 3.5,
           angle = 90, hjust = 0) +
  
  # 🏛️ Politiske hendelser
  geom_vline(data = events, aes(xintercept = date),
             linetype = "dashed", color = "white") +
  geom_text(data = events, aes(x = date, y = Inf, label = label),
            angle = 90, vjust = -0.5, hjust = 1.1,
            size = 3.5, color = "white", inherit.aes = FALSE) +
  
  # 🎨 Tilpasset fargepalett
  scale_color_manual(values = c(
    "Tesla" = "#E82127",
    "VW ID" = "#1A93D9",
    "Skoda Enyaq" = "#F9A825"
  )) +
  
  # 📝 Titler og layout
  labs(
    title = "Weekly retail volume – Tesla vs VW ID vs Skoda Enyaq",
    subtitle = "Actual listings per week · Retail only · From July 2024",
    x = "Week", y = "Number of listings", color = "Brand"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.background = element_rect(fill = "#1e1e1e"),
    legend.key = element_rect(fill = "#1e1e1e")
  )

ggsave(
  filename = "Weekly retail volume – Tesla vs VW ID vs Skoda Enyaq.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)


# Funksjon for å beregne bruddpunkter
# 🔍 Beregn bruddpunkter for ett segment
get_breakpoints <- function(data, group_name) {
  df <- data %>%
    filter(segment == group_name) %>%
    group_by(week) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(week) %>%
    mutate(week_num = as.integer(difftime(week, min(week), units = "weeks")))
  
  # Lineær modell og segmentert modell med 2 brudd
  lm_base <- lm(n ~ week_num, data = df)
  seg_model <- segmented(lm_base, seg.Z = ~week_num, npsi = 2)
  
  # Ekstraher bruddpunktene
  breakpoints <- round(seg_model$psi[, "Est."])
  tibble(segment = group_name, breakpoint = breakpoints)
}


# Kjør for valgte segmenter
segments_to_check <- c("Tesla", "VW ID", "Skoda Enyaq", "Electric vehicles")

breakpoint_data <- map_dfr(segments_to_check, ~get_breakpoints(market_data, .x)) %>%
  arrange(segment, breakpoint)  # Viktig for linjene



# Visualiser bruddpunktene
ggplot(breakpoint_data, aes(x = breakpoint, y = segment, color = segment, group = segment)) +
  geom_line(linewidth = 1.2) +        # 🔗 Koble punktene
  geom_point(size = 4) +              # 🔘 Vis punktene
  scale_x_continuous(
    name = "Estimated breakpoint (week number from July 1, 2024)",
    limits = c(1, 40),                # 🔁 Starter fra uke 1
    breaks = seq(1, 40, 2)
  ) +
  scale_color_manual(values = c(
    "Tesla" = "#E82127",
    "VW ID" = "#1A93D9",
    "Skoda Enyaq" = "#F9A825",
    "Electric vehicles" = "#FFDD57"
  )) +
  labs(
    title = "Timing of structural shifts in listing volume",
    subtitle = "Segmented regression – Two breakpoints per segment",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    axis.text = element_text(color = "white"),
    text = element_text(color = "white"),
    legend.position = "none"
  )

ggsave(
  filename = "Timing of structural shifts in listing volume.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)



# Funksjon for tabellgrunnlag: extract_segment_model_stats()
extract_segment_model_stats <- function(data, group_name) {
  df <- data %>%
    filter(segment == group_name) %>%
    group_by(week) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(week) %>%
    mutate(week_num = as.integer(difftime(week, min(week), units = "weeks")))
  
  lm_base <- lm(n ~ week_num, data = df)
  seg_model <- segmented(lm_base, seg.Z = ~week_num, npsi = 2)
  
  breakpoints <- round(seg_model$psi[, "Est."])
  r_squared <- summary(seg_model)$r.squared
  rse <- summary(seg_model)$sigma
  
  tibble(
    Segment = group_name,
    Breakpoints = paste(breakpoints, collapse = ", "),
    `R²` = round(r_squared, 2),
    `Residual Std. Error` = round(rse, 2)
  )
}


# Kjør for alle segmenter og lag tabellen
segments_to_check <- c("Tesla", "VW ID", "Skoda Enyaq", "Electric vehicles")

breakpoint_summary <- map_dfr(segments_to_check, ~extract_segment_model_stats(market_data, .x))

# Vis som tabell
breakpoint_summary

#  R-kode for å lagre breakpoint_summary som PNG-tabell
# 🎨 Tilpass mørkt tema
# 🎨 Tilpass mørkt og skarpere kontrast
ttheme_dark <- ttheme_minimal(
  core = list(
    fg_params = list(col = "white", fontsize = 11),
    bg_params = list(fill = "#2b2b2b", col = "#444444")
  ),
  colhead = list(
    fg_params = list(col = "white", fontface = "bold", fontsize = 12),
    bg_params = list(fill = "#3b3b3b", col = "#555555")
  )
)

# 📐 Lag grafisk tabellobjekt
table_plot <- tableGrob(breakpoint_summary, rows = NULL, theme = ttheme_dark)

# 🖼️ Mørk bakgrunn bak hele figuren
bg <- rectGrob(gp = gpar(fill = "#1e1e1e", col = NA))

# 💾 Lagre som PNG
png("Segmented_regression_summary_table.png", width = 800, height = 300, res = 150)
grid.draw(bg)
grid.draw(table_plot)
dev.off()


# Korrelasjon mellom medianpris og volum per uke
# 🎯 Segmenter som skal analyseres
segments <- c("Tesla", "VW ID", "Skoda Enyaq", "Electric vehicles", "Fossil vehicles")

# 🔁 Funksjon for korrelasjon per segment
correlation_by_segment <- function(segment_name) {
  df <- market_data %>%
    filter(segment == segment_name) %>%
    group_by(week) %>%
    summarise(
      median_price = median(Price, na.rm = TRUE),
      volume = n(),
      .groups = "drop"
    )
  
  test <- cor.test(df$volume, df$median_price, method = "pearson")
  
  tibble(
    Segment = segment_name,
    `Pearson correlation` = round(test$estimate, 3),
    `P-value` = signif(test$p.value, 3)
  )
}

segments <- c("Tesla", "VW ID", "Skoda Enyaq", "Electric vehicles", "Fossil vehicles")


# 🔁 Kjør for alle segmenter
breakpoint_correlation <- map_dfr(segments, correlation_by_segment)

# 📊 Vis som tabell
breakpoint_correlation



# 📐 Rydd opp i visning av p-verdier
breakpoint_correlation_fmt <- breakpoint_correlation %>%
  mutate(
    `P-value` = format(round(`P-value`, 4), nsmall = 4)
  )

# 🎨 Mørkt tema
ttheme_dark <- ttheme_minimal(
  core = list(
    fg_params = list(col = "white", fontsize = 11),
    bg_params = list(fill = "#2b2b2b", col = "#444444")
  ),
  colhead = list(
    fg_params = list(col = "white", fontface = "bold", fontsize = 12),
    bg_params = list(fill = "#3b3b3b", col = "#555555")
  )
)

# 🖼️ Tabell som grafisk objekt
table_plot <- tableGrob(breakpoint_correlation_fmt, rows = NULL, theme = ttheme_dark)
bg <- rectGrob(gp = gpar(fill = "#1e1e1e", col = NA))

# 💾 Lagre som PNG
png("Correlation_price_vs_volume_dark_clean.png", width = 850, height = 300, res = 150)
grid.draw(bg)
grid.draw(table_plot)
dev.off()





# scatterplot
# 🎯 Utvalg: kun de tre merkene
scatter_data <- market_data %>%
  filter(segment %in% c("Tesla", "VW ID", "Skoda Enyaq")) %>%
  group_by(segment, week) %>%
  summarise(
    volume = n(),
    median_price = median(Price, na.rm = TRUE),
    .groups = "drop"
  )

# 🎨 Scatterplot med trendlinjer
ggplot(scatter_data, aes(x = volume, y = median_price, color = segment)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = c(
    "Tesla" = "#E82127",
    "VW ID" = "#1A93D9",
    "Skoda Enyaq" = "#F9A825"
  )) +
  labs(
    title = "Volume vs. median retail price – Selected brands",
    subtitle = "Weekly listings (Retail only)",
    x = "Number of listings",
    y = "Median price (DKK)",
    color = "Segment"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    axis.text = element_text(color = "white"),
    text = element_text(color = "white"),
    legend.background = element_rect(fill = "#1e1e1e"),
    legend.key = element_rect(fill = "#1e1e1e")
  )

# 💾 Lagre figuren
ggsave(
  filename = "Scatterplot_volume_vs_medianprice_dark.png",
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)



# LOESS-prisgraf
# 📆 Politisk kontekst og juleperiode
events <- tibble(
  label = c("Musk backs Trump", "Trump elected", "Trump inaugurated"),
  date = as.POSIXct(c("2024-07-13", "2024-11-05", "2025-01-15"))
)
holiday_start <- as.POSIXct("2024-12-20")
holiday_end   <- as.POSIXct("2025-01-05")

# 📊 Klargjør medianpris per uke per segment – nå inkl. Skoda Enyaq
loess_data <- market_data %>%
  filter(segment %in% c("Tesla", "VW ID", "Skoda Enyaq", "Electric vehicles", "Fossil vehicles")) %>%
  group_by(segment, week) %>%
  summarise(median_price = median(Price, na.rm = TRUE), .groups = "drop")

# 🎨 Visualisering med LOESS-smoothing
ggplot(loess_data, aes(x = week, y = median_price, color = segment)) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.2) +
  annotate("rect",
           xmin = holiday_start, xmax = holiday_end,
           ymin = -Inf, ymax = Inf,
           fill = "gray30", alpha = 0.3) +
  geom_vline(data = events, aes(xintercept = date),
             linetype = "dashed", color = "white") +
  geom_text(data = events, aes(x = date, y = Inf, label = label),
            angle = 90, vjust = -0.5, hjust = 1.1,
            size = 3.5, color = "white", inherit.aes = FALSE) +
  scale_color_manual(values = c(
    "Tesla" = "#E82127",
    "VW ID" = "#1A93D9",
    "Skoda Enyaq" = "#F9A825",
    "Electric vehicles" = "#FFDD57",
    "Fossil vehicles" = "#FF6B6B"
  )) +
  labs(
    title = "LOESS-smoothed median retail price",
    subtitle = "Weekly prices by segment – Retail only",
    x = "Week", y = "DKK", color = "Segment"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    axis.text = element_text(color = "white"),
    text = element_text(color = "white"),
    legend.background = element_rect(fill = "#1e1e1e"),
    legend.key = element_rect(fill = "#1e1e1e")
  )

# 💾 Lagre grafen
ggsave(
  filename = "LOESS_median_price_trends_with_Enyaq_dark.png",
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)


# ------------------------------------
# appendiks prisvolatilitet etter Musk støtter Trump

# 📊 Klargjør ukentlig medianpris
price_line_data <- market_data %>%
  filter(segment %in% c("Tesla", "VW ID", "Skoda Enyaq")) %>%
  group_by(segment, week) %>%
  summarise(median_price = median(Price, na.rm = TRUE), .groups = "drop")

# 📈 Linjeplot
ggplot(price_line_data, aes(x = week, y = median_price, color = segment)) +
  geom_line(linewidth = 1.2) +
  annotate("rect",
           xmin = as.POSIXct("2024-12-20"), xmax = as.POSIXct("2025-01-05"),
           ymin = -Inf, ymax = Inf,
           fill = "gray30", alpha = 0.3) +
  geom_vline(data = events, aes(xintercept = date),
             linetype = "dashed", color = "white") +
  geom_text(data = events, aes(x = date, y = Inf, label = label),
            angle = 90, vjust = -0.5, hjust = 1.1,
            size = 3.5, color = "white", inherit.aes = FALSE) +
  scale_color_manual(values = c(
    "Tesla" = "#E82127",
    "VW ID" = "#1A93D9",
    "Skoda Enyaq" = "#F9A825"
  )) +
  labs(
    title = "Weekly median retail price – Tesla, VW ID, Skoda Enyaq",
    subtitle = "Raw price levels per week (Retail only)",
    x = "Week", y = "Median price (DKK)", color = "Segment"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    axis.text = element_text(color = "white"),
    text = element_text(color = "white"),
    legend.background = element_rect(fill = "#1e1e1e"),
    legend.key = element_rect(fill = "#1e1e1e")
  )

# 💾 Lagre figuren
ggsave(
  filename = "Median_price_weekly_lineplot_dark.png",
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)




# 🔁 Duplikater ny sjekk (for metodekapittelet)
# 📊 Marker duplikater basert på nøkkelfelt
raw_data_dedup <- raw_data %>%
  mutate(
    week = floor_date(dmy_hms(CreatedDate), "week")
  ) %>%
  filter(PriceType == "RetailPrice", !is.na(Price), Price >= 10000) %>%
  mutate(dup_id = paste(make, model, variant, year, zipcode, Price, sep = "_")) %>%
  group_by(dup_id, week) %>%
  slice(1) %>%
  ungroup()

# 📊 Ukentlig volum
volume_with_dups <- raw_data %>%
  filter(PriceType == "RetailPrice", !is.na(Price), Price >= 10000) %>%
  mutate(week = floor_date(dmy_hms(CreatedDate), "week")) %>%
  count(week) %>%
  mutate(dataset = "With duplicates")

volume_without_dups <- raw_data_dedup %>%
  count(week) %>%
  mutate(dataset = "Without duplicates")

volume_compare <- bind_rows(volume_with_dups, volume_without_dups)

volume_compare_filtered <- volume_compare %>%
  filter(week >= as.Date("2024-01-01"))

# 🎨 Visualiser forskjellen
ggplot(volume_compare_filtered, aes(x = week, y = n, color = dataset)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c(
    "With duplicates" = "#E57373",
    "Without duplicates" = "#64FFDA"
  )) +
  labs(
    title = "Weekly listing volume – With vs Without duplicates",
    subtitle = "From Jan 1, 2024 · Duplicates = identical make/model/variant/year/zipcode/price",
    x = "Week", y = "Number of listings", color = "Dataset"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    axis.text = element_text(color = "white"),
    text = element_text(color = "white"),
    legend.background = element_rect(fill = "#1e1e1e"),
    legend.key = element_rect(fill = "#1e1e1e")
  )

# 💾 Lagre som figur
ggsave(
  filename = "Weekly_listing_volume_duplicates_comparison_dark.png",
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)



# ------------------------------------
# xtra ish

# 📦 Forutsetter scatter_data fra tidligere (volum + median_price per uke)
scatter_data <- market_data %>%
  filter(segment %in% c("Tesla", "VW ID", "Skoda Enyaq")) %>%
  group_by(segment, week) %>%
  summarise(
    volume = n(),
    median_price = median(Price, na.rm = TRUE),
    .groups = "drop"
  )

# 🔍 Scatterplot med LOESS
ggplot(scatter_data, aes(x = volume, y = median_price, color = segment)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.2, span = 1) +
  scale_color_manual(values = c(
    "Tesla" = "#E82127",
    "VW ID" = "#1A93D9",
    "Skoda Enyaq" = "#F9A825"
  )) +
  labs(
    title = "Volume vs. median retail price (LOESS)",
    subtitle = "Weekly listings – Non-linear price response",
    x = "Number of listings",
    y = "Median price (DKK)",
    color = "Segment"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    axis.text = element_text(color = "white"),
    text = element_text(color = "white"),
    legend.background = element_rect(fill = "#1e1e1e"),
    legend.key = element_rect(fill = "#1e1e1e")
  )

# 💾 Lagre LOESS scatter
ggsave(
  filename = "Scatterplot_volume_vs_price_LOESS_dark.png",
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)







# 📊 Histogram over priser (log-skala for å se ekstremverdier)
ggplot(market_data, aes(x = Price)) +
  geom_histogram(bins = 100, fill = "#00c0c7", color = "white") +
  scale_x_log10(labels = scales::comma) +  # ← her er endringen
  labs(
    title = "Distribution of Retail Prices (log scale)",
    subtitle = "RetailPrice ≥ 10 000 DKK – Outliers become visible",
    x = "Price (log scale)", y = "Number of listings"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    axis.text = element_text(color = "white"),
    text = element_text(color = "white")
  )


# 📦 Boxplot per segment
ggplot(market_data, aes(x = segment, y = Price, fill = segment)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c(
    "Tesla" = "#E82127",
    "VW ID" = "#1A93D9",
    "Skoda Enyaq" = "#F9A825",
    "Electric vehicles" = "#FFDD57",
    "Fossil vehicles" = "#FF6B6B"
  )) +
  labs(
    title = "Boxplot of Retail Prices by Segment",
    subtitle = "Outliers shown in red",
    x = "Segment", y = "Retail Price (DKK)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    axis.text = element_text(color = "white"),
    text = element_text(color = "white"),
    legend.position = "none"
  )


# --------------------------

# 📈 1. Totalmarkedet – LOESS uten punkter, med råvolum svakt i bakgrunnen

# 📊 Data for total, fossil og electric
volume_total_loess <- volume_combined %>%
  mutate(week = as.POSIXct(week))

# 🎨 Plot
ggplot(volume_total_loess, aes(x = week)) +
  geom_line(aes(y = n, color = segment), linewidth = 0.8, alpha = 0.3) +  # råvolum, tynn og lys
  geom_smooth(aes(y = n, color = segment), method = "loess", span = 0.6, se = FALSE, linewidth = 1.5) + # LOESS-smooth
  annotate("rect", xmin = holiday_start, xmax = holiday_end, ymin = -Inf, ymax = Inf, fill = "gray30", alpha = 0.3) +
  geom_vline(data = events, aes(xintercept = date), linetype = "dashed", color = "white") +
  geom_text(data = events, aes(x = date, y = Inf, label = label),
            angle = 90, vjust = -0.5, hjust = 1.1, size = 3.5, color = "white", inherit.aes = FALSE) +
  scale_color_manual(values = c(
    "Electric vehicles" = "#FFDD57",
    "Fossil vehicles" = "#FF6B6B",
    "Total market" = "#00c0c7"
  )) +
  labs(
    title = "Weekly retail volume – Total vs Electric vs Fossil (LOESS smoothed)",
    subtitle = "Retail listings per week · Smoothed volume trends",
    x = "Week", y = "Number of listings", color = "Segment"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.background = element_rect(fill = "#1e1e1e"),
    legend.key = element_rect(fill = "#1e1e1e")
  )

# 💾 Lagre grafen
ggsave(
  filename = "Weekly retail volume – Total vs Electric vs Fossil (LOESS dark).png",
  width = 10, height = 6, dpi = 300, bg = "#1e1e1e"
)




# 📈 2. Brand-segmentene – Tesla vs VW ID vs Skoda Enyaq – LOESS uten punkter
# 📊 Data for Tesla, VW ID, Skoda Enyaq
volume_brand_loess <- market_data %>%
  filter(segment %in% c("Tesla", "VW ID", "Skoda Enyaq")) %>%
  count(week, segment) %>%
  mutate(week = as.POSIXct(week))

# 🎨 Plot
ggplot(volume_brand_loess, aes(x = week)) +
  geom_line(aes(y = n, color = segment), linewidth = 0.8, alpha = 0.3) +  # råvolum, tynn og lys
  geom_smooth(aes(y = n, color = segment), method = "loess", span = 0.6, se = FALSE, linewidth = 1.5) + # LOESS-smooth
  annotate("rect", xmin = holiday_start, xmax = holiday_end, ymin = -Inf, ymax = Inf, fill = "gray30", alpha = 0.3) +
  geom_vline(data = events, aes(xintercept = date), linetype = "dashed", color = "white") +
  geom_text(data = events, aes(x = date, y = Inf, label = label),
            angle = 90, vjust = -0.5, hjust = 1.1, size = 3.5, color = "white", inherit.aes = FALSE) +
  scale_color_manual(values = c(
    "Tesla" = "#E82127",
    "VW ID" = "#1A93D9",
    "Skoda Enyaq" = "#F9A825"
  )) +
  labs(
    title = "Weekly retail volume – Tesla vs VW ID vs Skoda Enyaq (LOESS smoothed)",
    subtitle = "Retail listings per week · Smoothed volume trends",
    x = "Week", y = "Number of listings", color = "Brand"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.background = element_rect(fill = "#1e1e1e"),
    legend.key = element_rect(fill = "#1e1e1e")
  )

# 💾 Lagre grafen
ggsave(
  filename = "Weekly retail volume – Tesla vs VW ID vs Skoda Enyaq (LOESS dark).png",
  width = 10, height = 6, dpi = 300, bg = "#1e1e1e"
)
