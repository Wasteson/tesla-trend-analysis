library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(tidyr)
library(ggrepel)
library(stringr)
library(knitr)
library(zoo)  # for rollmean
library(segmented)
library(purrr)


# 📁 Laster data
y <- read_excel("~/Desktop/wasteson/TrackSights/data/bilbasen/trumpmusktesla/Bilbasen biler 1903 2024-kopi.xlsx")

# 🕒 Formatterer datoer
y <- y %>%
  mutate(
    date = dmy_hms(CreatedDate),
    month = floor_date(date, "month")
  )

# 📆 Avgrensning
start_date <- as.Date("2024-04-01")
end_date <- as.Date("2025-03-14")

# 🎯 Datasett: kun retailannonser i valgt periode
market_data <- y %>%
  filter(PriceType == "RetailPrice", date >= start_date & date <= end_date) %>%
  mutate(
    week = floor_date(date, "week"),
    fuel_group = case_when(
      str_to_lower(FuelType) %in% c("el", "electric") ~ "Electric",
      TRUE ~ "Fossil"
    ),
    segment = case_when(
      make == "Tesla" ~ "Tesla",
      make == "VW" & str_detect(model, "^ID\\.") ~ "VW ID",
      fuel_group == "Electric" ~ "Electric vehicles",
      fuel_group == "Fossil" ~ "Fossil vehicles",
      TRUE ~ "Other"
    )
  )

# 📊 Beregn ukentlige volumer
volume_by_segment <- market_data %>%
  filter(segment %in% c("Tesla", "VW ID", "Electric vehicles", "Fossil vehicles")) %>%
  count(week, segment)

volume_total <- market_data %>%
  count(week) %>%
  mutate(segment = "Total market")

# 🔁 Samlet datasett
volume_all <- bind_rows(volume_by_segment, volume_total)

# 📍 Politisk event-data
events <- tibble::tibble(
  label = c("Musk backs Trump", "Trump elected", "Trump inaugurated"),
  date = as.POSIXct(c("2024-07-13", "2024-11-05", "2025-01-15"))
)

# 🔢 Indekser volum (Index = 100 ved første uke per segment)
volume_indexed <- volume_all %>%
  group_by(segment) %>%
  arrange(week) %>%
  mutate(index = (n / first(n)) * 100) %>%
  ungroup()

# 🎨 Darkmode: relativ utvikling per segment
ggplot(volume_indexed, aes(x = week, y = index, color = segment)) +
  geom_line(size = 1.2) +
  geom_vline(data = events, aes(xintercept = date),
             linetype = "dashed", color = "white") +
  geom_text(data = events, aes(x = date, y = Inf, label = label),
            angle = 90, vjust = -0.5, hjust = 1.1,
            size = 3.5, color = "white", inherit.aes = FALSE) +
  scale_color_manual(values = c(
    "Total market" = "#00c0c7",
    "Electric vehicles" = "#FFDD57",
    "Fossil vehicles" = "#FF6B6B",
    "Tesla" = "#6BE7C2",
    "VW ID" = "#8BC34A"
  )) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Indexed weekly volume – Used car listings (Retail only)",
    subtitle = "Relative growth since Apr 2024 (Index = 100)",
    x = "Week",
    y = "Volume index (100 = week 1)",
    color = "Segment"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.background = element_rect(fill = "#1e1e1e"),
    legend.key = element_rect(fill = "#1e1e1e")
  )



#Forarbeid – for separate grafer
# Datoavgrensning
start_date <- as.Date("2024-04-01")
end_date <- as.Date("2025-03-14")

# Event-data
events <- tibble::tibble(
  label = c("Musk backs Trump", "Trump elected", "Trump inaugurated"),
  date = as.POSIXct(c("2024-07-13", "2024-11-05", "2025-01-15"))
)

# Filter og segmentering
filtered_data <- y %>%
  filter(PriceType == "RetailPrice", date >= start_date & date <= end_date) %>%
  mutate(
    week = floor_date(date, "week"),
    fuel_group = case_when(
      str_to_lower(FuelType) %in% c("el", "electric") ~ "Electric",
      TRUE ~ "Fossil"
    ),
    segment = case_when(
      make == "Tesla" ~ "Tesla",
      make == "VW" & str_detect(model, "^ID\\.") ~ "VW ID",
      fuel_group == "Electric" ~ "Electric vehicles",
      fuel_group == "Fossil" ~ "Fossil vehicles",
      TRUE ~ "Other"
    )
  )


# Graf: Total, Electric, Fossil
# Filtrer og indeksér
# Først: Lag separate datasett
volume_segment <- filtered_data %>%
  filter(segment %in% c("Electric vehicles", "Fossil vehicles")) %>%
  count(week, segment)

volume_total <- filtered_data %>%
  count(week) %>%
  mutate(segment = "Total market")

# Kombiner
volume_primary <- bind_rows(volume_segment, volume_total)

# Så: Beregn indeks for alle segmenter korrekt
volume_primary <- volume_primary %>%
  group_by(segment) %>%
  arrange(week) %>%
  mutate(index = (n / first(n)) * 100) %>%
  ungroup()

# Plot
ggplot(volume_primary, aes(x = week, y = index, color = segment)) +
  geom_line(size = 1.3) +
  geom_vline(data = events, aes(xintercept = date), linetype = "dashed", color = "white") +
  geom_text(data = events, aes(x = date, y = Inf, label = label),
            angle = 90, vjust = -0.5, hjust = 1.1,
            size = 3.5, color = "white", inherit.aes = FALSE) +
  scale_color_manual(values = c(
    "Total market" = "#00c0c7",
    "Electric vehicles" = "#FFDD57",
    "Fossil vehicles" = "#FF6B6B"
  )) +
  labs(
    title = "Indexed listing volume – Market overview",
    subtitle = "Total vs Electric vs Fossil (Retail only, Index = 100 in April 2024)",
    x = "Week",
    y = "Volume index",
    color = "Segment"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e", color = NA),
    panel.background = element_rect(fill = "#1e1e1e", color = NA),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.background = element_rect(fill = "#1e1e1e"),
    legend.key = element_rect(fill = "#1e1e1e")
  )

# ggsave(
  filename = "indexed_volume_market_overview_dark.png",
  plot = last_plot(),  # evt. erstatt med objekt hvis du har lagret plottet som `p1` eller lignende
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)


# Graf: Tesla vs VW ID
volume_focus <- filtered_data %>%
  filter(segment %in% c("Tesla", "VW ID")) %>%
  count(week, segment) %>%
  group_by(segment) %>%
  arrange(week) %>%
  mutate(index = (n / first(n)) * 100) %>%
  ungroup()

ggplot(volume_focus, aes(x = week, y = index, color = segment)) +
  geom_line(size = 1.3) +
  geom_vline(data = events, aes(xintercept = date), linetype = "dashed", color = "white") +
  geom_text(data = events, aes(x = date, y = Inf, label = label),
            angle = 90, vjust = -0.5, hjust = 1.1,
            size = 3.5, color = "white", inherit.aes = FALSE) +
  scale_color_manual(values = c(
    "Tesla" = "#6BE7C2",
    "VW ID" = "#8BC34A"
  )) +
  labs(
    title = "Indexed listing volume – Tesla vs VW ID",
    subtitle = "Retail listings, relative to Apr 2024 = 100",
    x = "Week",
    y = "Volume index",
    color = "Brand"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e", color = NA),
    panel.background = element_rect(fill = "#1e1e1e", color = NA),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.background = element_rect(fill = "#1e1e1e"),
    legend.key = element_rect(fill = "#1e1e1e")
  )

# ggsave(
filename = "indexed_volume_tesla_vs_vw_id_dark.png",
plot = last_plot(),
width = 10,
height = 6,
dpi = 300,
bg = "#1e1e1e"
)


# Segmentert regresjon for Tesla-volum
# 📌 Filtrer Tesla fra det samlede volumet og beregn ukeindeks og indeksverdi
tesla_weekly <- volume_all %>%
  filter(segment == "Tesla") %>%
  arrange(week) %>%
  mutate(
    week_num = as.numeric(difftime(week, min(week), units = "weeks")),
    index = (n / first(n)) * 100
  )

# 📈 Lineær modell og segmentert regresjon
tesla_lm <- lm(index ~ week_num, data = tesla_weekly)

# Vi gir et initialt estimat for bruddpunkt, f.eks. uke 14
tesla_segmod <- segmented(tesla_lm, seg.Z = ~week_num, psi = list(week_num = 14))

# Ekstraher estimert bruddpunkt
breakpoint <- round(tesla_segmod$psi[1, "Est."])

# 📊 Visualisering i dark mode
ggplot(tesla_weekly, aes(x = week_num, y = index)) +
  geom_line(color = "#6BE7C2", size = 1.2) +
  geom_vline(xintercept = breakpoint, linetype = "dashed", color = "white") +
  annotate("text", x = breakpoint, y = max(tesla_weekly$index),
           label = paste("Estimated breakpoint\nWeek", breakpoint),
           color = "white", angle = 90, vjust = -0.5, hjust = 1.1, size = 3.5) +
  labs(
    title = "Segmented trend in Tesla listing volume",
    subtitle = "Index (week 1 = 100)",
    x = "Week number",
    y = "Volume index"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white")
  )

# ggsave(
filename = "Segmented trend in Tesla listing volume.png",
plot = last_plot(),  # evt. erstatt med objekt hvis du har lagret plottet som `p1` eller lignende
width = 10,
height = 6,
dpi = 300,
bg = "#1e1e1e"
)

# Segmentert regresjon for VW ID – med bruddpunktestimat
# 📦 1. Filtrer til VW ID og legg til uketall
vwid_seg <- volume_all %>%
  filter(segment == "VW ID") %>%
  arrange(week) %>%
  mutate(
    week_num = as.numeric(difftime(week, min(week), units = "weeks")),
    index = (n / first(n)) * 100
  )

# 📈 2. Lineær modell før segmentering
vwid_lm <- lm(index ~ week_num, data = vwid_seg)

# ✂️ 3. Segmentert regresjon – med startforslag uke 14 (samme som Tesla)
vwid_segmod <- segmented(vwid_lm, seg.Z = ~week_num, psi = list(week_num = 14))

# 📌 4. Ekstraher bruddpunkt
vwid_breakpoint <- round(summary(vwid_segmod)$psi[1, "Est."])

# 🎨 5. Visualisering i darkmode
ggplot(vwid_seg, aes(x = week_num, y = index)) +
  geom_line(color = "#8BC34A", size = 1.2) +
  geom_vline(xintercept = vwid_breakpoint, linetype = "dashed", color = "white") +
  annotate("text", x = vwid_breakpoint, y = max(vwid_seg$index, na.rm = TRUE),
           label = paste("Estimated breakpoint\nWeek", vwid_breakpoint),
           color = "white", angle = 90, vjust = -0.5, hjust = 1.1, size = 3.5) +
  labs(
    title = "Segmented trend in VW ID listing volume",
    subtitle = "Index (week 1 = 100)",
    x = "Week number",
    y = "Volume index"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white")
  )

# ggsave(
filename = "Segmented trend in VW ID listing volume.png",
plot = last_plot(),  # evt. erstatt med objekt hvis du har lagret plottet som `p1` eller lignende
width = 10,
height = 6,
dpi = 300,
bg = "#1e1e1e"
)


# Segmentert regresjon og sammenligning: Tesla vs VW ID
# 📍 Beregn uke-nummer (felles referanse for begge)
indexed_data <- volume_indexed %>%
  filter(segment %in% c("Tesla", "VW ID")) %>%
  group_by(segment) %>%
  arrange(week) %>%
  mutate(week_num = as.numeric(difftime(week, min(week), units = "weeks"))) %>%
  ungroup()

# 🚗 Segmentert modell for Tesla
tesla_data <- indexed_data %>% filter(segment == "Tesla")
tesla_lm <- lm(index ~ week_num, data = tesla_data)
tesla_seg <- segmented(tesla_lm, seg.Z = ~week_num, psi = list(week_num = 14))

# 🚙 Segmentert modell for VW ID
vw_data <- indexed_data %>% filter(segment == "VW ID")
vw_lm <- lm(index ~ week_num, data = vw_data)
vw_seg <- segmented(vw_lm, seg.Z = ~week_num, psi = list(week_num = 14))

# 📊 Hent ut resultater
extract_results <- function(model, label) {
  bp <- model$psi[1, "Est."]
  slopes <- slope(model)$week_num[, 1]
  tibble::tibble(
    Segment = label,
    `Estimated breakpoint (week)` = round(bp, 1),
    `Slope before` = round(slopes[1], 2),
    `Slope after` = round(slopes[2], 2)
  )
}

result_table <- bind_rows(
  extract_results(tesla_seg, "Tesla"),
  extract_results(vw_seg, "VW ID")
)

# 📋 Vis i pen tabell
result_table %>%
  kable(
    caption = "Segmented regression results – Tesla vs VW ID",
    col.names = c("Segment", "Estimated breakpoint (week)", "Slope before", "Slope after")
  )


volume_all %>%
  filter(segment == "Electric vehicles") %>%
  left_join(filtered_data, by = c("week", "segment")) %>%
  count(make, model, sort = TRUE)


# Kombiner ukentlig volum med detaljer for modellidentifikasjon
ev_model_counts <- volume_all %>%
  filter(segment == "Electric vehicles") %>%
  left_join(
    dplyr::select(filtered_data, week, segment, make, model),
    by = c("week", "segment")
  ) %>%
  count(make, model, sort = TRUE)


# Sjekk resultat
head(ev_model_counts, 10)


# analyse for Skoda Enyaq
enyaq_weekly <- filtered_data %>%
  filter(make == "Skoda", model == "Enyaq") %>%
  count(week) %>%
  arrange(week) %>%
  mutate(
    index = (n / first(n)) * 100,
    week_num = as.numeric(difftime(week, min(week), units = "weeks"))
  )

enyaq_lm <- lm(index ~ week_num, data = enyaq_weekly)

enyaq_segmod <- segmented(enyaq_lm, seg.Z = ~week_num, psi = list(week_num = 14))

summary(enyaq_segmod)

enyaq_break <- enyaq_segmod$psi[1, "Est."]

enyaq_slope_pre  <- slope(enyaq_segmod)$week_num[1]
enyaq_slope_post <- slope(enyaq_segmod)$week_num[2]

ggplot(enyaq_weekly, aes(x = week_num, y = index)) +
  geom_line(color = "#FFDD57", size = 1.2) +
  geom_vline(xintercept = enyaq_break, linetype = "dashed", color = "white") +
  annotate("text", x = enyaq_break, y = max(enyaq_weekly$index, na.rm = TRUE),
           label = paste("Estimated breakpoint\nWeek", round(enyaq_break, 1)),
           color = "white", angle = 90, vjust = -0.5, hjust = 1.1, size = 3.5) +
  labs(
    title = "Segmented trend in Skoda Enyaq listing volume",
    subtitle = "Index (week 1 = 100)",
    x = "Week number",
    y = "Volume index"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white")
  )

# ggsave(
  filename = "Segmented trend in Skoda Enyaq listing volume.png",
  plot = last_plot(),  # evt. erstatt med objekt hvis du har lagret plottet som `p1` eller lignende
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)



# Hente ut tall til sammenligning
# 🔢 Hent ut helninger for Enyaq
enyaq_slope_pre  <- slope(enyaq_segmod)$week_num[1]
enyaq_slope_post <- slope(enyaq_segmod)$week_num[2]

# Sikre at kolonnenavnene matcher med de tidligere
names(result_table)


# 🧱 Legg til i resultattabellen
result_table <- bind_rows(
  result_table,
  tibble(
    Segment = "Skoda Enyaq",
    `Estimated breakpoint (week)` = round(enyaq_break, 1),
    `Slope before` = round(enyaq_slope_pre, 2),
    `Slope after`  = round(enyaq_slope_post, 2)
  )
)

# 📋 Vis som pent formatert tabell
result_table %>%
  kable(
    caption = "Segmented regression results – including Skoda Enyaq",
    col.names = c("Segment", "Estimated breakpoint (week)", "Slope before", "Slope after")
  )


# Forbered ukentlig indeksert datasett for elbiler
ev_weekly <- volume_all %>%
  filter(segment == "Electric vehicles") %>%
  arrange(week) %>%
  mutate(
    week_num = as.numeric(difftime(week, min(week), units = "weeks")),
    index = (n / first(n)) * 100
  )

# lineær og segmentert regresjon
ev_lm <- lm(index ~ week_num, data = ev_weekly)

ev_segmod <- segmented(ev_lm, seg.Z = ~week_num, psi = list(week_num = 14))
summary(ev_segmod)

ev_break <- ev_segmod$psi[1, "Est."]
ev_slope_pre  <- slope(ev_segmod)$week_num[1]
ev_slope_post <- slope(ev_segmod)$week_num[2]

# Legg til i sammenligningstabellen
result_table <- bind_rows(
  result_table,
  tibble(
    Segment = "Electric vehicles",
    `Estimated breakpoint (week)` = round(ev_break, 1),
    `Slope before` = round(ev_slope_pre, 2),
    `Slope after`  = round(ev_slope_post, 2)
  )
)

#Vis oppdatert tabell
result_table %>%
  kable(
    caption = "Segmented regression results – including Electric vehicles",
    col.names = c("Segment", "Estimated breakpoint (week)", "Slope before", "Slope after")
  )

# Visualisering i mørk stil
ggplot(ev_weekly, aes(x = week_num, y = index)) +
  geom_line(color = "#FFDD57", size = 1.2) +
  geom_vline(xintercept = ev_break, linetype = "dashed", color = "white") +
  annotate("text", x = ev_break, y = max(ev_weekly$index, na.rm = TRUE),
           label = paste("Estimated breakpoint\nWeek", round(ev_break, 1)),
           color = "white", angle = 90, vjust = -0.5, hjust = 1.1, size = 3.5) +
  labs(
    title = "Segmented trend in Electric vehicles listing volume",
    subtitle = "Index (week 1 = 100)",
    x = "Week number",
    y = "Volume index"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white")
  )

# ggsave(
  filename = "Segmented trend in Electric vehicles listing volume.png",
  plot = last_plot(),  # evt. erstatt med objekt hvis du har lagret plottet som `p1` eller lignende
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)



# 📦 Data: totalmarkedet ukentlig
total_weekly <- volume_all %>%
  filter(segment == "Total market") %>%
  arrange(week) %>%
  mutate(
    week_num = as.numeric(difftime(week, min(week), units = "weeks")),
    index = (n / first(n)) * 100
  )

# 📈 Lineær modell og segmentert regresjon
total_lm <- lm(index ~ week_num, data = total_weekly)
total_segmod <- segmented(total_lm, seg.Z = ~week_num, psi = list(week_num = 14))

# 📍 Hent ut bruddpunkt og helninger
total_break <- total_segmod$psi[1, "Est."]
total_slope_pre  <- slope(total_segmod)$week_num[1]
total_slope_post <- slope(total_segmod)$week_num[2]

# 📊 Visualisering
ggplot(total_weekly, aes(x = week_num, y = index)) +
  geom_line(color = "#00c0c7", size = 1.2) +
  geom_vline(xintercept = total_break, linetype = "dashed", color = "white") +
  annotate("text", x = total_break, y = max(total_weekly$index, na.rm = TRUE),
           label = paste("Estimated breakpoint\nWeek", round(total_break, 1)),
           color = "white", angle = 90, vjust = -0.5, hjust = 1.1, size = 3.5) +
  labs(
    title = "Segmented trend in total market volume",
    subtitle = "Index (week 1 = 100)",
    x = "Week number",
    y = "Volume index"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white")
  )

# 💾 Lagre som fil
# ggsave(
  filename = "Segmented trend in Total Market listing volume.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)

# 🧱 Legg til i sammenligningstabellen
result_table <- bind_rows(
  result_table,
  tibble(
    Segment = "Total market",
    `Estimated breakpoint (week)` = round(total_break, 1),
    `Slope before` = round(total_slope_pre, 2),
    `Slope after`  = round(total_slope_post, 2)
  )
)

# 📋 Vis oppdatert tabell
result_table %>%
  kable(
    caption = "Segmented regression results – including Total market",
    col.names = c("Segment", "Estimated breakpoint (week)", "Slope before", "Slope after")
  )



# visualisering som fremhever forskjellen i tidspunkt for bruddpunkt (vekststart) i volum for Tesla sammenlignet med de andre segmentene:
# 🔢 Bruddpunkt-data manuelt satt inn
break_data <- tibble::tibble(
  Segment = c("Tesla", "VW ID", "Skoda Enyaq", "Electric vehicles", "Total market"),
  Breakpoint = c(29.8, 37.8, 38.7, 38.0, 34.3)
)

# 📊 Visualisering
ggplot(break_data, aes(x = Breakpoint, y = reorder(Segment, Breakpoint))) +
  geom_segment(aes(x = 0, xend = Breakpoint, yend = Segment), color = "#444444", linetype = "dotted") +
  geom_point(color = "#00c0c7", size = 4) +
  geom_text(aes(label = paste0("Week ", round(Breakpoint, 1))), hjust = -0.1, color = "white", size = 3.5) +
  labs(
    title = "Estimated Breakpoints in Listing Volume",
    subtitle = "Tesla shows earlier shift in volume growth",
    x = "Week number (relative to April 2024)",
    y = NULL
  ) +
  xlim(0, max(break_data$Breakpoint) + 5) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e", color = NA),
    panel.background = element_rect(fill = "#1e1e1e", color = NA),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white")
  )

# ggsave(
  filename = "Estimated Breakpoints in Listing Volume.png",
  plot = last_plot(),  # evt. erstatt med objekt hvis du har lagret plottet som `p1` eller lignende
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)


# residualanalyse for Tesla – vi sjekker modellens fit og om det er noe systematisk som modellen ikke fanger
# 📦 Residualanalyse – Tesla segmentert modell

# 1. Ekstraher residualer og predikert verdi
tesla_resid <- tibble(
  week = tesla_weekly$week,
  fitted = fitted(tesla_segmod),
  residuals = residuals(tesla_segmod)
)

# 2. Plot residualer over tid
ggplot(tesla_resid, aes(x = week, y = residuals)) +
  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
  geom_line(color = "#6BE7C2", size = 1.2) +
  labs(
    title = "Residual plot – Segmented model for Tesla",
    subtitle = "Residuals over time (should be random if model fits well)",
    x = "Week",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white")
  )

# ggsave(
  filename = "Residual plot – Segmented model for Tesla.png",
  plot = last_plot(),  # evt. erstatt med objekt hvis du har lagret plottet som `p1` eller lignende
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)


# utvider modellen for Tesla til å estimere to breakpoints – altså tre trendperioder – 
# og sjekker om det gir en mer stabil og realistisk utvikling + penere residualer
# 📌 Lag uketall på nytt hvis du ikke allerede har det
tesla_data <- volume_all %>%
  filter(segment == "Tesla") %>%
  arrange(week) %>%
  mutate(
    week_num = as.numeric(difftime(week, min(week), units = "weeks")),
    index = (n / first(n)) * 100
  )

# 📈 Enkel lineærmodell
tesla_lm_multi <- lm(index ~ week_num, data = tesla_data)

# ✂️ Segmentert modell med TO bruddpunkt (startgjetninger: uke 20 og 35)
tesla_seg_multi <- segmented(tesla_lm_multi, seg.Z = ~week_num, psi = c(20, 35))

# 📊 Hent residualer
tesla_data$resid_multi <- residuals(tesla_seg_multi)

# 🗓️ Ekstraher bruddpunkt
breaks_multi <- round(tesla_seg_multi$psi[, "Est."], 1)

# 📉 Visualiser residualer
ggplot(tesla_data, aes(x = week, y = resid_multi)) +
  geom_line(color = "#6BE7C2", size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "white") +
  labs(
    title = "Residual plot – Extended segmented model for Tesla",
    subtitle = "Two breakpoints estimated – should better capture irregular shifts",
    x = "Week",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white")
  )

# ggsave(
  filename = "Residual plot – Extended segmented model for Tesla.png",
  plot = last_plot(),  # evt. erstatt med objekt hvis du har lagret plottet som `p1` eller lignende
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)


# segmentert trend med to bruddpunkt
# 🔁 Predikert verdi fra modellen
tesla_data$fit_multi <- fitted(tesla_seg_multi)

# 📊 Visualiser trend + bruddpunkt
ggplot(tesla_data, aes(x = week)) +
  geom_line(aes(y = index), color = "#6BE7C2", size = 1.2, alpha = 0.4) +  # Original data
  geom_line(aes(y = fit_multi), color = "#6BE7C2", size = 1.2) +          # Modelltilpasning
  geom_vline(xintercept = min(tesla_data$week) + weeks(breaks_multi),
             linetype = "dashed", color = "white") +
  annotate("text",
           x = min(tesla_data$week) + weeks(breaks_multi),
           y = max(tesla_data$index, na.rm = TRUE),
           label = paste("Breakpoint\nWeek", breaks_multi),
           angle = 90, vjust = -0.5, hjust = 1.1,
           color = "white", size = 3.5) +
  labs(
    title = "Segmented trend in Tesla listing volume (2 breakpoints)",
    subtitle = "Three growth periods – improved model fit",
    x = "Week",
    y = "Volume index (week 1 = 100)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white")
  )

# ggsave(
  filename = "Segmented trend in Tesla listing volume (2 breakpoints).png",
  plot = last_plot(),  # evt. erstatt med objekt hvis du har lagret plottet som `p1` eller lignende
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)


# Modellkvalitet – sammenligning av 1 vs. 2 breakpoints
# Sammenlign R² og residualavvik
# 📐 Modellkvalitet
compare_models <- tibble(
  Model = c("1 breakpoint", "2 breakpoints"),
  `Breakpoints (week)` = c(
    round(tesla_seg$psi[1, "Est."], 1),
    paste(round(breaks_multi, 1), collapse = ", ")
  ),
  `R-squared` = c(
    summary(tesla_seg)$r.squared,
    summary(tesla_seg_multi)$r.squared
  ),
  `Residual Std. Error` = c(
    summary(tesla_seg)$sigma,
    summary(tesla_seg_multi)$sigma
  )
)

# 📋 Pen tabell
compare_models %>%
  kable(
    caption = "Comparison of segmented regression models – Tesla volume",
    digits = 2,
    col.names = c("Model", "Breakpoint(s)", "R²", "Residual Std. Error")
  )


# segmentert regresjon med to bruddpunkt for VW ID, tilsvarende analysen vi gjorde for Tesla.
# Segmentert regresjon – VW ID (2 breakpoints)
# 📦 1. Filtrer til VW ID og beregn ukentlige volumer med indeks
vwid_weekly <- volume_all %>%
  filter(segment == "VW ID") %>%
  arrange(week) %>%
  mutate(
    week_num = as.numeric(difftime(week, min(week), units = "weeks")),
    index = (n / first(n)) * 100
  )

# 📈 2. Lineær modell
vwid_lm2 <- lm(index ~ week_num, data = vwid_weekly)

# ✂️ 3. Segmentert regresjon med to startestimater for bruddpunkt
vwid_segmod2 <- segmented(vwid_lm2, seg.Z = ~week_num, psi = c(20, 35))

# 🔍 4. Ekstraher bruddpunkt og helninger
vwid_breaks <- round(vwid_segmod2$psi[, "Est."], 1)
vwid_slopes <- round(slope(vwid_segmod2)$week_num[, 1], 2)
vwid_r2 <- summary(vwid_segmod2)$r.squared
vwid_resid <- summary(vwid_segmod2)$sigma

# 🧾 5. Vis oppsummeringstabell (legg gjerne til i samlet compare_models-tabell)
tibble(
  Model = "VW ID (2 breakpoints)",
  `Breakpoints` = paste(vwid_breaks, collapse = ", "),
  `R²` = round(vwid_r2, 2),
  `Residual Std. Error` = round(vwid_resid, 2)
)


# Visualisering
# 🔮 Forutsi verdier
vwid_weekly$fit <- predict(vwid_segmod2)

# 🎨 Plot
ggplot(vwid_weekly, aes(x = week_num, y = index)) +
  geom_line(color = "#8BC34A", size = 1.2) +
  geom_line(aes(y = fit), color = "white", size = 1.1, linetype = "solid") +
  geom_vline(xintercept = vwid_breaks, linetype = "dashed", color = "white") +
  annotate("text", x = vwid_breaks, y = max(vwid_weekly$index, na.rm = TRUE),
           label = paste("Breakpoint\nWeek", vwid_breaks),
           color = "white", angle = 90, vjust = -0.5, hjust = 1.1, size = 3.5) +
  labs(
    title = "Segmented trend in VW ID listing volume (2 breakpoints)",
    subtitle = "Three growth periods – testing improved model fit",
    x = "Week",
    y = "Volume index (week 1 = 100)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white")
  )


# 🎨 Visualisering
ggplot(vwid_weekly, aes(x = week_num, y = index)) +
  geom_line(color = "#8BC34A", size = 1.2) +
  geom_line(aes(y = fit), color = "white", size = 1.1, linetype = "solid") +
  geom_vline(xintercept = vwid_breaks, linetype = "dashed", color = "white") +
  annotate("text", x = vwid_breaks, y = max(vwid_weekly$index, na.rm = TRUE),
           label = paste("Breakpoint\nWeek", vwid_breaks),
           color = "white", angle = 90, vjust = -0.5, hjust = 1.1, size = 3.5) +
  labs(
    title = "Segmented trend in VW ID listing volume (2 breakpoints)",
    subtitle = "Three growth periods – testing improved model fit",
    x = "Week",
    y = "Volume index (week 1 = 100)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white")
  )

# ggsave(
  filename = "Segmented trend in VW ID listing volume (2 breakpoints).png",
  plot = last_plot(),  # evt. erstatt med objekt hvis du har lagret plottet som `p1` eller lignende
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)


# da kjører vi samme analyseoppsett på hele elbilmarkedet.
# Segmentert regresjon med to bruddpunkter for Electric vehicles
# 🔍 Filtrer ukentlig elbilvolum og beregn indeks + uke_nr
ev_weekly <- volume_all %>%
  filter(segment == "Electric vehicles") %>%
  arrange(week) %>%
  mutate(
    week_num = as.numeric(difftime(week, min(week), units = "weeks")),
    index = (n / first(n)) * 100
  )

# 📈 Lineær modell
ev_lm <- lm(index ~ week_num, data = ev_weekly)

# ✂️ Segmentert regresjon (med to bruddpunkt)
ev_segmod2 <- segmented(ev_lm, seg.Z = ~week_num, psi = c(14, 36))

# 🔢 Ekstraher breakpoints
ev_breaks <- round(ev_segmod2$psi[, "Est."])

# 🎯 Modellvurdering
ev_r2 <- summary(ev_segmod2)$r.squared
ev_resid <- summary(ev_segmod2)$sigma

# 🔮 Predikert trendlinje
ev_weekly$fit <- predict(ev_segmod2)

# 🎨 Visualisering
ggplot(ev_weekly, aes(x = week_num, y = index)) +
  geom_line(color = "#FFDD57", size = 1.2) +
  geom_line(aes(y = fit), color = "white", size = 1.1, linetype = "solid") +
  geom_vline(xintercept = ev_breaks, linetype = "dashed", color = "white") +
  annotate("text", x = ev_breaks, y = max(ev_weekly$index, na.rm = TRUE),
           label = paste("Breakpoint\nWeek", ev_breaks),
           color = "white", angle = 90, vjust = -0.5, hjust = 1.1, size = 3.5) +
  labs(
    title = "Segmented trend in Electric Vehicles volume (2 breakpoints)",
    subtitle = "Three growth periods – testing improved model fit",
    x = "Week",
    y = "Volume index (week 1 = 100)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white")
  )

# ggsave(
  filename = "Segmented trend in Electric Vehicles volume (2 breakpoints).png",
  plot = last_plot(),  # evt. erstatt med objekt hvis du har lagret plottet som `p1` eller lignende
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)


#samle og sammenligne modellkvaliteten for de tre segmentene med to bruddpunkter
# 📊 Sammenligningstabell – 2-breakpoint modeller

compare_models_2breaks <- tibble(
  Segment = c("Tesla", "VW ID", "Electric vehicles"),
  `Breakpoints` = c(
    paste(round(tesla_seg_multi$psi[, "Est."], 1), collapse = ", "),
    paste(round(vwid_segmod2$psi[, "Est."], 1), collapse = ", "),
    paste(round(ev_segmod2$psi[, "Est."], 1), collapse = ", ")
  ),
  `R²` = c(
    round(summary(tesla_seg_multi)$r.squared, 2),
    round(summary(vwid_segmod2)$r.squared, 2),
    round(summary(ev_segmod2)$r.squared, 2)
  ),
  `Residual Std. Error` = c(
    round(summary(tesla_seg_multi)$sigma, 2),
    round(summary(vwid_segmod2)$sigma, 2),
    round(summary(ev_segmod2)$sigma, 2)
  )
)

# 📋 Vis tabell
compare_models_2breaks %>%
  kable(
    caption = "Comparison of 2-breakpoint segmented models – Tesla vs VW ID vs EV market",
    col.names = c("Segment", "Breakpoints", "R²", "Residual Std. Error"),
    digits = 2
  )



# Sammenligning av breakpoints i ett diagram
# 📍 Breakpoint-data samlet i én tabell
breakpoint_df <- tibble::tibble(
  segment = c(
    rep("Tesla", 2),
    rep("VW ID", 2),
    rep("Electric vehicles", 2)
  ),
  breakpoint = c(
    tesla_seg_multi$psi[, "Est."],
    vwid_segmod2$psi[, "Est."],
    ev_segmod2$psi[, "Est."]
  ),
  color = c(
    rep("#6BE7C2", 2),   # Tesla
    rep("#8BC34A", 2),   # VW ID
    rep("#FFDD57", 2)    # EV market
  )
)

# 📊 Visualisering
ggplot(breakpoint_df, aes(x = breakpoint, y = segment, color = segment)) +
  geom_point(size = 4) +
  geom_line(aes(group = segment), linewidth = 1.2) +
  scale_color_manual(values = c(
    "Tesla" = "#6BE7C2",
    "VW ID" = "#8BC34A",
    "Electric vehicles" = "#FFDD57"
  )) +
  labs(
    title = "Timing of structural shifts in listing volume",
    subtitle = "Segmented regression breakpoints – 2 per brand",
    x = "Estimated breakpoint (week number from April 1, 2024)",
    y = NULL,
    color = "Segment"
  ) +
  scale_x_continuous(breaks = seq(20, 40, 2)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.background = element_rect(fill = "#1e1e1e"),
    legend.key = element_rect(fill = "#1e1e1e")
  )

# ggsave(
  filename = "Timing of structural shifts in listing volume.png",
  plot = last_plot(),  # evt. erstatt med objekt hvis du har lagret plottet som `p1` eller lignende
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)



#Oppdatert og clean kode for prisrensing + ukentlig trend el vs fossil:
# 📦 1. Prisfilter – fjerner åpenbare feil (typisk leasing/testannonser)
price_clean <- market_data %>%
  filter(
    !is.na(Price),
    Price > 10000  # Ingen øvre grense – vi antar høye priser er reelle
  )

# 📊 2. Ukentlig medianpris per segment (Total, EV, Fossil)
price_trend <- price_clean %>%
  filter(segment %in% c("Total market", "Electric vehicles", "Fossil vehicles")) %>%
  group_by(week, segment) %>%
  summarise(median_price = median(Price), .groups = "drop")

# 🎨 3. Visualisering i darkmode
ggplot(price_trend, aes(x = week, y = median_price, color = segment)) +
  geom_line(size = 1.3) +
  geom_vline(data = events, aes(xintercept = date), linetype = "dashed", color = "white") +
  geom_text(data = events, aes(x = date, y = Inf, label = label),
            angle = 90, vjust = -0.5, hjust = 1.1,
            size = 3.5, color = "white", inherit.aes = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c(
    "Total market" = "#00c0c7",
    "Electric vehicles" = "#FFDD57",
    "Fossil vehicles" = "#FF6B6B"
  )) +
  labs(
    title = "Median retail price per week – Used car market",
    subtitle = "By segment (Apr 2024 – Mar 2025)",
    x = "Week",
    y = "Median price (DKK)",
    color = "Segment"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.background = element_rect(fill = "#1e1e1e"),
    legend.key = element_rect(fill = "#1e1e1e")
  )

# ggsave(
  filename = "Median retail price per week – Used car market.png",
  plot = last_plot(),  # evt. erstatt med objekt hvis du har lagret plottet som `p1` eller lignende
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)


# samme stil for Tesla vs. VW ID
# Ukentlig medianpris for Tesla og VW ID
# 📦 1. Filter og rengjøring for prisfeil
price_focus <- market_data %>%
  filter(
    segment %in% c("Tesla", "VW ID"),
    !is.na(Price),
    Price > 10000  # Fjern åpenbare outliers
  )

# 📊 2. Beregn ukentlig medianpris
price_focus_weekly <- price_focus %>%
  group_by(week, segment) %>%
  summarise(median_price = median(Price), .groups = "drop")

# 🎨 3. Visualisering i mørk stil
ggplot(price_focus_weekly, aes(x = week, y = median_price, color = segment)) +
  geom_line(size = 1.3) +
  geom_vline(data = events, aes(xintercept = date), linetype = "dashed", color = "white") +
  geom_text(data = events, aes(x = date, y = Inf, label = label),
            angle = 90, vjust = -0.5, hjust = 1.1,
            size = 3.5, color = "white", inherit.aes = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c(
    "Tesla" = "#6BE7C2",
    "VW ID" = "#8BC34A"
  )) +
  labs(
    title = "Median retail price per week – Tesla vs VW ID",
    subtitle = "Retail prices only (Apr 2024 – Mar 2025)",
    x = "Week",
    y = "Median price (DKK)",
    color = "Brand"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.background = element_rect(fill = "#1e1e1e"),
    legend.key = element_rect(fill = "#1e1e1e")
  )

# ggsave(
  filename = "Median retail price per week – Tesla vs VW ID.png",
  plot = last_plot(),  # evt. erstatt med objekt hvis du har lagret plottet som `p1` eller lignende
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)


# kvantifisere forholdet mellom ukentlig volum og medianpris per segment
# Korrelasjonsanalyse for utvalgte segmenter
# 🔁 Funksjon for korrelasjonsanalyse per segment
get_correlation <- function(seg) {
  df <- filtered_data %>%
    filter(segment == seg, Price >= 10000) %>%  # Fjern åpenbare outliers
    mutate(week = floor_date(date, "week")) %>%
    group_by(week) %>%
    summarise(
      volume = n(),
      median_price = median(Price),
      .groups = "drop"
    )
  
  # Sjekk at vi har nok variasjon
  if (nrow(df) < 5) return(NULL)
  
  # Beregn korrelasjon
  test <- cor.test(df$volume, df$median_price)
  tibble(
    Segment = seg,
    correlation = test$estimate,
    p_value = test$p.value
  )
}

# 🚗 Segmenter vi ønsker å analysere
segments_to_analyze <- c("Tesla", "VW ID", "Electric vehicles", "Fossil vehicles")

# 🧠 Kjør funksjonen og samle resultatene
correlation_results <- map_dfr(segments_to_analyze, get_correlation)

# 📋 Vis resultater som tabell
correlation_results %>%
  knitr::kable(
    caption = "Correlation between weekly listing volume and median retail price",
    digits = 3,
    col.names = c("Segment", "Pearson correlation", "P-value")
  )


# LOESS-smoothing av medianpris over tid (per uke)
# LOESS-pris for hovedsegmenter
price_loess <- market_data %>%
  filter(segment %in% c("Tesla", "VW ID", "Electric vehicles", "Fossil vehicles")) %>%
  group_by(week, segment) %>%
  summarise(median_price = median(Price, na.rm = TRUE), .groups = "drop")

# 🎨 Plot med mørk bakgrunn
ggplot(price_loess, aes(x = week, y = median_price, color = segment)) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, size = 1.2) +
  labs(
    title = "LOESS-smoothed median retail price",
    subtitle = "Weekly prices by segment – Retail only",
    x = "Week",
    y = "DKK",
    color = "Segment"
  ) +
  scale_color_manual(values = c(
    "Tesla" = "#6BE7C2",
    "VW ID" = "#8BC34A",
    "Electric vehicles" = "#FFDD57",
    "Fossil vehicles" = "#FF6B6B"
  )) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e", color = NA),
    panel.background = element_rect(fill = "#1e1e1e", color = NA),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.background = element_rect(fill = "#1e1e1e"),
    legend.key = element_rect(fill = "#1e1e1e")
  )

# ggsave(
  filename = "LOESS-smoothed median retail price.png",
  plot = last_plot(),  # evt. erstatt med objekt hvis du har lagret plottet som `p1` eller lignende
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)


# Price index per week
# 📈 Lag prisindeks: uke 1 = 100
price_index <- market_data %>%
  filter(segment %in% c("Tesla", "VW ID", "Electric vehicles", "Fossil vehicles")) %>%
  group_by(segment, week) %>%
  summarise(median_price = median(Price, na.rm = TRUE), .groups = "drop") %>%
  group_by(segment) %>%
  arrange(week) %>%
  mutate(price_index = (median_price / first(median_price)) * 100) %>%
  ungroup()

# 🎨 Plot
ggplot(price_index, aes(x = week, y = price_index, color = segment)) +
  geom_line(size = 1.2) +
  labs(
    title = "Retail price index (Week 1 = 100)",
    subtitle = "Weekly median prices, indexed to April 2024",
    x = "Week",
    y = "Price index",
    color = "Segment"
  ) +
  scale_color_manual(values = c(
    "Tesla" = "#6BE7C2",
    "VW ID" = "#8BC34A",
    "Electric vehicles" = "#FFDD57",
    "Fossil vehicles" = "#FF6B6B"
  )) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.background = element_rect(fill = "#1e1e1e"),
    legend.key = element_rect(fill = "#1e1e1e")
  )

# ggsave(
  filename = "Retail price index (Week 1 = 100).png",
  plot = last_plot(),  # evt. erstatt med objekt hvis du har lagret plottet som `p1` eller lignende
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)

# Volum vs pris – scatter med regresjonslinje
# 📈 Ukentlig volum og medianpris
vol_vs_price <- market_data %>%
  filter(segment %in% c("Tesla", "VW ID", "Electric vehicles", "Fossil vehicles")) %>%
  group_by(segment, week) %>%
  summarise(
    n = n(),
    median_price = median(Price, na.rm = TRUE),
    .groups = "drop"
  )

# 🎨 Scatterplot med regresjonslinje
ggplot(vol_vs_price, aes(x = n, y = median_price, color = segment)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  labs(
    title = "Volume vs. median retail price",
    subtitle = "Each point is one week (Retail only)",
    x = "Number of listings (weekly)",
    y = "Median retail price",
    color = "Segment"
  ) +
  scale_color_manual(values = c(
    "Tesla" = "#6BE7C2",
    "VW ID" = "#8BC34A",
    "Electric vehicles" = "#FFDD57",
    "Fossil vehicles" = "#FF6B6B"
  )) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.background = element_rect(fill = "#1e1e1e"),
    legend.key = element_rect(fill = "#1e1e1e")
  )

# ggsave(
  filename = "Volume vs. median retail price.png",
  plot = last_plot(),  # evt. erstatt med objekt hvis du har lagret plottet som `p1` eller lignende
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)



# nytt scatterplot av tesla, vw og skoda
# 📦 Data: ukentlig volum og medianpris for Tesla, VW ID og Skoda Enyaq
scatter_brands <- filtered_data %>%
  filter(segment %in% c("Tesla", "VW ID") | (make == "Skoda" & model == "Enyaq")) %>%
  mutate(segment = case_when(
    make == "Skoda" & model == "Enyaq" ~ "Skoda Enyaq",
    TRUE ~ segment
  )) %>%
  group_by(week, segment) %>%
  summarise(
    volume = n(),
    median_price = median(Price, na.rm = TRUE),
    .groups = "drop"
  )

# 🎨 Plot
ggplot(scatter_brands, aes(x = volume, y = median_price, color = segment)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  scale_color_manual(values = c(
    "Tesla" = "#6BE7C2",
    "VW ID" = "#8BC34A",
    "Skoda Enyaq" = "#FFDD57"
  )) +
  labs(
    title = "Volume vs. median retail price – Selected brands",
    subtitle = "Weekly listings (Retail only)",
    x = "Number of listings",
    y = "Median price (DKK)",
    color = "Segment"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e"),
    panel.background = element_rect(fill = "#1e1e1e"),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.background = element_rect(fill = "#1e1e1e"),
    legend.key = element_rect(fill = "#1e1e1e")
  )

# ggsave(
  filename = "Volume vs. median retail price – Selected brands.png",
  plot = last_plot(),  # evt. erstatt med objekt hvis du har lagret plottet som `p1` eller lignende
  width = 10,
  height = 6,
  dpi = 300,
  bg = "#1e1e1e"
)


