# ==============================================================================
# 02_charts.R — Tesla Trend Analysis (DK)
# Formål:   Producér alle charts fra validerede .rds-filer
# Input:    data/weekly_volume.rds, data/weekly_price.rds,
#           data/market_data.rds, data/events.rds
# Output:   output/*.png
# Køres:    Efter 01_data.R er kørt og validation_checklist.md er godkendt
# ==============================================================================

library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(lubridate)
library(segmented)
library(purrr)
library(tidyr)

library(conflicted)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::collapse)
conflicts_prefer(dplyr::first)

# ------------------------------------------------------------------------------
# 0. Indlæs data og sæt fælles parametre
# ------------------------------------------------------------------------------

weekly_volume <- readRDS("data/weekly_volume.rds")
weekly_price  <- readRDS("data/weekly_price.rds")
market_data   <- readRDS("data/market_data.rds")
events        <- readRDS("data/events.rds")

output_dir <- "output"
if (!dir.exists(output_dir)) dir.create(output_dir)

# Fælles farvepalette — aldrig automatisk ggplot2-farver i leverancer
brand_colors <- c(
  "Tesla"             = "#6BE7C2",
  "VW ID"             = "#8BC34A",
  "Hyundai Ioniq 5"   = "#FFA040",   # Ændret fra #F8766D til orange
  "Skoda Enyaq"       = "#00B0F6",
  "BYD"               = "#E76BF3",
  "Electric vehicles" = "#FFDD57",
  "Fossil vehicles"   = "#FF6B6B",
  "Total market"      = "#00c0c7"
)

# Fælles dark theme
theme_dark_ts <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = "#1e1e1e", color = NA),
      panel.background = element_rect(fill = "#1e1e1e", color = NA),
      panel.grid.major = element_line(color = "#444444"),
      panel.grid.minor = element_blank(),
      text             = element_text(color = "white"),
      axis.text        = element_text(color = "white"),
      legend.background = element_rect(fill = "#1e1e1e"),
      legend.key       = element_rect(fill = "#1e1e1e"),
      plot.title       = element_text(size = base_size + 2, face = "bold"),
      plot.subtitle    = element_text(size = base_size - 1, color = "#aaaaaa")
    )
}

# Event-linjer — geom-lag der kan tilføjes til alle plots
event_lines <- function(events) {
  list(
    geom_vline(
      data = events,
      aes(xintercept = as.numeric(date)),
      linetype = "dashed", color = "white", alpha = 0.6
    ),
    geom_text(
      data = events,
      aes(x = date, y = Inf, label = label),
      angle = 90, vjust = -0.4, hjust = 1.1,
      size = 3.2, color = "white", inherit.aes = FALSE
    )
  )
}

# Gem-funktion
save_chart <- function(filename, width = 10, height = 6) {
  ggsave(
    filename = file.path(output_dir, filename),
    plot     = last_plot(),
    width    = width,
    height   = height,
    dpi      = 300,
    bg       = "#1e1e1e"
  )
  cat("✓ Gemt:", filename, "\n")
}

# ------------------------------------------------------------------------------
# 1. Market overview — Total vs Electric vs Fossil (LOESS)
# ------------------------------------------------------------------------------

p1_data <- weekly_volume |>
  filter(segment %in% c("Total market", "Electric vehicles", "Fossil vehicles"))

ggplot(p1_data, aes(x = week, y = n, color = segment)) +
  geom_smooth(method = "loess", span = 0.3, se = TRUE, alpha = 0.15) +
  event_lines(events) +
  scale_color_manual(values = brand_colors) +
  scale_fill_manual(values = brand_colors) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Weekly listing volume — Market overview",
    subtitle = "Total vs Electric vs Fossil · LOESS smoothed (span = 0.3) · Retail only · Asking prices",
    x = "Week", y = "Number of listings", color = "Segment"
  ) +
  theme_dark_ts()

save_chart("01_market_overview_loess.png")

# ------------------------------------------------------------------------------
# 2. Brand comparison — Tesla vs peers (LOESS)
# ------------------------------------------------------------------------------

p2_data <- weekly_volume |>
  filter(segment %in% c("Tesla", "VW ID", "Hyundai Ioniq 5",
                        "Skoda Enyaq", "BYD"))

ggplot(p2_data, aes(x = week, y = n, color = segment)) +
  geom_smooth(method = "loess", span = 0.3, se = TRUE, alpha = 0.15) +
  event_lines(events) +
  scale_color_manual(values = brand_colors) +
  scale_fill_manual(values = brand_colors) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Weekly listing volume — Brand comparison",
    subtitle = "Tesla vs peers · LOESS smoothed (span = 0.3) · Retail only · Asking prices",
    x = "Week", y = "Number of listings", color = "Brand"
  ) +
  theme_dark_ts()

save_chart("02_brand_comparison_loess.png")

# ------------------------------------------------------------------------------
# 3. LOESS-sensitivitetsanalyse — span 0.2 / 0.3 / 0.5
# ------------------------------------------------------------------------------

library(cowplot)

sens_data <- weekly_volume |>
  filter(segment %in% c("Tesla", "VW ID"))

spans <- c(0.2, 0.3, 0.5)

plots_sens <- map(spans, function(sp) {
  ggplot(sens_data, aes(x = week, y = n, color = segment)) +
    geom_smooth(method = "loess", span = sp, se = FALSE, linewidth = 1.1) +
    scale_color_manual(values = brand_colors) +
    scale_y_continuous(labels = comma) +
    labs(title = paste("span =", sp), x = NULL, y = "Listings", color = NULL) +
    theme_dark_ts(base_size = 11) +
    theme(legend.position = if (sp == 0.3) "bottom" else "none")
})

combined <- plot_grid(plotlist = plots_sens, ncol = 3)
title_row <- ggdraw() +
  draw_label("LOESS sensitivity — Tesla vs VW ID",
             color = "white", fontface = "bold", size = 13)

final_sens <- plot_grid(title_row, combined,
                        ncol = 1, rel_heights = c(0.1, 1))

ggsave(
  filename = file.path(output_dir, "03_loess_sensitivity.png"),
  plot     = final_sens,
  width    = 14, height = 5, dpi = 300, bg = "#1e1e1e"
)
cat("✓ Gemt: 03_loess_sensitivity.png\n")

# ------------------------------------------------------------------------------
# 4. Segmenteret regression — hjælpefunktioner
# ------------------------------------------------------------------------------

# Byg ugentlig indeks-datasæt per segment
build_weekly_index <- function(segment_name, vol_data) {
  vol_data |>
    filter(segment == segment_name) |>
    arrange(week) |>
    mutate(
      week_num = as.numeric(difftime(week, min(week), units = "weeks")),
      index    = (n / first(n)) * 100
    )
}

# Fit segmenteret regression med 2 breakpoints + ekstraher resultater
fit_segmented <- function(df, start_psi = c(15, 40)) {
  lm_fit  <- lm(index ~ week_num, data = df)
  seg_fit <- tryCatch(
    segmented(lm_fit, seg.Z = ~week_num,
              psi = start_psi,
              control = seg.control(it.max = 100, tol = 1e-7)),
    error = function(e) NULL
  )
  if (is.null(seg_fit)) return(NULL)
  
  breaks <- seg_fit$psi[, "Est."]
  ci     <- confint(seg_fit)
  slopes <- slope(seg_fit)$week_num[, 1]
  r2     <- summary(seg_fit)$r.squared
  rse    <- summary(seg_fit)$sigma
  
  list(model = seg_fit, df = df, breaks = breaks,
       ci = ci, slopes = slopes, r2 = r2, rse = rse)
}

# Plot segmenteret regression for ét segment
plot_segmented <- function(fit, segment_name, color) {
  df <- fit$df
  df$fitted <- fitted(fit$model)
  breaks <- fit$breaks
  ci     <- fit$ci
  
  # Konvertér week_num breakpoints til datoer
  origin_week <- min(df$week)
  break_dates <- origin_week + weeks(round(breaks))
  ci_low_dates  <- origin_week + weeks(round(ci[, "CI(95%).low"]))
  ci_high_dates <- origin_week + weeks(round(ci[, "CI(95%).up"]))
  
  ggplot(df, aes(x = week)) +
    geom_line(aes(y = index), color = color, linewidth = 1.0, alpha = 0.5) +
    geom_line(aes(y = fitted), color = "white", linewidth = 1.1) +
    geom_vline(xintercept = as.numeric(break_dates),
               linetype = "dashed", color = "white") +
    # CI på breakpoints
    geom_rect(
      data = data.frame(
        xmin = as.numeric(ci_low_dates),
        xmax = as.numeric(ci_high_dates)
      ),
      aes(xmin = as.Date(xmin, origin = "1970-01-01"),
          xmax = as.Date(xmax, origin = "1970-01-01"),
          ymin = -Inf, ymax = Inf),
      fill = "white", alpha = 0.08, inherit.aes = FALSE
    ) +
    annotate("text",
             x = break_dates,
             y = max(df$index, na.rm = TRUE),
             label = paste0("BP ", round(breaks, 1),
                            "\n[", round(fit$ci[, "CI(95%).low"], 1),
                            "–", round(fit$ci[, "CI(95%).up"], 1), "]"),
             color = "white", angle = 90, vjust = -0.3,
             hjust = 1.1, size = 3.0) +
    labs(
      title    = paste("Segmented trend —", segment_name, "(2 breakpoints)"),
      subtitle = paste0("R² = ", round(fit$r2, 2),
                        " · RSE = ", round(fit$rse, 1),
                        " · CI shown as shaded band"),
      x = "Week", y = "Volume index (week 1 = 100)"
    ) +
    theme_dark_ts()
}

# ------------------------------------------------------------------------------
# 5. Kør segmenteret regression for alle brands
# ------------------------------------------------------------------------------

seg_brands <- c("Tesla", "VW ID", "Hyundai Ioniq 5", "Skoda Enyaq")

seg_fits <- map(seg_brands, function(b) {
  df <- build_weekly_index(b, weekly_volume)
  if (nrow(df) < 20) return(NULL)
  fit_segmented(df)
}) |> set_names(seg_brands)

# Gem individuelle plots
walk2(seg_brands, seg_brands, function(brand, name) {
  fit <- seg_fits[[brand]]
  if (is.null(fit)) {
    cat("⚠️  Springer over", brand, "— model fejlede\n")
    return()
  }
  col <- brand_colors[brand]
  plot_segmented(fit, brand, col)
  fname <- paste0("04_segmented_", str_replace_all(tolower(brand), " ", "_"), ".png")
  save_chart(fname)
})

# ------------------------------------------------------------------------------
# 6. Breakpoint timing — sammenligning på tværs af brands
# ------------------------------------------------------------------------------

bp_data <- map_dfr(seg_brands, function(brand) {
  fit <- seg_fits[[brand]]
  if (is.null(fit)) return(NULL)
  origin <- min(fit$df$week)
  tibble(
    brand    = brand,
    bp       = 1:2,
    date     = origin + weeks(round(fit$breaks)),
    ci_low   = origin + weeks(round(fit$ci[, "CI(95%).low"])),
    ci_high  = origin + weeks(round(fit$ci[, "CI(95%).up"]))
  )
}) |>
  mutate(
    # Markér CI'er der krydser analysens startdato som upålidelige
    unreliable = ci_low < as.Date("2024-07-01"),
    ci_low     = if_else(unreliable, date, ci_low),
    ci_high    = if_else(unreliable, date, ci_high)
  )

ggplot(bp_data, aes(x = date, y = reorder(brand, desc(date)),
                    color = brand)) +
  geom_linerange(aes(xmin = ci_low, xmax = ci_high),
                 linewidth = 2, alpha = 0.3) +
  geom_point(size = 4) +
  geom_point(data = filter(bp_data, unreliable),
             shape = 4, size = 5, color = "white") +
  geom_line(aes(group = brand), linewidth = 1.0, alpha = 0.6) +
  scale_color_manual(values = brand_colors) +
  labs(
    title    = "Timing of structural shifts in listing volume",
    subtitle = "Segmented regression breakpoints with 95% CI · ✕ = estimate near boundary, interpret with caution",
    x = "Date", y = NULL, color = "Brand"
  ) +
  theme_dark_ts() +
  theme(legend.position = "none")

save_chart("05_breakpoint_timing.png")

# ------------------------------------------------------------------------------
# 7. Korrelation: pris vs. volumen — Pearson OG Spearman
# ------------------------------------------------------------------------------

get_correlations <- function(seg) {
  df <- market_data |>
    filter(segment == seg, price >= 10000) |>
    mutate(week = floor_date(date_added, "week")) |>
    group_by(week) |>
    summarise(volume = n(), median_price = median(price), .groups = "drop")
  
  if (nrow(df) < 10) return(NULL)
  
  pearson  <- cor.test(df$volume, df$median_price, method = "pearson")
  spearman <- cor.test(df$volume, df$median_price, method = "spearman",
                       exact = FALSE)
  
  tibble(
    segment         = seg,
    pearson_r       = round(pearson$estimate, 3),
    pearson_p       = round(pearson$p.value, 3),
    spearman_rho    = round(spearman$estimate, 3),
    spearman_p      = round(spearman$p.value, 3)
  )
}

corr_segments <- c("Tesla", "VW ID", "Hyundai Ioniq 5",
                   "Skoda Enyaq", "Electric vehicles", "Fossil vehicles")

corr_results <- map_dfr(corr_segments, get_correlations)

cat("\n--- Korrelationsresultater ---\n")
print(corr_results)

# Visualisér som tabel-plot
corr_long <- corr_results |>
  dplyr::select(segment, pearson_r, spearman_rho) |>
  pivot_longer(cols = c(pearson_r, spearman_rho),
               names_to = "method", values_to = "r") |>
  mutate(
    method = recode(method,
                    "pearson_r"    = "Pearson r",
                    "spearman_rho" = "Spearman ρ"),
    sig_pearson  = corr_results$pearson_p[match(segment,
                                                corr_results$segment)] < 0.05,
    sig_spearman = corr_results$spearman_p[match(segment,
                                                 corr_results$segment)] < 0.05
  )

ggplot(corr_long, aes(x = r, y = reorder(segment, r),
                      color = method, shape = method)) +
  geom_vline(xintercept = 0, color = "#666666", linetype = "dashed") +
  geom_point(size = 4, position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c("Pearson r" = "#6BE7C2",
                                "Spearman ρ" = "#FFDD57")) +
  scale_x_continuous(limits = c(-1, 0.5)) +
  labs(
    title    = "Correlation: weekly volume vs. median retail price",
    subtitle = "Pearson r and Spearman ρ · Negative = more listings → lower prices",
    x = "Correlation coefficient", y = NULL, color = "Method", shape = "Method"
  ) +
  theme_dark_ts()

save_chart("06_correlation_price_volume.png")

# ------------------------------------------------------------------------------
# 8. Scatterplot — volumen vs. medianpris
# ------------------------------------------------------------------------------

scatter_data <- weekly_price |>
  dplyr::filter(segment %in% c("Tesla", "VW ID",
                               "Hyundai Ioniq 5", "Skoda Enyaq"))

ggplot(scatter_data, aes(x = n, y = median_price, color = segment)) +
  geom_point(alpha = 0.5, size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = brand_colors) +
  scale_y_continuous(labels = comma) +
  coord_cartesian(xlim = c(0, 600)) +   # Afskærer outlier visuelt
  labs(
    title    = "Volume vs. median retail price",
    subtitle = "Each point = one week · Linear trend per brand · Asking prices only",
    x = "Weekly listings", y = "Median retail price (DKK)", color = "Brand"
  ) +
  theme_dark_ts()

save_chart("07_scatter_volume_price.png")

# ------------------------------------------------------------------------------
# 9. LOESS-smoothed medianpris over tid
# ------------------------------------------------------------------------------

ggplot(weekly_price |>
         filter(segment %in% c("Tesla", "VW ID", "Hyundai Ioniq 5",
                               "Skoda Enyaq", "Electric vehicles",
                               "Fossil vehicles")),
       aes(x = week, y = median_price, color = segment)) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, linewidth = 1.2) +
  event_lines(events) +
  scale_color_manual(values = brand_colors) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "LOESS-smoothed median retail price",
    subtitle = "Weekly asking prices by segment · span = 0.3 · Not transaction prices",
    x = "Week", y = "Median price (DKK)", color = "Segment"
  ) +
  theme_dark_ts()

save_chart("08_price_loess.png")

# ------------------------------------------------------------------------------
# 10. Event-volatilitetsanalyse — Musk/Trump-vinklen
# Hypotese: Tesla's prisvolatilitet (IQR) øges around events vs. peers
# ------------------------------------------------------------------------------

event_window_weeks <- 4

volatility_data <- weekly_price |>
  dplyr::filter(segment %in% c("Tesla", "VW ID", "Hyundai Ioniq 5")) |>
  dplyr::select(week, segment, iqr_price, median_price)

ggplot(volatility_data, aes(x = week, y = iqr_price, color = segment)) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, linewidth = 1.4) +
  event_lines(events) +
  scale_color_manual(values = brand_colors) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Price volatility (IQR) over time — Tesla vs peers",
    subtitle = "Weekly interquartile range of retail asking prices · LOESS smoothed",
    x = "Week", y = "Price IQR (DKK)", color = "Brand"
  ) +
  theme_dark_ts()

save_chart("09_volatility_iqr.png")

# Event-vindue deep-dive
walk(seq_len(nrow(events)), function(i) {
  ev        <- events[i, ]
  win_start <- ev$date - weeks(event_window_weeks)
  win_end   <- ev$date + weeks(event_window_weeks)
  
  win_data <- weekly_price |>
    dplyr::filter(
      segment %in% c("Tesla", "VW ID", "Hyundai Ioniq 5"),
      week >= win_start & week <= win_end
    )
  
  if (nrow(win_data) == 0) return()
  
  # Tilføj datavolumeadvarsel for juli-vinduet
  n_tesla <- win_data |>
    dplyr::filter(segment == "Tesla") |> nrow()
  subtitle_note <- if (ev$date < as.Date("2024-09-01") && n_tesla < 6) {
    paste0("±", event_window_weeks,
           " weeks · IQR of weekly asking prices · ",
           "Note: low Tesla volume in this window — interpret with caution")
  } else {
    paste0("±", event_window_weeks,
           " weeks · IQR of weekly asking prices · ",
           "Descriptive only — no causal inference")
  }
  
  ggplot(win_data, aes(x = week, y = iqr_price, color = segment)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5) +
    geom_vline(xintercept = as.numeric(ev$date),
               linetype = "dashed", color = "white") +
    annotate("text", x = ev$date, y = Inf,
             label = ev$label, angle = 90,
             vjust = -0.3, hjust = 1.1,
             color = "white", size = 3.2) +
    scale_color_manual(values = brand_colors) +
    scale_y_continuous(labels = comma) +
    labs(
      title    = paste("Price volatility around:", ev$label),
      subtitle = subtitle_note,
      x = "Week", y = "Price IQR (DKK)", color = "Brand"
    ) +
    theme_dark_ts()
  
  fname <- paste0("10_event_volatility_",
                  str_replace_all(tolower(ev$label), " ", "_"), ".png")
  save_chart(fname)
})
# ------------------------------------------------------------------------------
# 11. Appendix — rå ugentlig medianpris (uden smoothing)
# ------------------------------------------------------------------------------

ggplot(weekly_price |>
         dplyr::filter(segment %in% c("Tesla", "VW ID",
                                      "Hyundai Ioniq 5", "Skoda Enyaq")),
       aes(x = week, y = median_price, color = segment)) +
  geom_line(linewidth = 0.9, alpha = 0.8) +
  event_lines(events) +
  facet_wrap(~ segment, ncol = 2, scales = "free_y") +
  scale_color_manual(values = brand_colors) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Weekly median retail price — unsmoothed",
    subtitle = "Raw weekly medians · Asking prices only · Not transaction prices",
    x = "Week", y = "Median price (DKK)"
  ) +
  theme_dark_ts() +
  theme(legend.position = "none")

save_chart("11_appendix_raw_price.png")

# ------------------------------------------------------------------------------
# Afslutning
# ------------------------------------------------------------------------------

cat("\n✓ Alle charts gemt i", output_dir, "\n")
cat("Klar til quarto render\n")