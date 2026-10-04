# =============================================================================
# 02_analysis.R — Key findings + static figures for the README
#
# Run from the repository root (after 01_clean.R):   Rscript R/02_analysis.R
# Output: outputs/key_findings.md, docs/figures/*.png
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate); library(ggplot2); library(sf)
})

incidents   <- readRDS("app/data/incidents.rds")
labels_long <- readRDS("app/data/labels_long.rds")
provinces   <- readRDS("app/data/provinces.rds")
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs", showWarnings = FALSE)

# ---- shared style ------------------------------------------------------------
ink <- "#0b0b0b"; ink2 <- "#52514e"; muted <- "#8a8984"; grid <- "#e6e5e0"
blue <- "#2a78d6"; blue_light <- "#b7d3f6"

theme_report <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title         = element_text(face = "bold", colour = ink, size = 14),
      plot.subtitle      = element_text(colour = ink2, margin = margin(b = 10)),
      plot.caption       = element_text(colour = muted, hjust = 0, size = 9),
      axis.text          = element_text(colour = ink2),
      axis.title         = element_text(colour = ink2, size = 10),
      panel.grid.major.y = element_line(colour = grid, linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.background    = element_rect(fill = "white", colour = NA),
      plot.title.position = "plot", plot.caption.position = "plot"
    )
}
src_caption <- "Source: 1,000 media-reported incidents, Jan 2018 to Jun 2020."

# ---- 1. Coverage --------------------------------------------------------------
date_min <- min(incidents$date, na.rm = TRUE)
date_max <- max(incidents$date, na.rm = TRUE)
by_year  <- incidents |> filter(!is.na(year)) |> count(year)

# ---- 2. Per-capita rates ------------------------------------------------------
# Raw counts mostly track population. Rates per 100k residents give a fairer
# comparison; provinces/territories with < 10 incidents are flagged as unstable.
rates <- provinces |>
  st_drop_geometry() |>
  mutate(reliable = incidents >= 10) |>
  arrange(desc(per_100k))

top_reliable <- rates |> filter(reliable) |> slice(1)
ontario      <- rates |> filter(province == "Ontario")
quebec       <- rates |> filter(province == "Quebec")

p_rates <- rates |>
  filter(reliable) |>
  mutate(province = reorder(province, per_100k),
         highlight = province %in% c(top_reliable$province, "Ontario")) |>
  ggplot(aes(per_100k, province)) +
  geom_col(aes(fill = highlight), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f  (n=%d)", per_100k, incidents)),
            hjust = -0.08, size = 3.4, colour = ink2) +
  scale_fill_manual(values = c(`TRUE` = blue, `FALSE` = blue_light), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.28))) +
  labs(title = "Ontario has the most incidents, but not the highest rate",
       subtitle = "Reported incidents per 100,000 residents, Jan 2018 to Jun 2020",
       x = NULL, y = NULL,
       caption = paste(src_caption, "\nPopulation: Statistics Canada 2021 Census.",
                       "Territories omitted (fewer than 10 incidents each).")) +
  theme_report() + theme(panel.grid.major.y = element_blank(),
                         axis.text.x = element_blank())
ggsave("docs/figures/rate_by_province.png", p_rates, width = 8, height = 4.8, dpi = 200)

# ---- 3. Timeline + 2019 federal election ------------------------------------
monthly <- incidents |>
  filter(!is.na(month)) |>
  count(month) |>
  complete(month = seq(min(month), max(month), by = "month"), fill = list(n = 0))
avg_month   <- mean(monthly$n)
peak        <- monthly |> slice_max(n, n = 1)
election    <- ymd("2019-10-21")

politics_oct19 <- labels_long |>
  filter(field == "Context", parent == "Politics") |>
  distinct(incident_id) |>
  inner_join(incidents, by = "incident_id") |>
  filter(month == ymd("2019-10-01")) |> nrow()

p_time <- ggplot(monthly, aes(month, n)) +
  geom_hline(yintercept = avg_month, colour = muted, linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = min(monthly$month), y = avg_month, vjust = -0.5, hjust = 0,
           label = sprintf("monthly average: %.0f", avg_month), colour = muted, size = 3.2) +
  geom_vline(xintercept = election, colour = ink2, linewidth = 0.4) +
  annotate("text", x = election, y = peak$n * 1.04, hjust = 1.05, vjust = 0,
           label = "Federal election\nOct 21, 2019", colour = ink, size = 3.3, lineheight = 0.9) +
  geom_line(colour = blue, linewidth = 0.8) +
  geom_point(data = peak, colour = blue, size = 2.6) +
  annotate("text", x = peak$month, y = peak$n, hjust = -0.2, vjust = 0.4,
           label = sprintf("%d incidents", peak$n), colour = ink, size = 3.3, fontface = "bold") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)), limits = c(0, NA)) +
  labs(title = "Reported incidents peaked in the month of the 2019 federal election",
       subtitle = sprintf("Incidents per month. October 2019 was %.1fx the average; %d of its %d incidents had a political context.",
                          peak$n / avg_month, politics_oct19, peak$n),
       x = NULL, y = NULL, caption = src_caption) +
  theme_report()
ggsave("docs/figures/monthly_timeline.png", p_time, width = 9, height = 4.4, dpi = 200)

# ---- 4. Anti-East-Asian incidents at the start of COVID-19 -------------------
east_asian <- labels_long |>
  filter(field == "Targeted community", label == "East Asian") |>
  inner_join(incidents, by = "incident_id") |>
  filter(!is.na(date))
ea_before <- east_asian |> filter(date <  ymd("2020-01-01")) |> nrow()
ea_2020   <- east_asian |> filter(date >= ymd("2020-01-01")) |> nrow()
months_before <- interval(floor_date(date_min, "month"), ymd("2020-01-01")) %/% months(1)
months_2020   <- interval(ymd("2020-01-01"), ceiling_date(date_max, "month")) %/% months(1)
ea_rate_before <- ea_before / months_before
ea_rate_2020   <- ea_2020 / months_2020

p_ea <- tibble(
  period = factor(c(sprintf("Jan 2018 to Dec 2019\n(%d months)", months_before),
                    sprintf("Jan to Jun 2020\n(%d months)", months_2020)),
                  levels = c(sprintf("Jan 2018 to Dec 2019\n(%d months)", months_before),
                             sprintf("Jan to Jun 2020\n(%d months)", months_2020))),
  rate = c(ea_rate_before, ea_rate_2020),
  n = c(ea_before, ea_2020)
) |>
  ggplot(aes(period, rate)) +
  geom_col(width = 0.5, fill = c(blue_light, blue)) +
  geom_text(aes(label = sprintf("%.1f / month\n(n=%d)", rate, n)), vjust = -0.3,
            size = 3.5, colour = ink, lineheight = 0.9) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(title = "Incidents targeting East Asian communities rose sharply in 2020",
       subtitle = sprintf("Average reported incidents per month: %.1fx higher in the first half of 2020",
                          ea_rate_2020 / ea_rate_before),
       x = NULL, y = NULL, caption = src_caption) +
  theme_report() + theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
ggsave("docs/figures/east_asian_2020.png", p_ea, width = 7, height = 4.4, dpi = 200)

# ---- 5. Criminal vs non-criminal -------------------------------------------
cat_tab <- incidents |> count(category) |> mutate(pct = n / sum(n) * 100)
pct_noncrim <- cat_tab$pct[cat_tab$category == "Non-Criminal"]
pct_charged <- cat_tab$pct[cat_tab$category == "Criminal: Charged"]

# ---- 6. Source language (coverage bias check) ------------------------------
french_outlets <- "radio-canada|lapresse|ledevoir|journaldemontreal|journaldequebec|tvanouvelles|lesoleil|ledroit"
n_french <- sum(grepl(french_outlets, incidents$source))
top_source <- incidents |> count(source, sort = TRUE) |> slice(1)

# ---- 7. Missingness -----------------------------------------------------------
pct_gender_missing <- mean(incidents$victim_gender == "Not reported") * 100
pct_prov_missing   <- mean(incidents$province == "Not specified") * 100

# ---- Write findings -------------------------------------------------------------
fmt <- function(x, d = 1) formatC(x, format = "f", digits = d)
findings <- c(
  "# Key findings (generated by R/02_analysis.R — do not edit by hand)",
  "",
  sprintf("* **Coverage:** %d incidents from %s to %s. 2020 covers only %d months, so yearly totals are not comparable (%s).",
          nrow(incidents), date_min, date_max, months_2020,
          paste(sprintf("%d: %d", by_year$year, by_year$n), collapse = ", ")),
  sprintf("* **Per capita:** %s has the highest rate among provinces with 10+ incidents (%s per 100k), vs %s for Ontario, which has the most incidents (%d).",
          top_reliable$province, fmt(top_reliable$per_100k), fmt(ontario$per_100k), ontario$incidents),
  sprintf("* **Election spike:** %s had %d incidents, %sx the monthly average of %s; %d had a political context.",
          format(peak$month, "%B %Y"), peak$n, fmt(peak$n / avg_month), fmt(avg_month), politics_oct19),
  sprintf("* **COVID-19:** incidents targeting East Asian communities rose from %s/month (2018-2019) to %s/month (Jan-Jun 2020), %sx higher.",
          fmt(ea_rate_before), fmt(ea_rate_2020), fmt(ea_rate_2020 / ea_rate_before)),
  sprintf("* **Criminality:** %s%% of incidents were non-criminal; only %s%% led to charges.",
          fmt(pct_noncrim), fmt(pct_charged)),
  sprintf("* **Coverage bias:** %d of %d articles come from French-language outlets; the top source is %s (%d). Quebec's low rate (%s per 100k) likely reflects this.",
          n_french, nrow(incidents), top_source$source, top_source$n, fmt(quebec$per_100k)),
  sprintf("* **Missing data:** victim gender not reported in %s%% of incidents; province missing in %s%%.",
          fmt(pct_gender_missing, 0), fmt(pct_prov_missing))
)
writeLines(findings, "outputs/key_findings.md")
cat(findings, sep = "\n")
