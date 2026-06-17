# ============================================================
# Heatmaps and saturation simulation from nested systematic map JSON
# 1000 random subsampling simulations
# ============================================================

library(jsonlite)
library(tidyverse)
library(patchwork)

# ----------------------------
# 1. Read JSON
# ----------------------------

dat <- fromJSON(
  "data/profor.json",
  flatten = TRUE
)

# ----------------------------
# 2. Unnest key variables
# ----------------------------

map_data <- dat |>
  as_tibble() |>
  select(aid, Title, geo, outcome, intervention) |>
  unnest(geo) |>
  unnest(outcome) |>
  unnest(intervention) |>
  transmute(
    aid,
    country = Study_country.x,
    region = region,
    outcome = Out_type_assigned,
    outcome_subtype = Out_subtype,
    intervention = Int_type
  ) |>
  mutate(
    across(
      c(country, region, outcome, outcome_subtype, intervention),
      ~replace_na(as.character(.x), "NA")
    )
  ) |>
  distinct()

# ----------------------------
# 3. Keep top intervention categories
# ----------------------------

top_n_x <- function(data, x, n = 15) {
  
  top <- data |>
    count({{ x }}, sort = TRUE) |>
    slice_head(n = n) |>
    pull({{ x }})
  
  data |>
    mutate({{ x }} := if_else({{ x }} %in% top, {{ x }}, "Other"))
}

map_data_top <- map_data |>
  top_n_x(intervention, n = 15)

# ----------------------------
# 4. Heatmap function
# ----------------------------

make_prop_heatmap <- function(data, xvar, yvar, title = "", full_data = map_data_top) {
  
  x_name <- rlang::as_name(rlang::enquo(xvar))
  y_name <- rlang::as_name(rlang::enquo(yvar))
  
  full_grid <- full_data |>
    distinct({{ xvar }}, {{ yvar }})
  
  max_prop <- full_data |>
    count({{ xvar }}, {{ yvar }}, name = "n") |>
    mutate(prop = n / sum(n)) |>
    pull(prop) |>
    max(na.rm = TRUE)
  
  plot_data <- data |>
    count({{ xvar }}, {{ yvar }}, name = "n") |>
    right_join(full_grid, by = c(x_name, y_name)) |>
    mutate(
      n = replace_na(n, 0),
      prop = n / sum(n)
    )
  
  ggplot(plot_data, aes(x = {{ xvar }}, y = {{ yvar }}, fill = prop)) +
    geom_tile(colour = "white") +
    scale_fill_viridis_c(
      option = "C",
      labels = scales::percent,
      limits = c(0, max_prop)
    ) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(drop = FALSE) +
    labs(
      x = NULL,
      y = NULL,
      fill = "Proportion",
      title = title
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}

# ----------------------------
# 5. Example cumulative heatmaps from one random ordering
# ----------------------------

set.seed(1)

study_order <- map_data_top |>
  distinct(aid) |>
  slice_sample(prop = 1)

get_sample <- function(data, percent, study_order) {
  
  keep_ids <- study_order |>
    slice_head(prop = percent / 100) |>
    pull(aid)
  
  data |>
    filter(aid %in% keep_ids)
}

sample_20  <- get_sample(map_data_top, 20, study_order)
sample_40  <- get_sample(map_data_top, 40, study_order)
sample_60  <- get_sample(map_data_top, 60, study_order)
sample_80  <- get_sample(map_data_top, 80, study_order)
sample_100 <- get_sample(map_data_top, 100, study_order)

p20 <- make_prop_heatmap(sample_20, intervention, outcome_subtype, "20% coded")
p40 <- make_prop_heatmap(sample_40, intervention, outcome_subtype, "40% coded")
p60 <- make_prop_heatmap(sample_60, intervention, outcome_subtype, "60% coded")
p80 <- make_prop_heatmap(sample_80, intervention, outcome_subtype, "80% coded")
p100 <- make_prop_heatmap(sample_100, intervention, outcome_subtype, "100% coded")

heatmap_plot <- (p20 + p40 + p60) /
  (p80 + p100 + plot_spacer())

heatmap_plot

ggsave(
  "outputs/plots/intervention_outcome_cumulative_heatmaps.png",
  heatmap_plot,
  width = 16,
  height = 9,
  dpi = 300
)

# ----------------------------
# 6. 1000 simulations: heatmap cell recovery
# ----------------------------

n_sims <- 1000
percentages <- seq(10, 100, by = 10)

all_cells <- map_data_top |>
  distinct(intervention, outcome_subtype)

n_total_cells <- nrow(all_cells)

simulate_cell_recovery <- function(data, sim_id, percentages) {
  
  study_order <- data |>
    distinct(aid) |>
    slice_sample(prop = 1)
  
  map_dfr(percentages, function(p) {
    
    keep_ids <- study_order |>
      slice_head(prop = p / 100) |>
      pull(aid)
    
    dat_p <- data |>
      filter(aid %in% keep_ids)
    
    n_cells_p <- dat_p |>
      distinct(intervention, outcome_subtype) |>
      nrow()
    
    tibble(
      sim = sim_id,
      percent_screened = p,
      cells_recovered = n_cells_p,
      proportion_cells_recovered = n_cells_p / n_total_cells
    )
  })
}

set.seed(123)

cell_recovery_sims <- map_dfr(
  1:n_sims,
  ~simulate_cell_recovery(
    data = map_data_top,
    sim_id = .x,
    percentages = percentages
  )
)

cell_recovery_summary <- cell_recovery_sims |>
  group_by(percent_screened) |>
  summarise(
    mean_recovery = mean(proportion_cells_recovered),
    lower = quantile(proportion_cells_recovered, 0.025),
    upper = quantile(proportion_cells_recovered, 0.975),
    .groups = "drop"
  )

cell_recovery_summary

cell_recovery_plot <- ggplot(
  cell_recovery_summary,
  aes(percent_screened, mean_recovery)
) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.2
  ) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = percentages,
    labels = scales::percent_format(scale = 1)
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  labs(
    x = "Percentage of studies coded",
    y = "Final heatmap cells recovered"
    #,
    #title = "Recovery of evidence map structure under random subsampling",
    #subtitle = "Mean and 95% simulation interval from 1000 random study orderings"
  ) +
  theme_minimal(base_size = 13)

cell_recovery_plot

ggsave(
  "outputs/plots/cell_recovery_1000_simulations.png",
  cell_recovery_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# ----------------------------
# 7. 1000 simulations: saturation of unique coding values
# ----------------------------

simulate_unique_values <- function(data, variable, variable_name, sim_id, percentages) {
  
  study_order <- data |>
    distinct(aid) |>
    slice_sample(prop = 1)
  
  map_dfr(percentages, function(p) {
    
    keep_ids <- study_order |>
      slice_head(prop = p / 100) |>
      pull(aid)
    
    dat_p <- data |>
      filter(aid %in% keep_ids)
    
    tibble(
      sim = sim_id,
      variable = variable_name,
      percent_screened = p,
      n_unique_values = n_distinct(pull(dat_p, {{ variable }}))
    )
  })
}

set.seed(456)

unique_value_sims <- map_dfr(1:n_sims, function(i) {
  
  bind_rows(
    simulate_unique_values(map_data, country, "Country", i, percentages),
    simulate_unique_values(map_data_top, intervention, "Intervention", i, percentages),
    simulate_unique_values(map_data_top, outcome_subtype, "Outcome subtype", i, percentages)
  )
})

unique_value_summary <- unique_value_sims |>
  group_by(variable, percent_screened) |>
  summarise(
    mean_unique = mean(n_unique_values),
    lower = quantile(n_unique_values, 0.025),
    upper = quantile(n_unique_values, 0.975),
    .groups = "drop"
  )

unique_value_plot <- ggplot(
  unique_value_summary,
  aes(percent_screened, mean_unique)
) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.2
  ) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  facet_wrap(~variable, scales = "free_y") +
  scale_x_continuous(
    breaks = percentages,
    labels = scales::percent_format(scale = 1)
  ) +
  labs(
    x = "Percentage of studies coded",
    y = "Cumulative unique values"
    #,
    #title = "Information saturation across coding variables",
    #subtitle = "Mean and 95% simulation interval from 1000 random study orderings"
  ) +
  theme_minimal(base_size = 13)

unique_value_plot

ggsave(
  "outputs/plots/information_saturation_1000_simulations.png",
  unique_value_plot,
  width = 12,
  height = 6,
  dpi = 300
)

# ----------------------------
# 8. 1000 simulations: occupied heatmap cells
# ----------------------------

cell_count_summary <- cell_recovery_sims |>
  group_by(percent_screened) |>
  summarise(
    mean_cells = mean(cells_recovered),
    lower = quantile(cells_recovered, 0.025),
    upper = quantile(cells_recovered, 0.975),
    .groups = "drop"
  )

cell_count_plot <- ggplot(
  cell_count_summary,
  aes(percent_screened, mean_cells)
) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.2
  ) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = percentages,
    labels = scales::percent_format(scale = 1)
  ) +
  labs(
    x = "Percentage of studies coded",
    y = "Cumulative occupied heatmap cells"
    #,
    #title = "Saturation of intervention × outcome heatmap structure",
    #subtitle = "Mean and 95% simulation interval from 1000 random study orderings"
  ) +
  theme_minimal(base_size = 13)

cell_count_plot

ggsave(
  "outputs/plots/cell_saturation_1000_simulations.png",
  cell_count_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# ----------------------------
# 9. Export simulation summaries
# ----------------------------

write_csv(
  cell_recovery_summary,
  "outputs/tables/cell_recovery_1000_simulations_summary.csv"
)

write_csv(
  unique_value_summary,
  "outputs/tables/information_saturation_1000_simulations_summary.csv"
)

write_csv(
  cell_count_summary,
  "outputs/tables/cell_saturation_1000_simulations_summary.csv"
)

print(cell_recovery_summary, n = Inf)
