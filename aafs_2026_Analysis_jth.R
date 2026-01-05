################################################################################
# TITLE:  AAFS 2026 - Passalacqua Network Dashboard (Ego-Kawa)
# AUTHORS: Joe Hefner, Nick Passalacqua, Natalie Clark [with Quill]
# LAST UPDATED: 2025-11-06  (V1.4: Institution Type color+analytics; fixed palette)
################################################################################
rm(list = ls())
################################################################################
path_lineage <- "C:/Users/Joseph_Hefner/Desktop/Passalacqua AAFS 2026/AAFS_2026_Passal/academic_lineage_master.xlsx"
path_kawa    <- "C:/Users/Joseph_Hefner/Desktop/Passalacqua AAFS 2026/AAFS_2026_Passal/aman_data.xlsx"
################################################################################
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(janitor)
library(lubridate)
library(ggplot2)
library(broom)
library(forcats)
library(scales)
library(igraph)
library(gridExtra)
library(igraph)
library(ggplot2)
library(dplyr)
library(forcats)
library(scales)
library(MASS)
################################################################################
lineage <- read_excel(path_lineage, sheet = 1) |> clean_names()
kawa    <- read_excel(path_kawa, sheet = 1)    |> clean_names()

## Expecting columns (lineage):
## researcher_last, researcher_first, advisor_last, advisor_first,
## institution_current, institution_ph_d, ph_d_year, prestige, inbred,
## h_index, department, field, region, gender, urm_status, retired,
## deceased, institution_type

## Expecting columns (kawa):
## university, last_name, first_name, rank, subfield, ph_d_institution, year_of_ph_d

## ----------------
## Derive flags + harmonize
## ----------------
# Robust UTK prestige flag (updated)
is_trueish <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(x == 1)
  if (is.character(x)) {
    lx <- str_to_lower(x)
    return(str_detect(lx, "utk|u[\\.]?t[\\.]?k|tennessee|knoxville|univ.*tennessee"))
  }
  rep(NA, length(x))
}

lineage <- lineage |>
  mutate(
    ph_d_year = suppressWarnings(as.integer(ph_d_year)),
    years_since_phd = ifelse(!is.na(ph_d_year), 2025L - ph_d_year, NA_integer_),
    h_per_year = ifelse(!is.na(h_index) & !is.na(years_since_phd) & years_since_phd > 0,
                        h_index / years_since_phd, NA_real_),
    decade = ifelse(!is.na(ph_d_year), paste0(floor(ph_d_year/10)*10, "s"), NA_character_),
    
    # 🟡 UPDATED: broaden UTK prestige detection
    is_prestige_utk = is_trueish(prestige) |
      is_trueish(institution_ph_d) |
      is_trueish(institution_current),
    
    is_inbred_self_hire = as.logical(inbred),
    gender = case_when(
      is.na(gender) ~ NA_character_,
      str_to_lower(gender) %in% c("f","female","woman","women") ~ "Female",
      str_to_lower(gender) %in% c("m","male","man","men") ~ "Male",
      TRUE ~ "Other/NA"
    ),
    institution_type = as.character(institution_type),
    field = as.character(field)
  )



# Kawa: keep Subfield for crosswalk
kawa <- kawa |>
  mutate(subfield = as.character(subfield)) |>
  filter(!is.na(subfield) & subfield != "")

## ----------------
## Quick descriptives (for tables)
## ----------------
desc_all <- lineage |>
  summarise(
    n = n(),
    pct_prestige_utk = mean(is_prestige_utk, na.rm = TRUE),
    pct_inbred = mean(is_inbred_self_hire, na.rm = TRUE),
    pct_female = mean(gender == "Female", na.rm = TRUE),
    h_median = median(h_index, na.rm = TRUE),
    h_iqr_l = quantile(h_index, 0.25, na.rm = TRUE),
    h_iqr_u = quantile(h_index, 0.75, na.rm = TRUE),
    hpy_median = median(h_per_year, na.rm = TRUE),
    hpy_iqr_l = quantile(h_per_year, 0.25, na.rm = TRUE),
    hpy_iqr_u = quantile(h_per_year, 0.75, na.rm = TRUE)
  )

desc_by_decade <- lineage %>%
  group_by(decade) %>%
  summarise(
    n = n(),
    pct_inbred = mean(is_inbred_self_hire, na.rm = TRUE),
    pct_prestige_utk = mean(is_prestige_utk, na.rm = TRUE),
    hpy_med = median(h_per_year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(decade)

desc_by_insttype <- lineage |>
  group_by(institution_type) |>
  summarise(
    n = n(),
    hpy_median = median(h_per_year, na.rm = TRUE),
    h_median = median(h_index, na.rm = TRUE),
    pct_inbred = mean(is_inbred_self_hire, na.rm = TRUE),
    pct_prestige_utk = mean(is_prestige_utk, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(n))

## ----------------
## Core models
## ----------------
# Model A: normalized productivity
m1 <- lm(h_per_year ~ is_prestige_utk + is_inbred_self_hire + decade +
           institution_type + gender, data = lineage)
m1_tidy <- tidy(m1, conf.int = TRUE)

# Model B: raw h-index with years since PhD
m2 <- lm(h_index ~ is_prestige_utk + is_inbred_self_hire + years_since_phd +
           institution_type + gender, data = lineage)
m2_tidy <- tidy(m2, conf.int = TRUE)

# Inbreeding trend over time (logistic)
lineage_logit <- lineage |>
  filter(!is.na(is_inbred_self_hire), !is.na(decade)) |>
  mutate(decade_f = fct_inorder(decade))
m3 <- glm(is_inbred_self_hire ~ decade_f + gender, data = lineage_logit,
          family = binomial())
m3_tidy <- tidy(m3, conf.int = TRUE, exponentiate = TRUE)  # ORs

## ----------------
## Subfield crosswalk to Kawa (simple)
## ----------------
kawa_subfield_counts <- kawa |>
  count(subfield, name = "kawa_n")

lineage_field_counts <- lineage |>
  count(field, name = "lineage_n")

subfield_join <- lineage_field_counts |>
  rename(subfield = field) |>
  full_join(kawa_subfield_counts, by = "subfield") |>
  arrange(desc(coalesce(lineage_n, 0L)))

## ----------------
## Plots (AAFS-ready starters)
## ----------------

# 1) Academic inbreeding over time
p_inbred_time <- desc_by_decade |>
  ggplot(aes(x = decade, y = pct_inbred)) +
  geom_line(aes(group = 1)) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Academic Self-Hire (Inbreeding) Over Time",
    x = "PhD Graduation Decade",
    y = "Self-Hire Rate"
  ) +
  theme_minimal(base_size = 12)

# 2) Prestige vs h_per_year
p_prestige_hpy <- lineage |>
  filter(!is.na(is_prestige_utk)) |>
  mutate(prestige_label = if_else(is_prestige_utk, "UTK Prestige", "Non-UTK")) |>
  ggplot(aes(x = prestige_label, y = h_per_year)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  labs(
    title = "Prestige Gradient in Normalized Productivity",
    x = NULL, y = "h-index per career year"
  ) +
  theme_minimal(base_size = 12)

# 4) Model A estimates (coefs with CI)
coef_plot <- function(tidy_df, title_txt){
  tidy_df |>
    filter(term != "(Intercept)") |>
    mutate(term = str_replace_all(term, "is_prestige_utkTRUE", "UTK prestige"),
           term = str_replace_all(term, "is_inbred_self_hireTRUE", "Self-hire"),
           term = str_replace_all(term, "decade", "Decade: "),
           term = str_replace_all(term, "institution_type", "Inst. type: "),
           term = str_replace_all(term, "gender", "Gender: "),
           term = str_replace_all(term, "_", " ")) |>
    mutate(term = fct_reorder(term, estimate, .desc = FALSE)) |>
    ggplot(aes(x = estimate, y = term)) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0) +
    geom_vline(xintercept = 0, linetype = 2) +
    labs(title = title_txt, x = "Estimate (95% CI)", y = NULL) +
    theme_minimal(base_size = 12)
}

p_m1 <- coef_plot(m1_tidy, "Model A: h_per_year ~ lineage & covariates")
p_m2 <- coef_plot(m2_tidy, "Model B: h_index ~ lineage & covariates (+years_since_phd)")

## ----------------
## Save outputs (optional)
## ----------------
# ggsave("inbreeding_over_time.png", p_inbred_time, width = 7, height = 4, dpi = 300)
# ggsave("prestige_vs_hpy.png",  p_prestige_hpy,   width = 5, height = 4, dpi = 300)
# ggsave("insttype_hpy.png",     p_insttype_hpy,   width = 6, height = 4, dpi = 300)
# ggsave("modelA_coefplot.png",  p_m1,             width = 6, height = 4, dpi = 300)
# ggsave("modelB_coefplot.png",  p_m2,             width = 6, height = 4, dpi = 300)

## Inspect in console
desc_all
desc_by_decade
desc_by_insttype
m1_tidy
m2_tidy
m3_tidy
subfield_join
p_m1
p_m2
p_prestige_hpy
p_inbred_time

## =============================================================================
## LINEAGE NETWORK ANALYSIS: depth, descendants, and binary descent prestige (UTK)
## =============================================================================

# ---- 1) Construct advisor → advisee edges with stable IDs --------------------
# Person ID helper
.person_id <- function(last, first) {
  paste0(str_squish(str_to_title(last %||% "")), ", ",
         str_squish(str_to_title(first %||% "")))
}

# create id_researcher/id_advisor
lineage_ids <- lineage |>
  mutate(
    id_researcher = .person_id(researcher_last, researcher_first),
    id_advisor    = .person_id(advisor_last, advisor_first)
  )

# define known UTK advisor names
utk_names <- c("Bass, William", "Jantz, Richard", "Steadman, Dawnie", "Konigsberg, Lyle", "Marks, Murray", "Jantz, Lee", "Smith, Fred")
# apply UTK prestige fix here (on lineage_ids, not lineage)
lineage_ids <- lineage_ids %>%
  mutate(is_prestige_utk = ifelse(id_researcher %in% utk_names | is_prestige_utk, TRUE, is_prestige_utk))
edges <- lineage_ids |>
  filter(!is.na(id_advisor), id_advisor != "", !is.na(id_researcher), id_researcher != "",
         id_advisor != id_researcher) |>
  distinct(id_advisor, id_researcher) |>
  rename(from = id_advisor, to = id_researcher)

# Create node set = all unique people appearing anywhere
node_ids <- union(lineage_ids$id_researcher, lineage_ids$id_advisor) |> unique()
nodes <- tibble(name = node_ids)

# Build directed graph (advisor -> advisee)
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

# Attach UTK prestige to vertices (if present in your lineage df for those people)
# Map person-level attributes from lineage (researcher rows) to vertices:
person_attr <- lineage_ids |>
  transmute(
    name = id_researcher,
    is_prestige_utk = is_prestige_utk,
    is_inbred_self_hire = is_inbred_self_hire,
    h_index = h_index,
    h_per_year = h_per_year,
    decade = decade,
    gender = gender,
    institution_type = institution_type
  ) |>
  distinct(name, .keep_all = TRUE)

# Join attributes to vertex data
vdat <- tibble(name = V(g)$name) |>
  left_join(person_attr, by = "name")

# Backfill missing prestige flag with FALSE (unknowns stay NA; here we use FALSE so ancestor logic is conservative)
vdat$is_prestige_utk <- ifelse(is.na(vdat$is_prestige_utk), FALSE, vdat$is_prestige_utk)

# ---- 2) Lineage depth (generations from a root) ------------------------------
# Roots = mentors with no incoming edges
roots <- V(g)[degree(g, mode = "in") == 0]

# Distance from ANY root to each node (choose the minimum)
# If a node is unreachable from roots (odd cycles or isolates), depth = NA
dist_mat <- distances(g, v = roots, to = V(g), mode = "out")
depth_vec <- apply(dist_mat, 2, function(x){
  # x = distances from all roots to this node
  best <- suppressWarnings(min(x[is.finite(x)]))
  if (length(best) == 0 || is.infinite(best)) NA_integer_ else as.integer(best)
})

# ---- 3) Descendants: direct and total ---------------------------------------
# Direct descendants = out-degree
direct_desc <- degree(g, mode = "out")

# Total descendants = size of reachable set (excluding self).
# Compute reachability from each node once using distances to all others.
dist_all <- distances(g, mode = "out")
total_desc <- apply(dist_all, 1, function(x) sum(is.finite(x) & x > 0))

# ---- 4) Descent prestige (binary): any UTK ancestor? -------------------------
# Efficient method: compute distances from all UTK vertices to all others;
# if a UTK vertex can reach node v (distance >=1 and finite), then v has UTK ancestor.
utk_vertices <- which(vdat$is_prestige_utk %in% TRUE)
has_utk_ancestor <- rep(NA, vcount(g))

if (length(utk_vertices) == 0) {
  # No UTK ancestors recorded anywhere
  has_utk_ancestor[] <- FALSE
} else {
  # Distances from UTK nodes to all; if any UTK can reach v with path length >=1, then v has UTK ancestor
  dist_from_utk <- distances(g, v = utk_vertices, to = V(g), mode = "out")
  reachable_from_utk <- apply(dist_from_utk, 2, function(col) any(is.finite(col) & col >= 1))
  has_utk_ancestor <- as.logical(reachable_from_utk)
}

# Also compute nearest UTK ancestor distance (smallest steps from any UTK → v; NA if none)
nearest_utk_distance <- rep(NA_integer_, vcount(g))
if (length(utk_vertices) > 0) {
  min_dist <- apply(dist_from_utk, 2, function(col) {
    d <- suppressWarnings(min(col[is.finite(col) & col >= 1]))
    if (length(d) == 0 || is.infinite(d)) NA_integer_ else as.integer(d)
  })
  nearest_utk_distance <- min_dist
}

# ---- 5) Assemble vertex metrics and merge back to lineage --------------------
v_metrics <- tibble(
  name = vdat$name,
  lineage_depth = as.integer(depth_vec),
  direct_descendants = as.integer(direct_desc),
  total_descendants = as.integer(total_desc),
  descent_prestige_binary = as.logical(has_utk_ancestor),
  utk_ancestor_distance = as.integer(nearest_utk_distance)
)

# Merge vertex metrics onto researcher rows in lineage using id_researcher
lineage_enriched <- lineage_ids |>
  left_join(v_metrics, by = c("id_researcher" = "name")) |>
  # Keep your original columns first, then appended metrics
  relocate(lineage_depth, direct_descendants, total_descendants,
           descent_prestige_binary, utk_ancestor_distance, .after = last_col())

# Replace working object for downstream models/plots
lineage <- lineage_enriched

## ---- 6) Models including new lineage metrics --------------------------------
# Model C: add lineage_depth and descent_prestige_binary to normalized productivity
mC <- lm(h_per_year ~ is_prestige_utk + is_inbred_self_hire +
           lineage_depth + descent_prestige_binary +
           decade + institution_type + gender,
         data = lineage)

mC_tidy <- broom::tidy(mC, conf.int = TRUE)

# Optional: nearest UTK ancestor distance as a dose-like effect (smaller = closer)
# (Note: where no UTK ancestor, distance is NA; dropping NAs here)
mD <- lm(h_per_year ~ is_prestige_utk + is_inbred_self_hire +
           lineage_depth + utk_ancestor_distance +
           decade + institution_type + gender,
         data = lineage)

mD_tidy <- broom::tidy(mD, conf.int = TRUE)

## ---- 7) AAFS-ready plots for new variables ----------------------------------
# Depth vs productivity (smoothed)
p_depth_hpy <- lineage |>
  filter(!is.na(lineage_depth), !is.na(h_per_year)) |>
  ggplot(aes(x = lineage_depth, y = h_per_year)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Lineage Depth vs. Normalized Productivity",
    x = "Lineage depth (generations from root)",
    y = "h-index per career year"
  ) +
  theme_minimal(base_size = 12)

# Binary descent prestige vs productivity
p_descent_prestige <- lineage |>
  mutate(dprest = if_else(descent_prestige_binary, "Has UTK ancestor", "No UTK ancestor")) |>
  ggplot(aes(x = dprest, y = h_per_year)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  labs(
    title = "Descent Prestige (UTK ancestor) and Normalized Productivity",
    x = NULL, y = "h-index per career year"
  ) +
  theme_minimal(base_size = 12)

# Coef plot for Model C (reuse your coef_plot helper)
p_mC <- coef_plot(mC_tidy, "Model C: h_per_year ~ lineage depth + UTK descent + covariates")

## ---- 8) Console checks -------------------------------------------------------
mC_tidy
mD_tidy
p_depth_hpy
p_descent_prestige
p_mC



grid.arrange(
  p_inbred_time,          # Academic self-hire over time
  p_prestige_hpy,         # Prestige gradient
  p_depth_hpy,            # Lineage depth vs. normalized productivity
  p_descent_prestige,     # Descent prestige (UTK ancestor)
  ncol = 2,
  nrow = 2
)
###############################################################################
## =============================================================================
## LINEAGE STRUCTURE CHARACTERIZATION
## =============================================================================

# ---- 1) Identify roots and root ancestors -----------------------------------
# Find all root nodes (no incoming edges)
roots <- V(g)[degree(g, mode = "in") == 0]$name

# Corrected helper: trace upward using igraph::neighbors() instead of predecessors()
get_root_ancestor <- function(node, g) {
  preds <- neighbors(g, node, mode = "in")
  if (length(preds) == 0) return(V(g)[node]$name)
  while (length(preds) > 0) {
    node <- preds[1]
    preds <- neighbors(g, node, mode = "in")
  }
  V(g)[node]$name
}

# Apply helper to every node
root_ancestor_vec <- sapply(V(g), function(v) get_root_ancestor(v, g))

# ---- 2) Compute node-level metrics ------------------------------------------
# Direct descendants (out-degree)
direct_desc <- degree(g, mode = "out")

# Total descendants = size of reachable set (excluding self)
dist_all <- distances(g, mode = "out")
total_desc <- apply(dist_all, 1, function(x) sum(is.finite(x) & x > 0))

# Retrieve depth_vec from earlier (computed as distance from roots)
# If not available, recompute it:
if (!exists("depth_vec")) {
  dist_mat <- distances(g, v = roots, to = V(g), mode = "out")
  depth_vec <- apply(dist_mat, 2, function(x){
    best <- suppressWarnings(min(x[is.finite(x)]))
    if (length(best) == 0 || is.infinite(best)) NA_integer_ else as.integer(best)
  })
}

# Combine into a vertex structure table
v_structure <- tibble(
  name = V(g)$name,
  lineage_depth = as.integer(depth_vec),
  direct_descendants = as.integer(direct_desc),
  total_descendants = as.integer(total_desc),
  root_ancestor = root_ancestor_vec,
  is_root = name %in% roots
)

# Merge in researcher-level metadata (prestige, gender, h-index, etc.)
v_structure <- v_structure %>%
  left_join(
    lineage %>%
      select(id_researcher, h_index, h_per_year,
             is_prestige_utk, descent_prestige_binary,
             institution_type, gender, field),
    by = c("name" = "id_researcher")
  )

# Now safely compute descendant density
v_structure <- v_structure %>%
  mutate(descendant_density = direct_descendants / (lineage_depth + 1))

# ---- 3) Network-level summaries ---------------------------------------------
network_summary <- list(
  total_nodes = vcount(g),
  total_edges = ecount(g),
  n_roots = length(roots),
  avg_depth = mean(v_structure$lineage_depth, na.rm = TRUE),
  max_depth = max(v_structure$lineage_depth, na.rm = TRUE),
  avg_direct_descendants = mean(v_structure$direct_descendants, na.rm = TRUE),
  avg_total_descendants = mean(v_structure$total_descendants, na.rm = TRUE)
)
network_summary

# Top 10 advisors by total descendants
top_advisors <- v_structure %>%
  arrange(desc(total_descendants)) %>%
  slice_head(n = 10) %>%
  select(name, total_descendants, h_index, is_prestige_utk, institution_type)

# ---- 4) Visualizations -------------------------------------------------------

# (a) Histogram of lineage depth
p_depth_hist <- v_structure %>%
  ggplot(aes(x = lineage_depth)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Lineage Depth",
       x = "Academic generation (depth from root)",
       y = "Count of scholars") +
  theme_minimal(base_size = 12)

# (b) Top advisors by total descendants
p_top_advisors <- top_advisors %>%
  mutate(name = fct_reorder(name, total_descendants)) %>%
  ggplot(aes(x = total_descendants, y = name, fill = is_prestige_utk)) +
  geom_col() +
  scale_fill_manual(values = c("gray70", "goldenrod"), name = "UTK Prestige") +
  labs(
    title = "Top Advisors by Total Academic Descendants",
    x = "Total descendants", y = NULL
  ) +
  theme_minimal(base_size = 12)

# (c) (Optional) Quick network plot (simplified)
set.seed(42)
# Attach UTK prestige attribute to graph vertices (for plotting)
V(g)$is_prestige_utk <- v_structure$is_prestige_utk[match(V(g)$name, v_structure$name)]
p_network_simple <- ggraph::ggraph(g, layout = "tree") +
  ggraph::geom_edge_link(alpha = 0.3) +
  ggraph::geom_node_point(aes(color = is_prestige_utk), size = 2) +
  scale_color_manual(values = c("gray40", "goldenrod")) +
  theme_void() +
  labs(title = "Academic Lineage Network of Forensic Anthropology")

# ---- 5) Inspect --------------------------------------------------------------
network_summary
top_advisors
p_depth_hist
p_top_advisors
p_network_simple  

## =============================================================================
## ACADEMIC FAMILY (ROOT ANCESTOR) SUMMARIES
## =============================================================================

# Summarize lineage families
family_summary <- v_structure %>%
  group_by(root_ancestor) %>%
  summarise(
    n_members = n(),
    avg_depth = mean(lineage_depth, na.rm = TRUE),
    max_depth = max(lineage_depth, na.rm = TRUE),
    n_prestige_utk = sum(is_prestige_utk, na.rm = TRUE),
    avg_h_per_year = mean(h_per_year, na.rm = TRUE),
    avg_h_index = mean(h_index, na.rm = TRUE),
    pct_inbred = mean(lineage_depth == 0, na.rm = TRUE),  # proxy if needed
    pct_with_utk_ancestor = mean(descent_prestige_binary, na.rm = TRUE),
    avg_descendants_per_member = mean(total_descendants, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_members))

# Identify major families (top 10 by size)
top_families <- family_summary %>%
  slice_max(order_by = n_members, n = 10)

# ---- Plot 1: Size of academic lineages ----
p_family_size <- family_summary %>%
  filter(!is.na(root_ancestor)) %>%   # drop NAs
  mutate(root_ancestor = as.factor(root_ancestor),
         root_ancestor = fct_reorder(root_ancestor, n_members)) %>%
  ggplot(aes(x = n_members, y = root_ancestor)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Size of Academic Lineages (Root Ancestors)",
    x = "Number of Members",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

# ---- Plot 2: Productivity by lineage family (top 10) ----
p_family_productivity <- top_families %>%
  filter(!is.na(root_ancestor)) %>%   # drop NAs
  mutate(root_ancestor = as.factor(root_ancestor),
         root_ancestor = fct_reorder(root_ancestor, avg_h_per_year)) %>%
  ggplot(aes(x = avg_h_per_year, y = root_ancestor)) +
  geom_col(fill = "darkseagreen4") +
  labs(
    title = "Mean Normalized Productivity by Lineage Family (Top 10)",
    x = "Mean h-index per career year",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

p_family_productivity_colored <- top_families %>%
  # Drop or label missing ancestors
  mutate(root_ancestor = ifelse(is.na(root_ancestor) | root_ancestor == "NA, NA",
                                "Unknown", as.character(root_ancestor))) %>%
  # Coerce to factor safely AFTER it's all text
  mutate(root_ancestor = forcats::fct_reorder(as.factor(root_ancestor), avg_h_per_year)) %>%
  ggplot(aes(x = avg_h_per_year, y = root_ancestor, fill = n_members)) +
  geom_col() +
  scale_fill_gradient(low = "darkseagreen3", high = "darkolivegreen4") +
  labs(
    title = "Mean Normalized Productivity by Lineage Family (Top 10)",
    x = "Mean h-index per career year",
    y = NULL,
    fill = "Lineage size"
  ) +
  theme_minimal(base_size = 12)




# Inspect
family_summary
p_family_size
p_family_productivity

## =============================================================================
## TOP 10 OVERALL ADVISORS BY DESCENDANTS
## =============================================================================

# Identify advisors with at least one recorded student
advisor_summary <- v_structure %>%
  filter(direct_descendants > 0) %>%
  arrange(desc(direct_descendants)) %>%
  mutate(advisor_label = name)

# Get top 10 advisors
top_advisors_overall <- advisor_summary %>%
  slice_head(n = 10)

# Plot 1: Top advisors by direct descendants
p_top_advisors_direct <- top_advisors_overall %>%
  mutate(advisor_label = fct_reorder(advisor_label, direct_descendants)) %>%
  ggplot(aes(x = direct_descendants, y = advisor_label, fill = is_prestige_utk)) +
  geom_col() +
  scale_fill_manual(values = c("gray60", "goldenrod"), name = "UTK Prestige") +
  labs(
    title = "Top 10 Advisors by Number of Direct Advisees",
    x = "Direct descendants (students)",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

# Plot 2: (Optional) Top advisors by total descendants (full downstream influence)
top_advisors_total <- v_structure %>%
  arrange(desc(total_descendants)) %>%
  slice_head(n = 10)

p_top_advisors_total <- top_advisors_total %>%
  mutate(advisor_label = fct_reorder(name, total_descendants)) %>%
  ggplot(aes(x = total_descendants, y = advisor_label, fill = is_prestige_utk)) +
  geom_col() +
  scale_fill_manual(values = c("gray60", "goldenrod"), name = "UTK Prestige") +
  labs(
    title = "Top 10 Advisors by Total Academic Descendants",
    x = "Total descendants (direct + indirect)",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

# Inspect results
top_advisors_overall
top_advisors_total
p_top_advisors_direct
p_top_advisors_total


## =============================================================================
## MENTOR–MENTEE PRODUCTIVITY ANALYSIS
## =============================================================================

# 1) Build a person-level lookup (advisor metrics)
person_lookup <- lineage_ids %>%
  select(id_researcher, h_index, h_per_year, years_since_phd, decade,
         is_prestige_utk, is_inbred_self_hire, gender, institution_type, field) %>%
  distinct(id_researcher, .keep_all = TRUE)

# 2) Create mentor–mentee dyads by joining advisor attributes onto advisee rows
dyads <- lineage_ids %>%
  filter(!is.na(id_advisor) & id_advisor != "" & id_advisor != id_researcher) %>%
  # advisee side
  rename(advisee_id = id_researcher) %>%
  left_join(
    person_lookup %>%
      rename(advisor_id = id_researcher,
             h_index_advisor = h_index,
             h_per_year_advisor = h_per_year,
             years_since_phd_advisor = years_since_phd,
             decade_advisor = decade,
             is_prestige_utk_advisor = is_prestige_utk,
             is_inbred_self_hire_advisor = is_inbred_self_hire,
             gender_advisor = gender,
             institution_type_advisor = institution_type,
             field_advisor = field),
    by = c("id_advisor" = "advisor_id")
  ) %>%
  # advisee attributes (from person_lookup)
  left_join(
    person_lookup %>%
      rename(h_index_advisee = h_index,
             h_per_year_advisee = h_per_year,
             years_since_phd_advisee = years_since_phd,
             decade_advisee = decade,
             is_prestige_utk_advisee = is_prestige_utk,
             is_inbred_self_hire_advisee = is_inbred_self_hire,
             gender_advisee = gender,
             institution_type_advisee = institution_type,
             field_advisee = field),
    by = c("advisee_id" = "id_researcher")
  )

# 3) Keep complete cases for the core relationship
dyads_core <- dyads %>%
  filter(!is.na(h_per_year_advisee),
         !is.na(h_per_year_advisor))

# Quick sanity check
summary(nrow(dyads_core))
summary(dyads_core$h_per_year_advisee)
summary(dyads_core$h_per_year_advisor)

# 4) Baseline OLS: Does mentor productivity predict mentee productivity?
m_base <- lm(h_per_year_advisee ~ h_per_year_advisor, data = dyads_core)
m_base_tidy <- tidy(m_base, conf.int = TRUE)

# 5) Adjusted OLS: add covariates (advisee-side + advisor UTK)
m_adj <- lm(
  h_per_year_advisee ~ h_per_year_advisor +
    is_prestige_utk_advisee + is_inbred_self_hire_advisee +
    years_since_phd_advisee + institution_type_advisee + gender_advisee +
    is_prestige_utk_advisor + years_since_phd_advisor + decade_advisee,
  data = dyads_core
)
m_adj_tidy <- tidy(m_adj, conf.int = TRUE)

# 6) (Optional) Cluster-robust SEs by advisor (multiple mentees per advisor)
# install.packages(c("sandwich","lmtest")) if needed
# library(sandwich); library(lmtest)
# vcov_cl <- sandwich::vcovCL(m_adj, cluster = ~ id_advisor, type = "HC1")
# m_adj_crse <- lmtest::coeftest(m_adj, vcov. = vcov_cl)

# 7) (Optional) Mixed model with advisor random intercept
# install.packages("lme4") if needed
# library(lme4)
# m_mixed <- lmer(
#   h_per_year_advisee ~ h_per_year_advisor +
#     is_prestige_utk_advisee + is_inbred_self_hire_advisee +
#     years_since_phd_advisee + institution_type_advisee + gender_advisee +
#     is_prestige_utk_advisor + years_since_phd_advisor + decade_advisee +
#     (1 | id_advisor),
#   data = dyads_core
# )

# 8) Publishable scatter with smooth (mentor vs mentee)
p_mentor_vs_mentee <- dyads_core %>%
  ggplot(aes(x = h_per_year_advisor, y = h_per_year_advisee)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Mentor vs. Mentee Normalized Productivity",
    x = "Advisor h-index per career year",
    y = "Advisee h-index per career year"
  ) +
  theme_minimal(base_size = 12)


glance(m_base)[, c("r.squared", "adj.r.squared", "p.value")]

p_mentor_vs_mentee +
  geom_point(aes(color = is_prestige_utk_advisor)) +
  scale_color_manual(values = c("gray40", "goldenrod"), name = "UTK Advisor")

dyads_core %>%
  mutate(advisor_quintile = ntile(h_per_year_advisor, 5)) %>%
  group_by(advisor_quintile) %>%
  summarise(
    n = n(),
    mean_mentor = mean(h_per_year_advisor, na.rm = TRUE),
    mean_mentee = mean(h_per_year_advisee, na.rm = TRUE)
  )

p_mentor_vs_mentee +
  geom_point(aes(color = is_prestige_utk_advisor)) +
  scale_color_manual(values = c("gray40", "goldenrod"), name = "UTK Advisor")


#for AAFS:
# 1) Summarize by mentor productivity quintile
mentor_quintiles <- dyads_core %>%
  mutate(advisor_quintile = ntile(h_per_year_advisor, 5)) %>%
  group_by(advisor_quintile) %>%
  summarise(
    n = n(),
    mean_mentor = mean(h_per_year_advisor, na.rm = TRUE),
    sd_mentor   = sd(h_per_year_advisor, na.rm = TRUE),
    mean_mentee = mean(h_per_year_advisee, na.rm = TRUE),
    sd_mentee   = sd(h_per_year_advisee, na.rm = TRUE),
    se_mentee   = sd_mentee / sqrt(n),
    .groups = "drop"
  )

# 2) Plot with error bars (±1 SE)
p_quintile_means <- mentor_quintiles %>%
  ggplot(aes(x = mean_mentor, y = mean_mentee)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(aes(ymin = mean_mentee - se_mentee,
                    ymax = mean_mentee + se_mentee),
                width = 0.02, color = "gray40") +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen", linetype = 2) +
  labs(
    title = "Mean Mentee Productivity by Mentor Productivity Quintile",
    x = "Mean Mentor h-index per career year (quintile mean)",
    y = "Mean Mentee h-index per career year ± 1 SE"
  ) +
  theme_minimal(base_size = 12) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+
  aes(color = factor(advisor_quintile)) +
  scale_color_brewer(palette = "Blues", name = "Mentor Quintile")


# 3) View
p_quintile_means 
# 9) (Optional) Facet by advisor UTK status or advisor lineage size
# p_mentor_vs_mentee + facet_wrap(~ is_prestige_utk_advisor)

# 10) Print summaries
m_base_tidy
m_adj_tidy
# m_adj_crse   # if you ran cluster-robust
# summary(m_mixed)  # if you ran mixed model
p_mentor_vs_mentee

grid.arrange(
  p_mentor_vs_mentee,          
  p_quintile_means,         # Prestige gradient
  ncol = 1,
  nrow = 2
)
###############################################################################
# figures
###############################################################################
## FIGURE 1: Inbreeding & normalized productivity by decade

# Panel A: self-hire (already very close to your p_inbred_time)
fig1A_inbred <- desc_by_decade %>%
  ggplot(aes(x = decade, y = pct_inbred)) +
  geom_line(aes(group = 1)) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Academic Self-Hire (Inbreeding) Over Time",
    x = "PhD Graduation Decade",
    y = "Self-hire rate"
  ) +
  theme_minimal(base_size = 12)

# Panel B: median h_per_year by decade
fig1B_hpy <- desc_by_decade %>%
  ggplot(aes(x = decade, y = hpy_med)) +
  geom_line(aes(group = 1)) +
  geom_point(size = 2) +
  labs(
    title = "Normalized Productivity Over Time",
    x = "PhD Graduation Decade",
    y = "Median h-index per career year"
  ) +
  theme_minimal(base_size = 12)

# Combined Figure 1 object
library(gridExtra)
fig1 <- grid.arrange(fig1A_inbred, fig1B_hpy, ncol = 1)
###############################################################################
y_max <- 3    # tweak to 2.5 / 3.5 as you like

## -------------------------------
## FIGURE 2: UTK prestige vs productivity
## -------------------------------

fig2_prestige <- lineage %>%
  filter(!is.na(is_prestige_utk)) %>%
  mutate(prestige_label = if_else(is_prestige_utk, "UTK prestige", "Non-UTK"),
         prestige_label = factor(prestige_label,
                                 levels = c("Non-UTK", "UTK prestige"))) %>%
  ggplot(aes(x = prestige_label, y = h_per_year)) +
  geom_violin(trim = TRUE,
              width = 0.9,
              fill  = "grey85",
              color = "grey40") +
  geom_boxplot(width = 0.15,
               outlier.shape = NA,
               fill = "white",
               color = "grey20",
               linewidth = 0.5) +
  coord_cartesian(ylim = c(0, y_max)) +
  labs(
    title = "Normalized Productivity by UTK Prestige Status",
    x = NULL,
    y = "h-index per career year"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(face = "bold", hjust = 0.5)
  )

## -------------------------------
## FIGURE 4: Descent prestige vs productivity
## -------------------------------

fig4_descent_prestige <- lineage %>%
  mutate(dprest = if_else(descent_prestige_binary,
                          "Has UTK ancestor",
                          "No UTK ancestor"),
         dprest = factor(dprest,
                         levels = c("No UTK ancestor", "Has UTK ancestor"))) %>%
  ggplot(aes(x = dprest, y = h_per_year)) +
  geom_violin(trim = TRUE,
              width = 0.9,
              fill  = "grey85",
              color = "grey40") +
  geom_boxplot(width = 0.15,
               outlier.shape = NA,
               fill = "white",
               color = "grey20",
               linewidth = 0.5) +
  coord_cartesian(ylim = c(0, y_max)) +
  labs(
    title = "Descent Prestige (UTK Ancestor) and Normalized Productivity",
    x = NULL,
    y = "h-index per career year"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(face = "bold", hjust = 0.5)
  )

## -------------------------------
## Arrange stacked
## -------------------------------

fig2 <- grid.arrange(fig2_prestige, fig4_descent_prestige, ncol = 1)

###############################################################################
## FIGURE 3: Lineage depth vs normalized productivity

fig3_depth_hpy <- lineage %>%
  filter(!is.na(lineage_depth), !is.na(h_per_year)) %>%
  ggplot(aes(x = lineage_depth, y = h_per_year)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Lineage Depth vs. Normalized Productivity",
    x = "Lineage depth (generations from root)",
    y = "h-index per career year"
  ) +
  theme_minimal(base_size = 12)
###############################################################################

## FIGURE 5: Size of academic lineages

fig5_family_size <- family_summary %>%
  filter(!is.na(root_ancestor)) %>%
  mutate(root_ancestor = as.factor(root_ancestor),
         root_ancestor = forcats::fct_reorder(root_ancestor, n_members)) %>%
  ggplot(aes(x = n_members, y = root_ancestor)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Size of Academic Lineages (Root Ancestors)",
    x = "Number of members",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

fig5_family_size
###############################################################################

## FIGURE 6: Mean normalized productivity by lineage family (top 10)

fig6_family_prod <- top_families %>%
  mutate(root_ancestor = ifelse(is.na(root_ancestor) | root_ancestor == "NA, NA",
                                "Unknown",
                                as.character(root_ancestor))) %>%
  mutate(root_ancestor = forcats::fct_reorder(as.factor(root_ancestor), avg_h_per_year)) %>%
  ggplot(aes(x = avg_h_per_year, y = root_ancestor, fill = n_members)) +
  geom_col() +
  scale_fill_gradient(low = "darkseagreen3", high = "darkolivegreen4") +
  labs(
    title = "Mean Normalized Productivity by Lineage Family (Top 10)",
    x = "Mean h-index per career year",
    y = NULL,
    fill = "Lineage size"
  ) +
  theme_minimal(base_size = 12)

fig6_family_prod
###############################################################################

## FIGURE 7: Mentor vs mentee normalized productivity

fig7_mentor_mentee <- dyads_core %>%
  ggplot(aes(x = h_per_year_advisor, y = h_per_year_advisee)) +
  geom_point(aes(color = is_prestige_utk_advisor), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("gray40", "goldenrod"),
                     name = "UTK-affiliated advisor") +
  labs(
    title = "Mentor vs. Mentee Normalized Productivity",
    x = "Advisor h-index per career year",
    y = "Advisee h-index per career year"
  ) +
  theme_minimal(base_size = 12)+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

fig7_mentor_mentee

###############################################################################
## FIGURE 8: Mean mentee productivity by mentor productivity quintile

library(ggplot2)
library(dplyr)

fig8_quintile_means <- mentor_quintiles %>%
  ggplot(aes(x = mean_mentor,
             y = mean_mentee,
             group = factor(advisor_quintile))) +
  
  # error bars first (behind points)
  geom_errorbar(
    aes(ymin = mean_mentee - se_mentee,
        ymax = mean_mentee + se_mentee,
        color = factor(advisor_quintile)),
    width = 0.05, linewidth = 1.1, alpha = 0.9
  ) +
  
  # points with outline for contrast
  geom_point(
    aes(fill = factor(advisor_quintile)),
    shape = 21, size = 5, stroke = 0.9, color = "grey15", alpha = 0.98
  ) +
  
  # labels (since you already label quintiles, legend can be optional)
  ggrepel::geom_text_repel(
    aes(label = advisor_quintile),
    size = 4.5,
    fontface = "bold",
    color = "grey10",
    box.padding = 0.25,
    point.padding = 0.25,
    min.segment.length = 0,
    segment.color = "grey60",
    show.legend = FALSE
  ) +
  
  # High-contrast, print-safe palette (dark to light)
  scale_fill_manual(
    values = c("1" = "#08306B", "2" = "#08519C", "3" = "#2171B5", "4" = "#4292C6", "5" = "#6BAED6"),
    name = "Mentor\nquintile"
  ) +
  scale_color_manual(
    values = c("1" = "#08306B", "2" = "#08519C", "3" = "#2171B5", "4" = "#4292C6", "5" = "#6BAED6"),
    name = "Mentor\nquintile"
  ) +
  
  labs(
    title = "Mentee productivity increases with mentor productivity",
    x = "Mean mentor h-index per career year (quintile mean)",
    y = "Mean mentee h-index per career year (±1 SE)"
  ) +
  
  scale_x_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0.01, 0.04))
  ) +
  scale_y_continuous(
    limits = c(0.35, NA),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 10)),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none",  # since quintiles are labeled on the plot
    plot.margin = margin(10, 12, 10, 12)
  )


fig8_quintile_means
###############################################################################
library(modelsummary)

models <- list(
  "Model 1\n(h_per_year)" = m1,
  "Model 2\n(h_index)"    = m2,
  "Model C\n(h_per_year)" = mC,
  "Model D\n(h_per_year)" = mD
)

modelsummary(
  models,
  estimate  = "{estimate} ({std.error})",
  statistic = "[p = {p.value}]",
  stars = TRUE,
  gof_omit = "IC|Log|RMSE",
  coef_rename = c(
    "is_prestige_utkTRUE"      = "UTK prestige",
    "is_inbred_self_hireTRUE"  = "Self-hire (academic inbreeding)",
    "lineage_depth"            = "Lineage depth",
    "descent_prestige_binaryTRUE" = "UTK descent prestige (binary)",
    "utk_ancestor_distance"    = "UTK ancestor distance",
    "years_since_phd"          = "Years since PhD",
    "years_since_phd_advisee"  = "Years since PhD (advisee)",
    "genderMale"               = "Gender: Male",
    "institution_typeUniversity" = "Institution type: University"
  ),
  fmt = 3,
  output = "data.frame" # or "kableExtra", "gt", "latex", etc.
)
###############################################################################
# measuring prestige bias using new variable job_prestige:
#  | Tier | Label                                             | Included positions                                                                                                               |
#  | ---- | ------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------- |
#  | 4    | **High-prestige, permanent research role**        | R1 tenure-track faculty; permanent federal scientist (DPAA GS-12+); museum curator/research scientist with permanent appointment |
#  | 3    | **Mid-prestige, stable academic or applied role** | R2/R3 TT faculty; permanent teaching faculty; long-term government scientist; senior SNA                                         |
#  | 2    | **Early-career / contingent research role**       | Postdocs; ORISE; VAPs; term-limited federal positions                                                                            |
#  | 1    | **Low-prestige / insecure role**                  | Adjunct; visiting lecturer; unrelated non-academic employment                                                                    |
###############################################################################  
# Ordered outcome (main model)
df<-lineage
df$job_prestige_ord <- factor(
  df$job_prestige,
  levels = c(2, 3, 4),
  ordered = TRUE
)

# binary (robustness check)
df$high_prestige_job <- ifelse(df$job_prestige >= 3, 1, 0)
df$utk_prestige <- as.numeric(df$prestige)
df$inbred       <- as.numeric(df$inbred)
###############################################################################  
#Core analysis: does prestige bias mean better jobs?

m1 <- polr(
  job_prestige_ord ~ h_index + utk_prestige + inbred + ph_d_year,
  data = df,
  Hess = TRUE
)

summary(m1)

###############################################################################  
#Model 2: Is scholarship rewarded differently for UTK grads?
m2 <- polr(
  job_prestige_ord ~ h_index * utk_prestige + inbred,
  data = df,
  Hess = TRUE
)

summary(m2)
###############################################################################  
#Over-placement analysis
#This reframes the result without modeling assumptions.

df$job_prestige_ord <- factor(df$job_prestige, levels = c(2,3,4), ordered = TRUE)

m_base <- polr(job_prestige_ord ~ h_index, data = df, Hess = TRUE)

summary(m_base)

# predicted probabilities for each tier
p <- predict(m_base, type = "probs")

lev <- as.numeric(colnames(p))

expected_prestige <- as.vector(p %*% lev)

observed_prestige <- as.numeric(as.character(df$job_prestige_ord))

# residual = observed - expected (positive = over-placed vs impact)
df$placement_resid <- observed_prestige - expected_prestige

summary(df$placement_resid)
aggregate(placement_resid ~ utk_prestige, data = df, mean, na.rm = TRUE)


df$utk_prestige_f <- factor(
  df$utk_prestige,
  levels = c(0, 1),
  labels = c("Non-UTK", "UTK")
)

ggplot(df, aes(x = utk_prestige_f, y = placement_resid, fill = utk_prestige_f)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_boxplot(
    width = 0.5,
    alpha = 0.6,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.35,
    size = 1
  ) +
  scale_fill_manual(
    values = c("grey70", "#FF8200")
  ) +
  labs(
    x = NULL,
    y = "Placement residual (observed − expected job prestige)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11),
    axis.title.y = element_text(size = 12)
  )

###############################################################################  

t.test(placement_resid ~ utk_prestige, data = df)
wilcox.test(placement_resid ~ utk_prestige, data = df)



# Prediction grid
pred_df <- expand.grid(
  h_index = seq(min(df$h_index, na.rm = TRUE),
                max(df$h_index, na.rm = TRUE),
                length.out = 200),
  utk_prestige = c(0, 1),
  inbred = 0,
  ph_d_year = median(df$ph_d_year, na.rm = TRUE)
)

# Predicted probabilities
probs <- as.data.frame(predict(m1, newdata = pred_df, type = "probs"))

plot_df <- bind_cols(pred_df, probs) |>
  pivot_longer(
    cols = c("2","3","4"),   # explicit is safer than starts_with() here
    names_to = "job_tier",
    values_to = "probability"
  ) |>
  mutate(
    job_tier = factor(
      job_tier,
      levels = c("2","3","4"),
      labels = c("Tier 2 (Contingent)", "Tier 3 (Stable)", "Tier 4 (High prestige)")
    ),
    utk_prestige = factor(utk_prestige, levels = c(0,1), labels = c("Non-UTK","UTK"))
  )

newFigure2 <- ggplot(
  plot_df,
  aes(x = h_index, y = probability,
      color = utk_prestige,
      linetype = job_tier)
) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("Non-UTK" = "grey55", "UTK" = "orange")) +
  scale_linetype_manual(values = c(
    "Tier 2 (Contingent)" = "dotted",
    "Tier 3 (Stable)"     = "dashed",
    "Tier 4 (High prestige)" = "solid"
  )) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(
    x = "Scholarly impact (h-index)",
    y = "Predicted probability",
    color = "Doctoral prestige",
    linetype = "Job tier"
    # (leave title out; let caption interpret)
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 11)
  )

newFigure2





