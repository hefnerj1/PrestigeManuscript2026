################################################################################
# TITLE:  AAFS 2026 - Passalacqua Network Dashboard (Ego-Kawa)
# AUTHORS: Joe Hefner, Nick Passalacqua, Natalie Clark [with Quill]
# LAST UPDATED: 2025-11-06  (V1.4: Institution Type color+analytics; fixed palette)
################################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(shiny)
  library(shinycssloaders)
  library(readxl)
  library(tidyverse)
  library(janitor)
  library(igraph)
  library(visNetwork)
  library(DT)
  library(scales)
  library(quantreg)
  library(ggplot2)
  library(colorspace)
  library(htmltools)
  library(gridExtra)
  library(circlize)
  library(ineq)
})

set.seed(42)
CURRENT_YEAR <- 2025

# ------------------------------------------------------------------------------
# Global journal-style ggplot theme
# ------------------------------------------------------------------------------
theme_journal <- function(base_size = 12, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(color = "#333333"),
      plot.title = element_text(face = "bold", size = base_size + 2, margin = margin(b = 6)),
      axis.title = element_text(size = base_size),
      axis.text  = element_text(size = base_size - 1, color = "gray25"),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
      panel.grid.minor = element_line(color = "gray95", linewidth = 0.2),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA)
    )
}
theme_set(theme_journal())

# ------------------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------------------
gini_index <- function(x){
  x <- as.numeric(x); x <- x[is.finite(x) & x >= 0]
  n <- length(x); if (n == 0) return(NA_real_)
  x <- sort(x); s <- sum(x); if (s == 0) return(0)
  (2 * sum(seq_len(n) * x) / (n * s)) - ((n + 1) / n)
}

safe_rescale <- function(x, to = c(0, 1)){
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || all(is.na(x))) return(rep(mean(to), length(x)))
  x[is.na(x)] <- 0
  if (length(unique(x)) == 1) return(rep(mean(to), length(x)))
  scales::rescale(x, to = to)
}

get_col <- function(df, pattern, default = NA){
  col <- names(df)[grepl(pattern, names(df), ignore.case = TRUE)]
  if (length(col) > 0) df[[col[1]]] else rep(default, nrow(df))
}

known_inst_colors <- c(
  "MSU"="#18453B","UF"="#FA4616","OSU"="#BB0000",
  "Chico"="#D32026","WCU"="#592A8A","UNL"="#D00000","UA"="#990000"
)

assign_institution_colors <- function(inst_vector){
  insts <- sort(unique(inst_vector))
  insts <- insts[!is.na(insts) & insts != ""]
  if (length(insts) == 0) return(character(0))
  cols <- rep(NA_character_, length(insts)); names(cols) <- insts
  for (i in seq_along(insts)){
    inst <- insts[i]; hit <- NA_character_
    if (!is.na(known_inst_colors[inst])) hit <- known_inst_colors[inst]
    if (is.na(hit) && !is.na(known_inst_colors[toupper(inst)])) hit <- known_inst_colors[toupper(inst)]
    if (is.na(hit) && !is.na(known_inst_colors[stringr::str_to_title(inst)])) hit <- known_inst_colors[stringr::str_to_title(inst)]
    cols[i] <- hit
  }
  unk <- which(is.na(cols))
  if (length(unk) > 0) cols[unk] <- hue_pal()(length(unk))
  cols
}

assign_gender_attributes <- function(gender_vector){
  pal <- c("Female"="orange","Male"="lightblue","Nonbinary"="#9370DB",
           "Other"="cornsilk","Unknown"="#C0C0C0")
  g <- gender_vector
  g[g %in% c("", NA)] <- "Unknown"
  g[!g %in% names(pal)] <- "Unknown"
  tibble(gender = g, color = pal[g], shape = "dot")
}

abbr_label <- function(x){
  x <- gsub("University of ", "U. ", x, fixed=TRUE)
  x <- gsub("University ", "U. ", x, fixed=TRUE)
  x <- gsub("California","Calif.",x,fixed=TRUE)
  x <- gsub("Pennsylvania","Penn.",x,fixed=TRUE)
  x <- gsub("Washington","Wash.",x,fixed=TRUE)
  x <- gsub("Massachusetts","Mass.",x,fixed=TRUE)
  x
}

# Remove single-letter middle initials in names (e.g., "Jane A. Doe" → "Jane Doe")
remove_middle_initials <- function(name) {
  name <- gsub("\\b[A-Z]\\.?\\s+", " ", name)  # remove single-letter initials
  name <- gsub("\\s{2,}", " ", name)
  stringr::str_squish(name)
}

# Fixed palette for Institution Type (in provided order)
inst_type_palette <- c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600", "#dddddd",
                       "#34ebba", "#eb34eb", "#3437eb")

make_insttype_palette <- function(levels_vec){
  lv <- as.character(levels_vec)
  pal <- rep(inst_type_palette, length.out = length(lv))
  names(pal) <- lv
  pal
}

# ------------------------------------------------------------------------------
# Load Lineage Data (people-level)
# ------------------------------------------------------------------------------
file_lineage <- "academic_lineage_master.xlsx"; stopifnot(file.exists(file_lineage))

df_raw <- read_excel(file_lineage, sheet = 1) %>%
  clean_names() %>%
  mutate(across(where(is.character),
                ~ trimws(iconv(., from = "", to = "UTF-8", sub = ""))))
names(df_raw) <- tolower(names(df_raw))

df <- df_raw %>%
  mutate(
    researcher_first    = get_col(df_raw, "^researcher[_ ]?first"),
    researcher_last     = get_col(df_raw, "^researcher[_ ]?last"),
    advisor_first       = get_col(df_raw, "^advisor[_ ]?first"),
    advisor_last        = get_col(df_raw, "^advisor[_ ]?last"),
    institution_current = get_col(df_raw, "institution[_ ]?current|university"),
    institution_phd     = get_col(df_raw, "institution[_ ]?phd|phd[_ ]?institution"),
    institution_type    = get_col(df_raw, "institution[_ ]?type"),              # <— NEW
    phd_year_raw        = get_col(df_raw, "phd"),
    prestige_raw        = get_col(df_raw, "prestige"),
    inbred_raw          = get_col(df_raw, "inbred"),
    h_index_raw         = suppressWarnings(as.numeric(get_col(df_raw, "h[_ ]?index|hindex"))),
    department          = get_col(df_raw, "department"),
    field               = get_col(df_raw, "field"),
    region              = get_col(df_raw, "region"),
    gender              = get_col(df_raw, "gender"),
    urm_status          = get_col(df_raw, "urm")
  ) %>%
  mutate(
    researcher = remove_middle_initials(str_to_lower(str_squish(paste(researcher_first, researcher_last)))),
    advisor    = remove_middle_initials(str_to_lower(str_squish(paste(advisor_first, advisor_last)))),
    prestige   = case_when(
      is.logical(prestige_raw) ~ as.integer(prestige_raw),
      str_to_upper(as.character(prestige_raw)) %in% c("Y","YES","1","TRUE") ~ 1L,
      str_to_upper(as.character(prestige_raw)) %in% c("N","NO","0","FALSE") ~ 0L,
      TRUE ~ NA_integer_
    ),
    inbred = case_when(
      is.logical(inbred_raw) ~ as.integer(inbred_raw),
      str_to_upper(as.character(inbred_raw)) %in% c("Y","YES","1","TRUE") ~ 1L,
      str_to_upper(as.character(inbred_raw)) %in% c("N","NO","0","FALSE") ~ 0L,
      TRUE ~ NA_integer_
    ),
    h_index     = h_index_raw,
    phd_year    = suppressWarnings(as.numeric(phd_year_raw)),
    career_years= ifelse(is.finite(phd_year), CURRENT_YEAR - phd_year, NA_real_),
    gender      = ifelse(is.na(gender), NA_character_, str_to_title(gender)),
    urm_status  = suppressWarnings(as.logical(urm_status))
  ) %>%
  mutate(inbred_auto = as.integer(tolower(institution_current) == tolower(institution_phd))) %>%
  mutate(inbred = ifelse(is.na(inbred) & !is.na(inbred_auto), inbred_auto, inbred)) %>%
  select(-inbred_auto)

edges <- df %>%
  filter(!is.na(advisor), advisor != "", !is.na(researcher), researcher != "") %>%
  select(from = advisor, to = researcher)

nodes <- df %>%
  distinct(researcher, .keep_all = TRUE) %>%
  transmute(
    id = researcher,
    label = str_to_title(researcher),
    institution_current,
    institution_phd,
    institution_type,                 # <— NEW in nodes
    phd_year,
    prestige = as.integer(prestige),
    inbred = as.integer(inbred),
    h_index = as.numeric(h_index),
    career_years,
    department, field, region, gender, urm_status
  )

# ensure all advisors exist as nodes
extra_advisors <- setdiff(edges$from, nodes$id)
if (length(extra_advisors) > 0) {
  nodes <- bind_rows(
    nodes,
    tibble(
      id = extra_advisors,
      label = str_to_title(extra_advisors),
      institution_current = NA_character_,
      institution_phd = NA_character_,
      institution_type = NA_character_,      # <— keep column aligned
      phd_year = NA_real_,
      prestige = NA_real_,
      inbred = NA_real_,
      h_index = NA_real_,
      career_years = NA_real_,
      department = NA_character_,
      field = NA_character_,
      region = NA_character_,
      gender = NA_character_,
      urm_status = NA
    )
  )
}

# keep edges that point to existing nodes (after adding missing advisors)
edges <- edges %>% filter(from %in% nodes$id & to %in% nodes$id)

inst_colors_people <- assign_institution_colors(nodes$institution_current)
edges_vis <- edges %>% mutate(arrows = "to", color = "gray70", width = 2)

# ------------------------------------------------------------------------------
# Load Kawa Data (institution-level) + Graph
# ------------------------------------------------------------------------------
file_kawa <- "aman_data.xlsx"; stopifnot(file.exists(file_kawa))

kawa_raw <- read_excel(file_kawa, sheet = 1) %>%
  clean_names() %>%
  mutate(across(where(is.character), ~ trimws(iconv(., from = "", to = "UTF-8", sub = ""))))

# columns
current_col <- "university"
phd_col <- "ph_d_institution"
subfield_col <- if ("subfield" %in% names(kawa_raw)) "subfield" else NA_character_

kawa <- kawa_raw %>%
  transmute(
    researcher = str_squish(paste(first_name, last_name)),
    university_current = .data[[current_col]],
    phd_institution    = .data[[phd_col]],
    subfield           = if (!is.na(subfield_col)) .data[[subfield_col]] else NA_character_
  ) %>%
  filter(!is.na(university_current) & university_current != "",
         !is.na(phd_institution) & phd_institution != "")

prestige_universities <- c(
  "Chicago", "Harvard", "Michigan", "UC-Berkeley", "Arizona", "Stanford", "Columbia",
  "UCLA", "Pennsylvania", "Yale", "UT-Austin", "New Mexico", "NYU", "Washington", "UC-Santa Barbara",
  "Florida")

k_edges <- kawa %>%
  transmute(from = phd_institution, to = university_current) %>%
  count(from, to, name = "weight") %>%
  mutate(inbred_edge = from == to)

k_nodes_all <- unique(c(k_edges$from, k_edges$to))
k_nodes <- tibble(inst = k_nodes_all) %>%
  mutate(
    inst_clean = str_to_lower(str_trim(inst)),
    # Normalize common variants (remove punctuation, handle UC/UT abbreviations, etc.)
    inst_norm = inst_clean %>%
      str_replace_all("-", " ") %>%
      str_replace_all("\\.", "") %>%
      str_replace_all("univ|university|college", "") %>%
      str_trim(),
    prestige = str_detect(
      inst_norm,
      paste(str_to_lower(prestige_universities), collapse = "|")
    )
  ) %>%
  mutate(prestige = as.logical(prestige)) %>%
  select(-inst_clean, -inst_norm)


k_in_deg  <- k_edges %>% group_by(to)   %>% summarise(in_deg  = sum(weight), .groups = "drop")
k_out_deg <- k_edges %>% group_by(from) %>% summarise(out_deg = sum(weight), .groups = "drop")

k_nodes <- k_nodes %>%
  left_join(k_in_deg,  by = c("inst" = "to")) %>%
  left_join(k_out_deg, by = c("inst" = "from")) %>%
  mutate(across(c(in_deg, out_deg), ~ replace_na(., 0)),
         total_deg = in_deg + out_deg,
         inbred_hires = ifelse(inst %in% k_edges$from[k_edges$from == k_edges$to],
                               dplyr::coalesce(k_edges$weight[match(inst, k_edges$from[k_edges$from == k_edges$to])], 0), 0),
         pct_inbred = ifelse(in_deg > 0, inbred_hires / in_deg, 0))

g_kawa <- graph_from_data_frame(k_edges %>% select(from, to, weight),
                                vertices = k_nodes %>% transmute(name = inst, prestige),
                                directed = TRUE)

# ------------------------------------------------------------------------------
# Version footer (visual only)
# ------------------------------------------------------------------------------
version_footer <- div(style="text-align:center; color:gray; margin-top:8px; font-size:12px;",
                      HTML("AAFS_V1.4 (2025)"))

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
ui <- navbarPage(
  title = "AAFS 2026 PhD Employment Network",
  id = "main_nav",
  header = tags$head(
    tags$style(HTML("
      .fixed-sidebar-layout > .row { display: flex; align-items: flex-start; }
      .fixed-sidebar-layout > .row > div:first-child { flex: 0 0 420px; max-width: 420px; }
      .fixed-sidebar-layout > .row > div:last-child { flex: 1 1 auto; max-width: none; }
      .sidebar-panel h4 { margin-top: 4px; }
      .alert-warning { background-color: #fff3cd; border: 1px solid #ffeeba; padding: 10px; border-radius: 6px; }
    "))
  ),
  
  # ---- TAB 1: Network ----
  tabPanel("Network",
           div(class = "fixed-sidebar-layout",
               sidebarLayout(
                 sidebarPanel(class = "sidebar-panel",
                              h4("Filters"),
                              radioButtons(
                                "color_mode", "Color Nodes By:",
                                choices = c("Prestige" = "prestige",
                                            "Inbred" = "inbred",
                                            #"Institution (Current)" = "institution_current",
                                            "Institution Type" = "institution_type",   # <— NEW choice
                                            #"Department" = "department",
                                            #"Field" = "field",
                                            "Gender" = "gender"),
                                selected = "institution_type"
                              ),
                              checkboxInput("enable_physics", "Enable physics", value = TRUE),
                              actionButton("refresh_network", "🔄 Refresh Graphic", class = "btn-primary")
                 ),
                 mainPanel(
                   withSpinner(visNetworkOutput("network", height = "800px")),
                   br(), uiOutput("legend_block"),
                   hr(), version_footer
                 )
               )
           )
  ),
  
  # ---- TAB 2: Analytics ----
  tabPanel("Analytics",
           fluidRow(
             column(6,
                    h4("H-index by Department (bar)"),
                    withSpinner(plotOutput("p_h_by_dept", height="340px")),
                    DTOutput("t_h_by_dept")),
             column(6,
                    h4("Gini of H-index by Department"),
                    withSpinner(plotOutput("p_gini_by_dept", height="340px")),
                    DTOutput("t_gini_by_dept"))
           ),
           hr(),
           fluidRow(
             column(6,
                    h4("H-index by Prestige (bar)"),
                    withSpinner(plotOutput("p_h_by_prestige", height="280px")),
                    DTOutput("t_h_by_prestige")),
             column(6,
                    h4("H-index by Inbred (bar)"),
                    withSpinner(plotOutput("p_h_by_inbred", height="280px")),
                    DTOutput("t_h_by_inbred"))
           ),
           hr(),
           fluidRow(
             column(6,
                    h4("H-index by Institution Type (bar)"),                # <— NEW
                    withSpinner(plotOutput("p_h_by_insttype", height="320px")),
                    DTOutput("t_h_by_insttype")),
             column(6,
                    h4("Prestige × Inbred Crosstab"),
                    DTOutput("t_prestige_inbred"))
           ),
           hr(),
           h4("Cross-Dataset Self-Hire Comparison"),
           p(tags$em("Compares self-hire rates by Field (lineage) and Subfield (Kawa).")),
           fluidRow(
             column(6, DTOutput("selfhire_table")),
             column(6, plotOutput("selfhire_bar", height="420px"))
           ),
           hr(), version_footer
  ),
  
  # ---- TAB 3: Kawa Data ----
  tabPanel("Kawa Data",
           div(class = "fixed-sidebar-layout",
               sidebarLayout(
                 sidebarPanel(class = "sidebar-panel",
                              h4("Kawa Explorer"),
                              radioButtons("k_view", "View Mode:",
                                           choices = c("Network", "Chord"), selected = "Network"),
                              
                              conditionalPanel(
                                "input.k_view == 'Network'",
                                selectInput("focus_inst", "Select Institution:",
                                            choices = sort(k_nodes$inst),
                                            selected = sort(k_nodes$inst)[1]),
                                sliderInput("ego_depth", "Network Depth (hops):",
                                            min = 1, max = 3, value = 1, step = 1),
                                radioButtons("k_color", "Color Nodes By:",
                                             choices = c("Prestigious Universities (gold)" = "prestigious",
                                                         "Institution Palette" = "institution"),
                                             selected = "prestigious"),
                                checkboxInput("k_show_inbred_edges_only", "Show self-hires only", FALSE),
                                sliderInput("k_min_weight", "Minimum edge weight (placements):",
                                            min = 1,
                                            max = max(1, max(k_edges$weight, na.rm = TRUE)),
                                            value = 1, step = 1),
                                checkboxInput("k_enable_physics", "Enable Physics", TRUE),
                                actionButton("k_refresh", "🔄 Refresh Ego Network", class = "btn-primary"),
                                hr(),
                                h4("Downloads"),
                                downloadButton("dl_ego_nodes",  "Ego Nodes (CSV)"),
                                downloadButton("dl_ego_edges",  "Ego Edges (CSV)"),
                                br(), br(),
                                downloadButton("dl_full_nodes", "Full Nodes (CSV)"),
                                downloadButton("dl_full_edges", "Full Edges (CSV)")
                              ),
                              
                              conditionalPanel(
                                "input.k_view == 'Chord'",
                                sliderInput("k_chord_top_n", "Chord: include top N institutions (by total degree)",
                                            min = 10, max = min(60, nrow(k_nodes)),
                                            value = min(30, nrow(k_nodes)), step = 1),
                                # (min-edge-weight control removed earlier)
                                checkboxInput("k_chord_show_self", "Show self-hire loops", TRUE),
                                checkboxInput("k_chord_show_labels", "Show abbreviated labels", TRUE),
                                actionButton("k_chord_refresh", "🔄 Refresh Chord", class = "btn-primary")
                              )
                 ),
                 mainPanel(
                   conditionalPanel(
                     "input.k_view == 'Network'",
                     div(class = "alert alert-warning",
                         HTML("<b>Note:</b> Ego views are fast, but 2–3 hops can take a few seconds.")),
                     withSpinner(visNetworkOutput("kawa_network", height = "820px")),
                     br(), uiOutput("kawa_legend")
                   ),
                   conditionalPanel(
                     "input.k_view == 'Chord'",
                     div(class = "alert alert-warning",
                         HTML("<b>Note:</b> Chord graph shows flows among top-N programs (Prestigious = gold).")),
                     withSpinner(plotOutput("kawa_chord", height = "820px")),
                     br(), uiOutput("kawa_chord_caption")
                   ),
                   hr(), version_footer
                 )
               )
           )
  )
  
  # ---- TAB 4: Career Lineage Tree (MASKED) ----
  # tabPanel("Career Lineage Tree", div("Temporarily disabled."))
)

# ------------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # ---------------- NETWORK TAB ----------------
  inst_color_map_people <- reactive({
    all_insts <- nodes$institution_current
    cmap <- assign_institution_colors(all_insts)
    list(fill = cmap, border = setNames(darken(unname(cmap), 0.3), names(cmap)))
  })
  
  filtered_nodes <- reactive({
    nv <- nodes
    nv$size <- ifelse(is.finite(nv$h_index),
                      rescale(nv$h_index, to = c(12, 42),
                              from = range(nv$h_index, na.rm = TRUE)), 14)
    gender_attrs <- assign_gender_attributes(nv$gender)
    nv$shape.gender <- gender_attrs$shape
    nv$color.gender <- gender_attrs$color
    
    cmaps <- inst_color_map_people()
    mode <- input$color_mode
    
    if (mode == "prestige") {
      nv$color.background <- ifelse(is.na(nv$prestige), "#CCCCCC",
                                    ifelse(nv$prestige == 1, "#FFC300", "#D9E3F0"))
      nv$color.border <- darken(nv$color.background, 0.35)
      nv$shape <- "dot"
      
    } else if (mode == "inbred") {
      nv$color.background <- dplyr::case_when(
        is.na(nv$inbred) ~ "#CCCCCC",
        nv$inbred == 1 ~ "#B2182B",
        nv$inbred == 0 ~ "#2166AC"
      )
      nv$color.border <- darken(nv$color.background, 0.35)
      nv$shape <- "dot"
      
    } else if (mode == "institution_current") {
      fills  <- cmaps$fill[as.character(nv$institution_current)]
      borders <- cmaps$border[as.character(nv$institution_current)]
      fills[is.na(fills)] <- "#DDDDDD"; borders[is.na(borders)] <- "#999999"
      nv$color.background <- fills; nv$color.border <- borders; nv$shape <- "dot"
      
    } else if (mode == "institution_type") {   # <— NEW coloring
      vals <- nv$institution_type
      labs <- sort(unique(vals[!is.na(vals) & vals != ""]))
      pal_named <- make_insttype_palette(labs)
      col_bg <- pal_named[as.character(vals)]
      col_bg[is.na(col_bg)] <- "#DDDDDD"
      nv$color.background <- col_bg
      nv$color.border <- darken(nv$color.background, 0.35)
      nv$shape <- "dot"
      
    } else if (mode == "department") {
      vals <- nv$department; labs <- sort(unique(vals[!is.na(vals) & vals != ""]))
      pal <- setNames(hue_pal()(max(1, length(labs))), labs)
      nv$color.background <- pal[as.character(vals)]
      nv$color.background[is.na(nv$color.background)] <- "#DDDDDD"
      nv$color.border <- darken(nv$color.background, 0.35)
      nv$shape <- "dot"
      
    } else if (mode == "field") {
      vals <- nv$field; labs <- sort(unique(vals[!is.na(vals) & vals != ""]))
      pal <- setNames(hue_pal()(max(1, length(labs))), labs)
      nv$color.background <- pal[as.character(vals)]
      nv$color.background[is.na(nv$color.background)] <- "#DDDDDD"
      nv$color.border <- darken(nv$color.background, 0.35)
      nv$shape <- "dot"
      
    } else if (mode == "gender") {
      nv$color.background <- nv$color.gender
      nv$color.border <- darken(nv$color.background, 0.35)
      nv$shape <- "dot"
    } else {
      nv$color.background <- "#DDDDDD"; nv$color.border <- "#999999"; nv$shape <- "dot"
    }
    nv
  })
  
  filtered_edges <- reactive({
    ids <- filtered_nodes()$id
    edges_vis %>% filter(from %in% ids & to %in% ids)
  })
  
  render_main_network <- function() {
    vis <- visNetwork(filtered_nodes(), filtered_edges()) %>%
      visNodes(font = list(size = 22, vadjust = -20, align = "center")) %>%
      visEdges(smooth = TRUE, color = list(highlight = "red")) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1), nodesIdSelection = TRUE) %>%
      visPhysics(
        enabled = input$enable_physics,
        solver = "forceAtlas2Based",
        stabilization = FALSE,
        barnesHut = list(
          gravitationalConstant = -3000, centralGravity = 0.3,
          springLength = 150, springConstant = 0.04),
        minVelocity = 0.75
      ) %>% visLayout(randomSeed = 123)
    output$network <- renderVisNetwork({ vis })
  }
  
  # Auto-load on start
  observe({ render_main_network() })
  # Manual refresh remains
  observeEvent(input$refresh_network, { render_main_network() }, ignoreInit = TRUE)
  
  output$legend_block <- renderUI({
    fn <- tryCatch(filtered_nodes(), error=function(e) NULL)
    if (is.null(fn) || nrow(fn)==0) return(NULL)
    
    if (input$color_mode == "institution_current") {
      insts <- sort(unique(fn$institution_current))
      insts <- insts[!is.na(insts) & insts != ""]
      if (length(insts) > 25) {
        top_counts <- fn %>% filter(!is.na(institution_current), institution_current!="") %>%
          count(institution_current, sort = TRUE) %>% slice_head(n = 25)
        insts <- top_counts$institution_current
        extra <- " + more"
      } else extra <- ""
      fills <- assign_institution_colors(insts)[insts]; fills[is.na(fills)] <- "#DDDDDD"
      dots <- vapply(seq_along(insts), function(i) {
        sprintf("<span style='display:inline-block; width:14px; height:14px; background-color:%s; 
                 border-radius:50%%; margin-right:6px; border:1px solid %s;'></span>%s",
                fills[i], darken(fills[i],0.35), htmltools::htmlEscape(insts[i])) }, character(1))
      div(style="text-align:center;",
          HTML("<h5><b>Institution Legend (top 25)</b></h5>"),
          HTML(paste(dots, collapse="&nbsp;&nbsp;")), HTML(extra))
      
    } else if (input$color_mode == "institution_type") {   # <— NEW legend
      vals <- fn$institution_type
      labs <- sort(unique(vals[!is.na(vals) & vals != ""]))
      pal_named <- make_insttype_palette(labs)
      dots <- vapply(seq_along(labs), function(i) {
        col_bg <- pal_named[[ labs[i] ]]
        sprintf("<span style='display:inline-block; width:14px; height:14px; background-color:%s; 
                 border-radius:50%%; margin-right:6px; border:1px solid %s;'></span>%s",
                col_bg, darken(col_bg,0.35), htmltools::htmlEscape(labs[i])) }, character(1))
      div(style="text-align:center;", HTML("<h5><b>Institution Type Legend</b></h5>"),
          HTML(paste(dots, collapse="&nbsp;&nbsp;")))
      
    } else if (input$color_mode == "prestige") {
      div(style="text-align:center;", HTML("<h5><b>Prestige Legend</b></h5>"),
          HTML(paste(
            "<span style='display:inline-block; width:14px; height:14px; background-color:#FFC300; border-radius:50%; margin-right:6px;'></span>Prestige",
            "&nbsp;&nbsp;&nbsp;&nbsp;",
            "<span style='display:inline-block; width:14px; height:14px; background-color:#D9E3F0; border-radius:50%; margin-right:6px;'></span>Non-Prestige",
            "&nbsp;&nbsp;&nbsp;&nbsp;",
            "<span style='display:inline-block; width:14px; height:14px; background-color:#CCCCCC; border-radius:50%; margin-right:6px;'></span>Unknown")))
      
    } else if (input$color_mode == "inbred") {
      div(style="text-align:center;", HTML("<h5><b>Inbreeding Legend</b></h5>"),
          HTML(paste(
            "<span style='display:inline-block; width:14px; height:14px; background-color:#B2182B; border-radius:50%; margin-right:6px;'></span>Inbred",
            "&nbsp;&nbsp;&nbsp;&nbsp;",
            "<span style='display:inline-block; width:14px; height:14px; background-color:#2166AC; border-radius:50%; margin-right:6px;'></span>Not Inbred",
            "&nbsp;&nbsp;&nbsp;&nbsp;",
            "<span style='display:inline-block; width:14px; height:14px; background-color:#CCCCCC; border-radius:50%; margin-right:6px;'></span>Unknown")))
      
    } else if (input$color_mode == "gender") {
      vals <- sort(unique(fn$gender)); vals <- vals[!is.na(vals) & vals != ""]
      gender_palette <- c("Female"="orange","Male"="lightblue","Nonbinary"="#9370DB","Other"="cornsilk","Unknown"="#C0C0C0")
      cmap <- gender_palette[match(vals, names(gender_palette))]; cmap[is.na(cmap)] <- "#C0C0C0"
      dots <- vapply(seq_along(vals), function(i) {
        sprintf("<span style='display:inline-block; width:14px; height:14px; background-color:%s; 
               border-radius:50%%; margin-right:6px; border:1px solid %s;'></span>%s",
                cmap[i], darken(cmap[i],0.35), htmltools::htmlEscape(vals[i])) }, character(1))
      div(style="text-align:center;", HTML("<h5><b>Gender Legend</b></h5>"), HTML(paste(dots, collapse="&nbsp;&nbsp;")))
      
    } else if (input$color_mode %in% c("department","field")) {
      var <- input$color_mode
      labs <- fn[[var]] %>% unique() %>% {.[!is.na(.) & . != ""]} %>% sort()
      if (length(labs) > 25) labs <- labs[1:25]
      pal <- setNames(hue_pal()(max(1, length(labs))), labs)
      dots <- vapply(seq_along(labs), function(i) {
        sprintf("<span style='display:inline-block; width:14px; height:14px; background-color:%s; 
                 border-radius:50%%; margin-right:6px; border:1px solid %s;'></span>%s",
                pal[i], darken(pal[i],0.35), htmltools::htmlEscape(labs[i])) }, character(1))
      div(style="text-align:center;", HTML(sprintf("<h5><b>%s Legend (top 25)</b></h5>", str_to_title(var))),
          HTML(paste(dots, collapse="&nbsp;&nbsp;")))
    } else NULL
  })
  
  # ---------------- ANALYTICS ----------------
  nodes_for_stats <- reactive({
    nodes %>% transmute(
      h_index = as.numeric(h_index),
      prestige = factor(ifelse(prestige == 1, "Prestige", 
                               ifelse(prestige == 0, "Non-Prestige", NA)), 
                        levels = c("Prestige","Non-Prestige")),
      inbred = factor(ifelse(inbred == 1, "Inbred",
                             ifelse(inbred == 0, "Not Inbred", NA)),
                      levels = c("Inbred","Not Inbred")),
      department = as.character(department),
      field = as.character(field),
      institution_type = as.character(institution_type)   # <— NEW in analytics
    ) %>% drop_na(h_index)
  })
  
  # H-index by Department (bar + table)
  output$p_h_by_dept <- renderPlot({
    df <- nodes_for_stats()
    agg <- df %>% group_by(department) %>%
      summarise(n = n(), mean_h = mean(h_index, na.rm=TRUE), .groups = "drop") %>%
      arrange(desc(mean_h)) %>% slice_head(n = 25)
    ggplot(agg, aes(reorder(department, mean_h), mean_h)) +
      geom_col(fill = "gray60") +
      coord_flip() +
      labs(x = NULL, y = "Mean H-index", title = NULL)
  })
  output$t_h_by_dept <- renderDT({
    df <- nodes_for_stats()
    df %>% group_by(department) %>%
      summarise(n = n(), mean_h = round(mean(h_index, na.rm=TRUE),2),
                median_h = round(median(h_index, na.rm=TRUE),2)) %>%
      arrange(desc(mean_h)) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  # Gini by Department (bar + table)
  output$p_gini_by_dept <- renderPlot({
    df <- nodes_for_stats()
    agg <- df %>% group_by(department) %>%
      summarise(n = n(),
                gini = gini_index(h_index), .groups="drop") %>%
      filter(n >= 3) %>% arrange(desc(gini)) %>% slice_head(n = 25)
    ggplot(agg, aes(reorder(department, gini), gini)) +
      geom_col(fill = "gray60") +
      coord_flip() +
      labs(x = NULL, y = "Gini (H-index)", title = NULL)
  })
  output$t_gini_by_dept <- renderDT({
    df <- nodes_for_stats()
    df %>% group_by(department) %>%
      summarise(n = n(),
                gini = round(gini_index(h_index),3), .groups="drop") %>%
      arrange(desc(gini)) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  # H-index by Prestige
  output$p_h_by_prestige <- renderPlot({
    df <- nodes_for_stats() %>% filter(!is.na(prestige))
    agg <- df %>% group_by(prestige) %>%
      summarise(n = n(), mean_h = mean(h_index), .groups="drop")
    ggplot(agg, aes(prestige, mean_h)) +
      geom_col(fill = "gray60") +
      labs(x = NULL, y = "Mean H-index", title = NULL)
  })
  output$t_h_by_prestige <- renderDT({
    df <- nodes_for_stats() %>% filter(!is.na(prestige))
    df %>% group_by(prestige) %>%
      summarise(n = n(),
                mean_h = round(mean(h_index),2),
                median_h = round(median(h_index),2)) %>%
      datatable(options = list(pageLength = 5), rownames = FALSE)
  })
  
  # H-index by Inbred
  output$p_h_by_inbred <- renderPlot({
    df <- nodes_for_stats() %>% filter(!is.na(inbred))
    agg <- df %>% group_by(inbred) %>%
      summarise(n = n(), mean_h = mean(h_index), .groups="drop")
    ggplot(agg, aes(inbred, mean_h)) +
      geom_col(fill = "gray60") +
      labs(x = NULL, y = "Mean H-index", title = NULL)
  })
  output$t_h_by_inbred <- renderDT({
    df <- nodes_for_stats() %>% filter(!is.na(inbred))
    df %>% group_by(inbred) %>%
      summarise(n = n(),
                mean_h = round(mean(h_index),2),
                median_h = round(median(h_index),2)) %>%
      datatable(options = list(pageLength = 5), rownames = FALSE)
  })
  
  # NEW: H-index by Institution Type
  output$p_h_by_insttype <- renderPlot({
    df <- nodes_for_stats() %>% filter(!is.na(institution_type) & institution_type != "")
    if(nrow(df)==0){ plot.new(); text(.5,.5,"No Institution Type data"); return(invisible()) }
    agg <- df %>% group_by(institution_type) %>%
      summarise(n = n(), mean_h = mean(h_index, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(mean_h))
    ggplot(agg, aes(reorder(institution_type, mean_h), mean_h)) +
      geom_col(fill = "gray60") +
      coord_flip() +
      labs(x = NULL, y = "Mean H-index", title = NULL)
  })
  output$t_h_by_insttype <- renderDT({
    df <- nodes_for_stats() %>% filter(!is.na(institution_type) & institution_type != "")
    if(nrow(df)==0){
      return(datatable(data.frame(Message="No Institution Type data"), options=list(dom='t'), rownames=FALSE))
    }
    df %>% group_by(institution_type) %>%
      summarise(n = n(),
                mean_h = round(mean(h_index, na.rm=TRUE),2),
                median_h = round(median(h_index, na.rm=TRUE),2)) %>%
      arrange(desc(mean_h)) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  # Prestige × Inbred Crosstab
  output$t_prestige_inbred <- renderDT({
    df <- nodes_for_stats() %>% filter(!is.na(prestige), !is.na(inbred))
    tab <- df %>% count(prestige, inbred) %>%
      tidyr::pivot_wider(names_from = inbred, values_from = n, values_fill = 0) %>%
      arrange(match(prestige, c("Prestige","Non-Prestige")))
    datatable(tab, options = list(pageLength = 5), rownames = FALSE)
  })
  
  # -------- Cross-Dataset Self-Hire Comparison (Field vs Subfield) ------------
  lineage_selfhire <- reactive({
    nodes %>%
      filter(!is.na(field) & field != "") %>%
      group_by(field) %>%
      summarise(n=n(), n_inbred=sum(inbred==1,na.rm=TRUE),
                rate=ifelse(n>0,n_inbred/n,NA_real_), .groups="drop") %>%
      transmute(Discipline=field, SelfHire_Network=rate)
  })
  kawa_selfhire <- reactive({
    kawa_raw %>%
      mutate(self = tolower(!!sym(phd_col)) == tolower(!!sym(current_col)),
             subfield = ifelse(is.na(!!sym(subfield_col)) | !subfield_col %in% names(kawa_raw),
                               "Unspecified", !!sym(subfield_col))) %>%
      group_by(subfield) %>%
      summarise(n=n(), n_self=sum(self,na.rm=TRUE),
                rate=ifelse(n>0,n_self/n,NA_real_), .groups="drop") %>%
      transmute(Discipline=subfield, SelfHire_Kawa=rate)
  })
  selfhire_merge <- reactive({
    full_join(lineage_selfhire(), kawa_selfhire(), by="Discipline") %>%
      mutate(
        `Self-Hire (Network)` = percent(SelfHire_Network,accuracy=0.1),
        `Self-Hire (Kawa)`    = percent(SelfHire_Kawa,accuracy=0.1),
        Difference = percent(SelfHire_Network - SelfHire_Kawa,accuracy=0.1)
      ) %>%
      select(Discipline, `Self-Hire (Network)`, `Self-Hire (Kawa)`, Difference)
  })
  output$selfhire_table <- renderDT({
    datatable(selfhire_merge(), rownames=FALSE,
              options=list(pageLength=8,dom='tip'),class="compact stripe hover")
  })
  output$selfhire_bar <- renderPlot({
    m <- full_join(lineage_selfhire(), kawa_selfhire(), by="Discipline")
    if(nrow(m)==0){plot.new();text(.5,.5,"No overlapping disciplines");return(invisible())}
    plot_df <- m %>%
      pivot_longer(cols=c(SelfHire_Network,SelfHire_Kawa),
                   names_to="Source",values_to="Rate") %>%
      mutate(Source=recode(Source,
                           SelfHire_Network="Network",
                           SelfHire_Kawa="Kawa"))
    ggplot(plot_df,aes(x=reorder(Discipline,Rate),y=Rate,fill=Source))+
      geom_col(position=position_dodge(.7),width=.6,color="gray25")+
      coord_flip()+
      scale_y_continuous(labels=percent,expand=expansion(mult=c(0,0.05)))+
      scale_fill_manual(values=c("Network"="gray60","Kawa"="gray85"))+
      labs(x=NULL,y="Self-hire rate")+theme_journal(base_size=12)+
      theme(legend.position="top",legend.title=element_blank())
  })
  
  # ---------------- KAWA: Ego Network (visNetwork) ----------------
  inst_color_map_kawa <- assign_institution_colors(k_nodes$inst)
  
  kawa_ego_subgraph <- reactive({
    req(input$focus_inst, input$ego_depth)
    if (!(input$focus_inst %in% V(g_kawa)$name)) return(induced_subgraph(g_kawa, character(0)))
    ego_idx <- which(V(g_kawa)$name == input$focus_inst)
    ego_nodes <- neighborhood(g_kawa, order = input$ego_depth, nodes = ego_idx, mode = "all")[[1]]
    sg <- induced_subgraph(g_kawa, ego_nodes)
    e_df <- as_data_frame(sg, what = "edges")
    if (!is.null(input$k_min_weight) && is.finite(input$k_min_weight)) e_df <- e_df %>% filter(weight >= input$k_min_weight)
    if (isTRUE(input$k_show_inbred_edges_only)) e_df <- e_df %>% filter(from == to)
    keep_nodes <- unique(c(e_df$from, e_df$to))
    induced_subgraph(g_kawa, keep_nodes)
  })
  
  ego_nodes_vis <- reactive({
    sg <- kawa_ego_subgraph()
    if (vcount(sg) == 0) return(tibble(id = character(), label = character()))
    nms <- V(sg)$name
    dat <- k_nodes %>% filter(inst %in% nms) %>%
      mutate(
        color.background = if (input$k_color == "prestigious") {
          ifelse(prestige, "#FFC300", "#D9E3F0")
        } else {
          tmp <- inst_color_map_kawa[inst]; tmp[is.na(tmp)] <- "#D9E3F0"; tmp
        },
        color.border = darken(color.background, 0.35),
        size = safe_rescale(pmax(total_deg, 1), to = c(12, 40))
      ) %>% transmute(
        id = inst, label = inst, prestige, in_deg, out_deg, total_deg, pct_inbred, size,
        color.background, color.border,
        title = paste0("<b>", htmlEscape(inst), "</b><br/>",
                       "Prestigious: ", ifelse(prestige,"Yes","No"), "<br/>",
                       "Incoming hires: ", in_deg, "<br/>Outgoing PhDs: ", out_deg, "<br/>",
                       "Pct Self-Hire of hires: ", scales::percent(pct_inbred, accuracy=0.1))
      )
    dat
  })
  
  ego_edges_vis <- reactive({
    sg <- kawa_ego_subgraph()
    if (ecount(sg) == 0) return(tibble(from = character(), to = character()))
    as_data_frame(sg, what="edges") %>%
      transmute(from, to, width = safe_rescale(weight, to=c(1,8)), arrows="to",
                color = ifelse(from==to, "#B2182B", "gray70"), dashes = from==to)
  })
  
  observeEvent(input$k_refresh, {
    n <- ego_nodes_vis(); e <- ego_edges_vis()
    output$kawa_network <- renderVisNetwork({
      if (nrow(n)==0 || nrow(e)==0) return(visNetwork(data.frame(id=character(), label=character()), data.frame(from=character(), to=character())))
      visNetwork(n, e) %>%
        visIgraphLayout(layout = "layout_in_circle") %>%
        visPhysics(enabled = FALSE)%>%
        visOptions(highlightNearest=TRUE, nodesIdSelection=TRUE) %>%
        visPhysics(enabled=isTRUE(input$k_enable_physics), solver="repulsion", stabilization=TRUE,
                   repulsion=list(nodeDistance=200, springLength=160, springConstant=0.03, damping=0.09),
                   minVelocity=0.8) %>% visLayout(randomSeed=123, improvedLayout=TRUE)
    })
  }, ignoreInit = FALSE)
  
  observeEvent(list(input$focus_inst,input$ego_depth,input$k_min_weight,input$k_show_inbred_edges_only,input$k_color,input$k_enable_physics),
               {
                 n <- ego_nodes_vis(); e <- ego_edges_vis()
                 output$kawa_network <- renderVisNetwork({
                   if (nrow(n)==0 || nrow(e)==0) return(visNetwork(data.frame(id=character(), label=character()), data.frame(from=character(), to=character())))
                   visNetwork(n, e) %>%
                     visIgraphLayout(layout = "layout_in_circle") %>%
                     visPhysics(enabled = FALSE)%>%
                     visOptions(highlightNearest=TRUE, nodesIdSelection=TRUE) %>%
                     visPhysics(enabled=isTRUE(input$k_enable_physics), solver="repulsion", stabilization=TRUE,
                                repulsion=list(nodeDistance=200, springLength=160, springConstant=0.03, damping=0.09),
                                minVelocity=0.8) %>% visLayout(randomSeed=123, improvedLayout=TRUE)
                 })
               }, ignoreInit = TRUE)
  
  output$kawa_legend <- renderUI({
    if (input$k_color == "prestigious") {
      div(style="text-align:center;",
          HTML("<b>Prestigious universities</b>: gold • <b>Non-prestigious</b>: gray • <b>Self-hires</b>: red dashed edges"))
    } else {
      div(style="text-align:center;",
          HTML("<b>Institution palette</b> (unique color per university) • <b>Self-hires</b>: red dashed edges"))
    }
  })
  
  # Downloads (Kawa)
  output$dl_ego_nodes <- downloadHandler(
    filename = function() paste0("kawa_ego_nodes_", gsub("\\W","_", input$focus_inst), "_", input$ego_depth, "hop.csv"),
    content = function(file) { write.csv(ego_nodes_vis(), file, row.names=FALSE) }
  )
  output$dl_ego_edges <- downloadHandler(
    filename = function() paste0("kawa_ego_edges_", gsub("\\W","_", input$focus_inst), "_", input$ego_depth, "hop.csv"),
    content = function(file) { write.csv(ego_edges_vis(), file, row.names=FALSE) }
  )
  output$dl_full_nodes <- downloadHandler(
    filename = function() "kawa_full_nodes.csv",
    content = function(file) { write.csv(k_nodes, file, row.names=FALSE) }
  )
  output$dl_full_edges <- downloadHandler(
    filename = function() "kawa_full_edges.csv",
    content = function(file) { write.csv(k_edges, file, row.names=FALSE) }
  )
  
  # ---------------- KAWA: Chord (circlize) ----------------
  chord_data <- reactive({
    req(input$k_chord_top_n)
    top_tbl <- k_nodes %>% arrange(desc(total_deg)) %>% slice_head(n = input$k_chord_top_n)
    keep <- top_tbl$inst
    ed <- k_edges %>% filter(from %in% keep & to %in% keep)
    if (!isTRUE(input$k_chord_show_self)) ed <- ed %>% filter(from != to)
    list(edges = ed, keep = keep, nodes = k_nodes %>% filter(inst %in% keep))
  })
  
  output$kawa_chord <- renderPlot({
    req(input$k_view == "Chord")
    cd <- chord_data(); ed <- cd$edges; keep <- cd$keep; nd <- cd$nodes
    if (nrow(ed) == 0) { plot.new(); text(0.5,0.5,"No edges after filters"); return(invisible()) }
    
    mat <- matrix(0, nrow = length(keep), ncol = length(keep), dimnames = list(keep, keep))
    idx_from <- match(ed$from, keep); idx_to <- match(ed$to, keep)
    for (i in seq_len(nrow(ed))) mat[idx_from[i], idx_to[i]] <- mat[idx_from[i], idx_to[i]] + ed$weight[i]
    
    prestige_gold <- "#FFC300"; non_prestige_gray <- "#D9E3F0"
    grid.col <- ifelse(nd$prestige[match(keep, nd$inst)], prestige_gold, non_prestige_gray)
    names(grid.col) <- keep
    
    rim_labels <- if (isTRUE(input$k_chord_show_labels)) abbr_label(keep) else keep
    
    circos.clear()
    circos.par(gap.after = rep(3, length(keep)), start.degree = 90, track.margin = c(0.01,0.01), cell.padding = c(0,0,0,0))
    chordDiagram(mat, grid.col = grid.col, transparency = 0.25, annotationTrack = "grid",
                 preAllocateTracks = list(list(track.height = 0.08)),
                 directional = 1, direction.type = c("arrows","diffHeight"),
                 link.arr.type = "big.arrow", link.border = NA,
                 link.sort = TRUE, link.decreasing = FALSE,
                 col = rgb(106,76,147, maxColorValue = 255, alpha = 160))
    
    circos.trackPlotRegion(track.index = 1, bg.border = NA, panel.fun = function(x, y) {
      sector.name <- get.cell.meta.data("sector.index")
      i <- which(keep == sector.name)[1]
      nm <- rim_labels[i]
      circos.text(CELL_META$xcenter, CELL_META$ycenter, nm, facing = "clockwise", niceFacing = TRUE, cex = 0.6, adj = c(0, 0.5))
    })
    
    if (isTRUE(input$k_chord_show_self)) {
      diag_w <- diag(mat); has_loop <- which(diag_w > 0)
      if (length(has_loop) > 0) for (j in has_loop) highlight.sector(keep[j], track.index = 1, col = NA, border = "#B2182B", lwd = 2)
    }
    
    legend("bottom", horiz = TRUE, bty = "n",
           legend = c("Prestigious (gold)", "Non-prestigious (gray)", "Self-hire border"),
           pch = 16, pt.cex = 1.5, col = c(prestige_gold, non_prestige_gray, "#B2182B"),
           inset = 0.02, xpd = NA)
  })
  
  output$kawa_chord_caption <- renderUI({
    cd <- chord_data()
    div(style = "text-align:center;",
        HTML(paste0("<i>Top ", length(cd$keep), " institutions by total degree.</i>")))
  })
  
}

# ------------------------------------------------------------------------------
# Run App
# ------------------------------------------------------------------------------
shinyApp(ui, server)
