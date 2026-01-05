################################################################################
# TITLE:  AAFS 2026 - Passalacqua Network (Stable with missing-year handling)
# PURPOSE: Visualize academic lineage + prestige + inbreeding + H-index
# AUTHOR: Joe Hefner, Nick Passalacqua, Natalie Clark [with Quill]
# LAST UPDATED: 2025-11-03
################################################################################
rm(list = ls())





# ================================
# Libraries
# ================================
suppressPackageStartupMessages({
  library(shiny)
  library(readxl)
  library(tidyverse)
  library(igraph)
  library(visNetwork)
  library(DT)
  library(scales)
  library(quantreg)
  library(ggplot2)
  library(gridExtra)
})

set.seed(42)
CURRENT_YEAR <- 2025

# ================================
# Helper: column matching
# ================================
get_col <- function(df, pattern, default = NA) {
  col <- names(df)[grepl(pattern, names(df), ignore.case = TRUE)]
  if (length(col) > 0) df[[col[1]]] else rep(default, nrow(df))
}

# --- helpers for chord view ---
normalize_name <- function(x) tolower(trimws(gsub("\\s+", " ", x)))
abbr_label <- function(x) {            # short labels around the circle
  x <- gsub("University of ", "U. ", x, fixed = TRUE)
  x <- gsub("University ", "U. ", x, fixed = TRUE)
  x <- gsub("California", "Calif.", x, fixed = TRUE)
  x <- gsub("Pennsylvania", "Penn.", x, fixed = TRUE)
  x <- gsub("Washington", "Wash.", x, fixed = TRUE)
  x <- gsub("Massachusetts", "Mass.", x, fixed = TRUE)
  x
}

# ================================
# 1. Load & Prepare Data
# ================================
file_path <- "academic_lineage_master.xlsx"

df <- read_excel(file_path, sheet = 1) %>%
  janitor::clean_names() %>%
  mutate(across(where(is.character), ~ trimws(iconv(., from = "", to = "UTF-8", sub = ""))))

# Lowercase all column names
names(df) <- tolower(names(df))

# Extract columns flexibly
df <- df %>%
  mutate(
    researcher_first = get_col(df, "^researcher_first"),
    researcher_last  = get_col(df, "^researcher_last"),
    advisor_first    = get_col(df, "^advisor_first"),
    advisor_last     = get_col(df, "^advisor_last"),
    institution_current = get_col(df, "institution_current"),
    institution_phd     = get_col(df, "institution_phd"),
    phd_year_raw        = get_col(df, "phd"),
    prestige_raw        = get_col(df, "prestige"),
    inbred_raw          = get_col(df, "inbred"),
    h_index_raw         = suppressWarnings(as.numeric(get_col(df, "h_index"))),
    department = get_col(df, "department"),
    field = get_col(df, "field"),
    region = get_col(df, "region"),
    gender = get_col(df, "gender"),
    urm_status = get_col(df, "urm")
  )

# Combine and standardize
df <- df %>%
  mutate(
    researcher = str_squish(paste(researcher_first, researcher_last)),
    advisor = str_squish(paste(advisor_first, advisor_last)),
    prestige = ifelse(str_to_upper(prestige_raw) %in% c("Y","YES","1"), 1, 0),
    inbred = ifelse(str_to_upper(inbred_raw) %in% c("Y","YES","1"), 1, 0),
    h_index = h_index_raw,
    phd_year = suppressWarnings(as.numeric(phd_year_raw)),
    career_years = ifelse(is.finite(phd_year), CURRENT_YEAR - phd_year, NA_real_),
    gender = str_to_title(gender),
    urm_status = as.logical(urm_status)
  )

# Derive inbred automatically if same institution
df <- df %>%
  mutate(inbred_auto = as.numeric(tolower(institution_current) == tolower(institution_phd))) %>%
  mutate(inbred = ifelse(is.na(inbred) & !is.na(inbred_auto), inbred_auto, inbred)) %>%
  select(-inbred_auto)

# ================================
# 2. Build Nodes & Edges
# ================================
edges <- df %>%
  filter(!is.na(advisor) & advisor != "" & !is.na(researcher) & researcher != "") %>%
  select(from = advisor, to = researcher)

nodes <- df %>%
  distinct(researcher, .keep_all = TRUE) %>%
  transmute(
    id = researcher,
    label = researcher,
    institution_current,
    institution_phd,
    phd_year,
    prestige,
    inbred,
    h_index,
    career_years,
    department, field, region, gender, urm_status
  )

# Add advisors not in researcher list
extra_advisors <- setdiff(edges$from, nodes$id)
if (length(extra_advisors) > 0) {
  nodes <- bind_rows(nodes,
                     tibble(id = extra_advisors,
                            label = extra_advisors,
                            prestige = NA,
                            inbred = NA,
                            h_index = NA,
                            career_years = NA))
}

# Handle missing year values safely
phd_min <- suppressWarnings(min(nodes$phd_year, na.rm = TRUE))
phd_max <- suppressWarnings(max(nodes$phd_year, na.rm = TRUE))
if (!is.finite(phd_min) || !is.finite(phd_max)) {
  phd_min <- 1990
  phd_max <- CURRENT_YEAR
}

# Stable layout
g <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)
xy <- layout_with_fr(g) * 500
nodes$x <- xy[,1]; nodes$y <- xy[,2]
edges_vis <- edges %>% mutate(arrows = "to", color = "gray70", width = 2)

# ================================
# 3. UI
# ================================
ui <- navbarPage(
  title = "AAFS 2026 Passalacqua Network",
  id = "main_nav",
  
  tabPanel("Network",
           sidebarLayout(
             sidebarPanel(
               h4("Filters"),
               radioButtons("color_mode", "Color Nodes By:",
                            choices = c("Prestige" = "prestige",
                                        "Inbred" = "inbred",
                                        "Institution" = "institution_current",
                                        "Field" = "field",
                                        "Gender" = "gender"),
                            selected = "prestige"),
               sliderInput("hindex_range", "H-index Range:",
                           min = floor(min(nodes$h_index, na.rm=TRUE)),
                           max = ceiling(max(nodes$h_index, na.rm=TRUE)),
                           value = c(floor(min(nodes$h_index, na.rm=TRUE)),
                                     ceiling(max(nodes$h_index, na.rm=TRUE))),
                           step = 1),
               sliderInput("phd_range", "PhD Year Range:",
                           min = phd_min, max = phd_max,
                           value = c(phd_min, phd_max), step = 1),
               checkboxInput("enable_physics", "Enable physics", value = TRUE),
               actionButton("refresh_network", "🔄 Refresh Graphic", class="btn-primary")
             ),
             mainPanel(
               visNetworkOutput("network", height="800px"),
               br(),
               uiOutput("legend_block")
             )
           )
  ),
  
  tabPanel("Analytics",
           fluidRow(
             column(6, h4("H-index Distribution"), plotOutput("h_hist", height="350px")),
             column(6, h4("H-index by Prestige / Inbred"), plotOutput("h_box", height="350px"))
           ),
           hr(),
           verbatimTextOutput("lm_out"),
           verbatimTextOutput("qr_out")
  )
)

# ================================
# 4. SERVER
# ================================
server <- function(input, output, session) {
  
  filtered_nodes <- reactive({
    nv <- nodes
    # Safely filter by H-index
    if (!all(is.na(nv$h_index))) {
      nv <- nv %>%
        filter(is.na(h_index) |
                 (h_index >= input$hindex_range[1] &
                    h_index <= input$hindex_range[2]))
    }
    # Safely filter by PhD year
    if (!all(is.na(nv$phd_year))) {
      nv <- nv %>%
        filter(is.na(phd_year) |
                 (phd_year >= input$phd_range[1] &
                    phd_year <= input$phd_range[2]))
    }
    # Node sizing
    nv$size <- ifelse(is.finite(nv$h_index),
                      rescale(nv$h_index,to=c(12,42),
                              from=range(nv$h_index,na.rm=TRUE)),14)
    # Node color
    if (input$color_mode == "prestige") {
      nv$color.background <- ifelse(is.na(nv$prestige),"#CCCCCC",
                                    ifelse(nv$prestige==1,"#18453B","#FFFFFF"))
    } else if (input$color_mode == "inbred") {
      nv$color.background <- case_when(
        is.na(nv$inbred) ~ "#CCCCCC",
        nv$inbred == 1 ~ "#B2182B",
        nv$inbred == 0 ~ "#2166AC"
      )
    } else {
      var <- sym(input$color_mode)
      values <- sort(unique(nv[[as.character(var)]]))
      values <- values[!is.na(values)]
      pal <- colorRampPalette(c("#e0ecf4","#9ebcda","#8856a7","#810f7c"))(length(values))
      cmap <- setNames(pal, values)
      nv$color.background <- cmap[as.character(nv[[as.character(var)]])]
      nv$color.background[is.na(nv$color.background)] <- "#DDDDDD"
    }
    nv
  })
  
  filtered_edges <- reactive({
    ids <- filtered_nodes()$id
    edges_vis %>% filter(from %in% ids & to %in% ids)
  })
  
  output$legend_block <- renderUI({
    if (input$color_mode == "prestige") {
      HTML("<b>Legend:</b> Green = Prestige, White = Non-Prestige, Gray = Unknown")
    } else if (input$color_mode == "inbred") {
      HTML("<b>Legend:</b> Red = Inbred Yes, Blue = No, Gray = Unknown")
    } else {
      HTML(paste0("<b>Legend:</b> Colored by ", input$color_mode))
    }
  })
  
  observeEvent(input$refresh_network, {
    vis <- visNetwork(filtered_nodes(), filtered_edges()) %>%
      visNodes(font=list(size=24)) %>%
      visEdges(smooth=TRUE, color=list(highlight="red")) %>%
      visOptions(highlightNearest=list(enabled=TRUE, degree=1),
                 nodesIdSelection=TRUE) %>%
      visPhysics(enabled=input$enable_physics,
                 solver="forceAtlas2Based",
                 barnesHut=list(gravitationalConstant=-3000,
                                centralGravity=0.3,
                                springLength=150,
                                springConstant=0.04),
                 minVelocity=0.75)
    output$network <- renderVisNetwork({vis})
  }, ignoreInit=FALSE)
  
  nodes_for_stats <- reactive({
    filtered_nodes() %>%
      transmute(h_index = as.numeric(h_index),
                prestige = as.numeric(prestige),
                inbred = as.numeric(inbred),
                career_years = as.numeric(career_years)) %>%
      drop_na(h_index)
  })
  
  output$h_hist <- renderPlot({
    df <- nodes_for_stats(); if(nrow(df)==0) return(NULL)
    ggplot(df, aes(h_index)) +
      geom_histogram(bins=20, color="black", fill="white") +
      theme_bw() + labs(x="H-index", y="Count")
  })
  
  output$h_box <- renderPlot({
    df <- nodes_for_stats(); if(nrow(df)==0) return(NULL)
    df <- df %>% mutate(
      Prestige = factor(prestige, levels=c(0,1), labels=c("No","Yes")),
      Inbred = factor(inbred, levels=c(0,1), labels=c("No","Yes"))
    )
    p1 <- ggplot(df, aes(Prestige, h_index)) + geom_boxplot() + theme_bw()
    p2 <- ggplot(df, aes(Inbred, h_index)) + geom_boxplot() + theme_bw()
    gridExtra::grid.arrange(p1,p2,ncol=1)
  })
  
  output$lm_out <- renderPrint({
    df <- nodes_for_stats(); if(nrow(df)<10){cat("Not enough data.\n");return()}
    summary(lm(h_index ~ prestige + inbred + career_years, data=df))
  })
  
  output$qr_out <- renderPrint({
    df <- nodes_for_stats(); if(nrow(df)<20){cat("Not enough data.\n");return()}
    taus <- seq(0.1, 0.9, 0.2)
    summary(rq(h_index ~ career_years + prestige + inbred, tau=taus, data=df))
  })
}

# ================================
# Run App
# ================================
shinyApp(ui, server)
