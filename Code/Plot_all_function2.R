plot_all <- function(sample_data_4,
                     sample_data_11 = NULL,
                     families = c("respiration","respiration2","DNA_Yield",
                                  "biomass","enzyme","FDA","POXC","soil","CUE","MAOM"),
                     respiration_mode = c("by_addition","by_site")) {
  
  respiration_mode <- match.arg(respiration_mode)
  lvl_aggr <- c("A","B","C","D","Bulk")
  
  sample_data_4 <- sample_data_4 %>%
    dplyr::mutate(
      Aggregate = factor(Aggregate, levels = lvl_aggr),
      Site      = as.factor(Site)
    )
  
  sample_data_4<-  sample_data_4%>%
    mutate(fPOM_TC = fPOM_TC * proportion.fPOM.by.mass,    # Converting from per OM fraction to per aggregate
           oPOM_TC = oPOM_TC * proportion.oPOM.by.mass,
           MAOM_TC = MAOM_TC * proportion.MAOM.by.mass,
           fPOM_TN = fPOM_TN * proportion.fPOM.by.mass,
           oPOM_TN= oPOM_TN * proportion.oPOM.by.mass,
           MAOM_TN = MAOM_TN * proportion.MAOM.by.mass)
  
  
  
  # Join in soil data (TC, TN.1, C%, N., etc.) when requested
  if ("soil" %in% families && !is.null(sample_data_11)) {
    sample_data_4 <- sample_data_4 %>%
      dplyr::left_join(sample_data_11, by = "ID")
  }
  
  ylab_map <- list(
    CO2   = expression(CO[2] ~ (mu*g ~ day^{-1} ~ g^{-1}  ~ aggregate)),
    N2O   = expression(N[2]*O ~ (mu*g ~ day^{-1} ~ g^{-1}  ~ aggregate)),
    CH4   = expression(CH[4] ~ (mu*g ~ day^{-1} ~ g^{-1} ~ aggregate)),
    MBC   = expression(MBC ~ (mu*g ~ C ~ g^{-1}  ~ aggregate)),
    MBN   = expression(MBN ~ (mu*g ~ N ~ g^{-1}  ~ aggregate)),
    TOC   = expression(NPOC ~ (mu*g ~ C ~ g^{-1}  ~ aggregate)),
    TN    = expression(TDN ~ (mu*g ~ N ~ g^{-1} ~ aggregate)),
    BG    = expression(BG ~ (nmol ~ g^{-1} ~ aggregate ~ hr^{-1})),
    CBH   = expression(CBH ~ (nmol ~ g^{-1} ~ aggregate ~ hr^{-1})),
    LAP   = expression(LAP ~ (nmol ~ g^{-1} ~ aggregate ~ hr^{-1})),
    NAG   = expression(NAG ~ (nmol ~ g^{-1} ~ aggregate ~ hr^{-1})),
    PHOS  = expression(PHOS ~ (nmol ~ g^{-1} ~ aggregate ~ hr^{-1})),
    FDA   = expression(FDA ~ (mu*g ~ hr^{-1} ~ g^{-1} ~ dry ~ aggregate)),
    POXC  = expression(POXC ~ (mu*g ~ g^{-1}  ~ aggregate)),
    CUE   = "CUE",
    NO3   = "NO3 (ppm)", 
    NH4   = "NH4 (ppm)", 
    OlsenP = "OlsenP (ppm)",
    X_Ca  = "X-Ca (meq/100g)", 
    X_Mg  = "X-Mg (meq/100g)", 
    X_Na  = "X-Na (ppm)",
    pH    = "pH", 
    CEC   = "CEC (meq/100g)", 
    LOI   = "LOI (%)", 
    X_K   = "X-K (ppm)",
    
    # ---- POM/MAOM labels ----
    `fPOM_TC`   = expression(fPOM ~ Total~C ~ (g ~ kg^{-1} ~ aggregate)),
    `oPOM_TC`   = expression(oPOM ~ Total~C ~ (g ~ kg^{-1} ~ aggregate)),
    `MAOM_TC`   = expression(MAOM ~ Total~C ~ (g ~ kg^{-1} ~ aggregate)),
    `fPOM_TN`   = expression(fPOM ~ Total~N ~ (g ~ kg^{-1} ~ aggregate)),
    `oPOM_TN`   = expression(oPOM ~ Total~N ~ (g ~ kg^{-1} ~ aggregate)),
    `MAOM_TN`   = expression(MAOM ~ Total~N ~ (g ~ kg^{-1} ~ aggregate)),
    `fPOM_13C.` = expression(fPOM ~ {}^{13}*C ~ "(%)"),
    `oPOM_13C.` = expression(oPOM ~ {}^{13}*C ~ "(%)"),
    `MAOM_13C.` = expression(MAOM ~ {}^{13}*C ~ "(%)"),
    `fPOM_15N.` = expression(fPOM ~ {}^{15}*N ~ "(%)"),
    `oPOM_15N.` = expression(oPOM ~ {}^{15}*N ~ "(%)"),
    `MAOM_15N.` = expression(MAOM ~ {}^{15}*N ~ "(%)"),
    
    # ---- NEW: whole-soil TC, TN.1, C%, N% ----
    `TC`   = expression(Soil ~ Total~C ~ (g ~ kg^{-1} ~ aggregate)),
    `TN.1` = expression(Soil ~ Total~N ~ (g ~ kg^{-1} ~ aggregate)),
    `C.`   = expression(Soil ~ {}^{13}*C ~ "(%)"),
    `N.`   = expression(Soil ~ {}^{15}*N ~ "(%)")
  )
  
  agg_labels <- c(
    "A"    = ">2 mm",
    "B"    = "2-0.25 mm",
    "C"    = "0.25-0.053 mm",
    "D"    = "< 0.053 mm",
    "Bulk" = "Bulk"
  )
  
  make_boxplot <- function(df, var, facet = c("Site","Addition","none"),
                           palette = cbPalette6, x_title = "", use_theme_bp = TRUE) {
    facet <- match.arg(facet)
    yl <- ylab_map[[var]]
    
    base <- ggplot2::ggplot(df, ggplot2::aes(
      x     = as.factor(Aggregate),
      y     = .data[[var]],
      color = Aggregate
    )) +
      ggplot2::geom_boxplot(
        show.legend     = FALSE,
        outlier.colour  = NA,
        outlier.fill    = NA,
        alpha           = 0.2
      ) +
      ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.8)) +
      ggplot2::scale_color_manual(values = palette, labels = agg_labels) +
      ggplot2::scale_x_discrete(labels = agg_labels) +
      { if (is.language(yl) || is.expression(yl)) ggplot2::ylab(yl) else ggplot2::ylab(yl %||% var) } +
      ggplot2::xlab(x_title) +
      { if (use_theme_bp) theme_CKMBP2() else theme_CKM2() }
    
    if (facet == "Site") {
      base <- base +
        ggplot2::facet_wrap(
          ~ Site,
          labeller = ggplot2::labeller(Site = c("C"="Conventional","O"="Organic"))
        )
    } else if (facet == "Addition") {
      base <- base + ggplot2::facet_wrap(~ Addition)
    }
    
    base
  }
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  plots <- list()
  
  # ---------- respiration ----------
  if ("respiration" %in% families) {
    for (v in c("CO2","N2O","CH4")) {
      plots[[paste0(v,"_plot")]] <-
        make_boxplot(sample_data_4, v, facet = "Addition", x_title = "")
    }
  }
  
  if ("respiration2" %in% families) {
    for (v in c("CO2","N2O","CH4")) {
      plots[[paste0(v,"_plot_site")]] <-
        make_boxplot(sample_data_4, v, facet = "Site", x_title = "")
    }
  }
  
  # ---------- DNA yield ----------
  if ("DNA_Yield" %in% families && "Extraction.number" %in% names(sample_data_4)) {
    plots$DNA_Yield <-
      ggplot2::ggplot(sample_data_4,
                      ggplot2::aes(x = as.factor(Site),
                                   y = Extraction.number,
                                   color = Aggregate)) +
      ggplot2::geom_boxplot(
        show.legend     = FALSE,
        outlier.colour  = NA,
        outlier.fill    = NA,
        alpha           = 0.2
      ) +
      ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.8)) +
      ggplot2::ylab("DNA yield") +
      ggplot2::xlab("Field ID") +
      ggplot2::scale_x_discrete(labels = c("O"="Organic","C"="Conventional")) +
      ggplot2::scale_color_manual(values = cbPalette6, labels = agg_labels) +
      theme_CKM2()
  }
  
  # ---------- biomass ----------
  if ("biomass" %in% families) {
    for (v in c("MBC","MBN","TOC","TN")) {
      plots[[paste0(v,"_plot")]] <-
        make_boxplot(sample_data_4, v, facet = "Site", x_title = "Field ID")
    }
  }
  
  # ---------- enzyme ----------
  if ("enzyme" %in% families &&
      all(c("BG","CBH","LAP","NAG","PHOS") %in% names(sample_data_4))) {
    
    for (v in c("BG","CBH","LAP","NAG","PHOS")) {
      plots[[paste0(v,"_plot")]] <-
        make_boxplot(sample_data_4, v, facet = "Site", x_title = "Field ID")
    }
    
    PHOSL <-
      ggplot2::ggplot(sample_data_4,
                      ggplot2::aes(x = as.factor(Aggregate),
                                   y = PHOS,
                                   color = Aggregate)) +
      ggplot2::geom_boxplot(
        show.legend     = TRUE,
        outlier.colour  = NA,
        outlier.fill    = NA,
        alpha           = 0.2
      ) +
      ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.8)) +
      ggplot2::scale_color_manual(values = cbPalette6, labels = agg_labels) +
      theme_CKMBP2()
    
    legend <- cowplot::get_legend(PHOSL)
    
    plots$gg_enzyme <- cowplot::plot_grid(
      plots$BG_plot,
      plots$CBH_plot,
      plots$LAP_plot,
      plots$NAG_plot,
      plots$PHOS_plot,
      legend,
      nrow      = 2,
      align     = "v",
      label_size = 14,
      vjust     = -0.5
    )
  }
  
  # ---------- FDA / POXC ----------
  if ("FDA" %in% families) {
    plots$FDA_plot <- make_boxplot(sample_data_4, "FDA", facet = "Site", x_title = "Field ID")
  }
  
  if ("POXC" %in% families) {
    plots$POXC_plot <- make_boxplot(sample_data_4, "POXC", facet = "Site", x_title = "")
  }
  
  # ---------- soil (including TC, TN.1, C%, N.) ----------
  if ("soil" %in% families) {
    soil_vars <- c(
      "TC","TN.1","C.","N.",
      "NO3","NH4","OlsenP","X_Ca","X_Mg","X_Na","pH","CEC","LOI","X_K"
    )
    
    for (v in soil_vars) {
      if (v %in% names(sample_data_4)) {
        plots[[paste0(v,"_plot")]] <-
          make_boxplot(sample_data_4, v, facet = "Site", x_title = "Field ID")
      }
    }
  }
  
  # ---------- CUE ----------
  if ("CUE" %in% families && "CUE" %in% names(sample_data_4)) {
    sdf <- sample_data_4 %>%
      dplyr::filter(CUE >= 0, CUE <= 1)
    plots$CUE_plot <- make_boxplot(sdf, "CUE", facet = "Site", x_title = "")
  }
  
  # ---------- MAOM/fPOM/oPOM ----------
  if ("MAOM" %in% families) {
    vars_maom <- c(
      "fPOM_TC","oPOM_TC","MAOM_TC",
      "fPOM_TN","oPOM_TN","MAOM_TN",
      "fPOM_13C.","oPOM_13C.","MAOM_13C.",
      "fPOM_15N.","oPOM_15N.","MAOM_15N."
    )
    
    vars_present <- intersect(vars_maom, names(sample_data_4))
    
    for (v in vars_present) {
      plots[[paste0(v,"_plot")]] <-
        make_boxplot(sample_data_4, v, facet = "Site", x_title = "Field ID")
    }
  }
  
  return(plots)
}


# Plot MAOM/fPOM/oPOM together with x=Aggregate, y=(TC/TN/13C%/15N%), color=Pool
plot_MAOM_pools <- function(sample_data_4,
                            facet_by = c("both","site","metric","none"),
                            palette = NULL) {
  facet_by <- match.arg(facet_by)
  
  # Factor order
  lvl_aggr <- c("A","B","C","D","Bulk")
  df <- sample_data_4 %>%
    dplyr::mutate(Aggregate = factor(Aggregate, levels = lvl_aggr),
                  Site = as.factor(Site))
  
  # Columns present
  cols_expected <- c("fPOM_TC","oPOM_TC","MAOM_TC",
                     "fPOM_TN","oPOM_TN","MAOM_TN",
                     "fPOM_13C%","oPOM_13C%","MAOM_13C%",
                     "fPOM_15N%","oPOM_15N%","MAOM_15N%")
  cols_present <- intersect(cols_expected, names(df))
  if (!length(cols_present)) stop("No MAOM/fPOM/oPOM columns found in data.")
  
  # Long form
  long_df <- df %>%
    tidyr::pivot_longer(dplyr::all_of(cols_present),
                        names_to = "PoolMetric", values_to = "Value") %>%
    dplyr::mutate(
      Pool   = sub("_(TC|TN|13C%|15N%)$", "", PoolMetric),
      Metric = sub("^.*_", "", PoolMetric),
      Pool   = factor(Pool, levels = c("MAOM","fPOM","oPOM")),
      Metric = factor(Metric, levels = c("TC","TN","13C%","15N%"))
    )
  
  # Labels & palette
  agg_labels  <- c("A"=">2 mm","B"="2-0.25 mm","C"="0.25-0.053 mm","D"="< 0.053 mm","Bulk"="Bulk")
  metric_labs <- c("TC"="Total C (g kg^-1)",
                   "TN"="Total N (g kg^-1)",
                   "13C%"="13C (%)",
                   "15N%"="15N (%)")
  
  if (is.null(palette)) {
    if (exists("cbPalette3", inherits = TRUE)) {
      palette <- c("MAOM"=cbPalette3[1], "fPOM"=cbPalette3[2], "oPOM"=cbPalette3[3])
    } else if (exists("cbPalette6", inherits = TRUE)) {
      palette <- c("MAOM"=cbPalette6[1], "fPOM"=cbPalette6[2], "oPOM"=cbPalette6[3])
    } else {
      palette <- c("MAOM"="#1b9e77", "fPOM"="#d95f02", "oPOM"="#7570b3")
    }
  }
  
  # Use the same dodge for BOTH layers
  dodge <- ggplot2::position_dodge(width = 0.72)
  
  base <- ggplot2::ggplot(
    long_df,
    ggplot2::aes(x = Aggregate, y = Value, fill = Pool, color = Pool)  # fill defines grouping for boxplot
  ) +
    ggplot2::geom_boxplot(
      position = dodge, width = 0.64,
      outlier.shape = NA, alpha = 0.22, na.rm = TRUE, show.legend = TRUE
    ) +
    ggplot2::geom_point(
      position = dodge,
      size = 1.8, alpha = 0.9, na.rm = TRUE, show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_x_discrete(labels = agg_labels, drop = FALSE) +
    ggplot2::xlab("Aggregate fraction") +
    ggplot2::ylab(NULL) +
    { if (use_theme_bp) theme_CKMBP2() else theme_CKM2() } +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Pool"), color = "none")
  
  # Facets with free y so TN isn't dwarfed by TC
  if (facet_by == "both") {
    base <- base + ggplot2::facet_grid(
      Metric ~ Site,
      labeller = ggplot2::labeller(
        Metric = metric_labs,
        Site   = c("C"="Conventional","O"="Organic")
      ),
      scales = "free_y"
    )
  } else if (facet_by == "site") {
    base <- base + ggplot2::facet_wrap(
      ~ Site,
      labeller = ggplot2::labeller(Site = c("C"="Conventional","O"="Organic")),
      scales = "free_y"
    )
  } else if (facet_by == "metric") {
    base <- base + ggplot2::facet_wrap(
      ~ Metric,
      labeller = ggplot2::labeller(Metric = metric_labs),
      nrow = 2, scales = "free_y"
    )
  }
  
  return(base)
}



plot_MAOM_pairs_within_pool <- function(sample_data_4,
                                        facet_by     = c("metric","none"),
                                        site_order   = c("C","O"),   # order of C/O inside each pool
                                        palette_base = NULL) {
  facet_by <- match.arg(facet_by)
  
  # --- Standardize column names: 13C. / 15N. -> 13C% / 15N% ---
  df <- sample_data_4
  nm <- names(df)
  nm <- sub("(fPOM|oPOM|MAOM)_13C\\.$", "\\1_13C%", nm)
  nm <- sub("(fPOM|oPOM|MAOM)_15N\\.$", "\\1_15N%", nm)
  names(df) <- nm
  
  # --- Factors & order ---
  lvl_aggr <- c("A","B","C","D","Bulk")
  site_order <- match.arg(site_order, choices = c("C","O"), several.ok = TRUE)
  if (length(site_order) != 2) site_order <- c("C","O")
  
  df <- df %>%
    dplyr::mutate(
      Aggregate = factor(Aggregate, levels = lvl_aggr),
      Site      = factor(Site, levels = site_order)
    ) 
  
  df <- df %>%
    mutate(
      `oPOM_15N%` = na_if(`oPOM_15N%`, 0),
      `oPOM_13C%` = na_if(`oPOM_13C%`, 0)
    )
  # --- Columns present ---
  cols_expected <- c("fPOM_TC","oPOM_TC","MAOM_TC",
                     "fPOM_TN","oPOM_TN","MAOM_TN",
                     "fPOM_13C%","oPOM_13C%","MAOM_13C%",
                     "fPOM_15N%","oPOM_15N%","MAOM_15N%")
  cols_present <- intersect(cols_expected, names(df))
  if (!length(cols_present)) stop("No MAOM/fPOM/oPOM columns found in data.")
  
  # --- Long form ---
  long_df <- df %>%
    tidyr::pivot_longer(dplyr::all_of(cols_present),
                        names_to = "PoolMetric", values_to = "Value") %>%
    dplyr::mutate(
      Pool     = sub("_(TC|TN|13C%|15N%)$", "", PoolMetric),
      Metric   = sub("^.*_", "", PoolMetric),
      Pool     = factor(Pool, levels = c("MAOM","fPOM","oPOM")),
      Metric   = factor(Metric, levels = c("TC","TN","13C%","15N%")),
      PoolSite = interaction(Pool, Site, sep = ".")
    ) %>%
    dplyr::filter(is.finite(Value))
  
  # Order groups inside dodge so we get: MAOM.C, MAOM.O, fPOM.C, fPOM.O, oPOM.C, oPOM.O
  long_df <- long_df %>%
    dplyr::mutate(
      PoolSite = factor(
        PoolSite,
        levels = c(paste0("MAOM.", site_order),
                   paste0("fPOM.", site_order),
                   paste0("oPOM.", site_order))
      )
    )
  
  # --- Colors: two-tone per pool (lighter = first site, darker = second) ---
  if (is.null(palette_base)) {
    if (exists("cbPalette3", inherits = TRUE)) {
      palette_base <- c(MAOM = cbPalette3[1], fPOM = cbPalette3[2], oPOM = cbPalette3[3])
    } else if (exists("cbPalette6", inherits = TRUE)) {
      palette_base <- c(MAOM = cbPalette6[1], fPOM = cbPalette6[2], oPOM = cbPalette6[3])
    } else {
      palette_base <- c(MAOM = "#1b9e77", fPOM = "#d95f02", oPOM = "#7570b3")
    }
  }
  darken  <- function(col, f = 0.20) { rgb <- grDevices::col2rgb(col)/255; grDevices::rgb((1-f)*rgb[1], (1-f)*rgb[2], (1-f)*rgb[3]) }
  lighten <- function(col, f = 0.12) { rgb <- grDevices::col2rgb(col)/255; grDevices::rgb(pmin(1, rgb[1]+(1-rgb[1])*f), pmin(1, rgb[2]+(1-rgb[2])*f), pmin(1, rgb[3]+(1-rgb[3])*f)) }
  
  # Build vector, then name it (this avoids the 'unexpected =' error)
  fill_values <- c(
    lighten(palette_base["MAOM"], 0.12),
    darken (palette_base["MAOM"], 0.20),
    lighten(palette_base["fPOM"], 0.12),
    darken (palette_base["fPOM"], 0.20),
    lighten(palette_base["oPOM"], 0.12),
    darken (palette_base["oPOM"], 0.20)
  )
  names(fill_values) <- c(
    paste0("MAOM.", site_order[1]), paste0("MAOM.", site_order[2]),
    paste0("fPOM.", site_order[1]), paste0("fPOM.", site_order[2]),
    paste0("oPOM.", site_order[1]), paste0("oPOM.", site_order[2])
  )
  
  # --- Plot: x = Aggregate; dodge by PoolSite so each pool shows C|O side-by-side ---
  dodge <- ggplot2::position_dodge(width = 0.9)
  
  agg_labels <- c("A"=">2 mm","B"="2-0.25 mm","C"="0.25-0.053 mm","D"="< 0.053 mm","Bulk"="Bulk")
  metric_labs <- c("TC"="Total C (g kg^-1)",
                   "TN"="Total N (g kg^-1)",
                   "13C%"="13C (%)",
                   "15N%"="15N (%)")
  
  p <- ggplot2::ggplot(
    long_df,
    ggplot2::aes(x = Aggregate, y = Value, fill = PoolSite, color = PoolSite)
  ) +
    ggplot2::geom_boxplot(position = dodge, width = 0.75,
                          outlier.shape = NA, alpha = 0.22, show.legend = TRUE) +
    ggplot2::geom_point(position = dodge, size = 1.8, alpha = 0.9, show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = fill_values) +
    ggplot2::scale_color_manual(values = fill_values, guide = "none") +
    ggplot2::scale_x_discrete(labels = agg_labels, drop = FALSE) +
    ggplot2::xlab("Aggregate fraction") +
    ggplot2::ylab(NULL) +
    theme_CKMBP2() +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "OM fraction"))
  
  if (facet_by == "metric") {
    p <- p + ggplot2::facet_wrap(~ Metric,
                                 labeller = ggplot2::labeller(Metric = metric_labs),
                                 nrow = 2, scales = "free_y")
  }
  
  p
}


plot_MAOM_pairs_within_pool2 <- function(sample_data_4,
                                        facet_by     = c("metric_pool","metric","none"),
                                        site_order   = c("C","O"),   # order of C/O
                                        palette_base = NULL) {
  facet_by <- match.arg(facet_by)

  # --- Standardize column names: 13C. / 15N. -> 13C% / 15N% ---
  df <- sample_data_4
  nm <- names(df)
  nm <- sub("(fPOM|oPOM|MAOM)_13C\\.$", "\\1_13C%", nm)
  nm <- sub("(fPOM|oPOM|MAOM)_15N\\.$", "\\1_15N%", nm)
  names(df) <- nm
  
  sample_data_4<- sample_data_4 %>%
    mutate( fPOM_C = fPOM_TC * proportion.fPOM.by.mass,
            oPOM_C= oPOM_TC * proportion.oPOM.by.mass,
            MAOM_C= MAOM_TC * proportion.MAOM.by.mass,
            fPOM_N = fPOM_TN * proportion.fPOM.by.mass,
            oPOM_N= oPOM_TN * proportion.oPOM.by.mass,
            MAOM_N= MAOM_TN * proportion.MAOM.by.mass
    )

  # --- Factors & order ---
  lvl_aggr   <- c("A","B","C","D","Bulk")
  site_order <- match.arg(site_order, choices = c("C","O"), several.ok = TRUE)
  if (length(site_order) != 2) site_order <- c("C","O")
  
  df <- df %>%
    dplyr::mutate(
      Aggregate = factor(Aggregate, levels = lvl_aggr),
      Site      = factor(Site, levels = site_order)
    )
  
  # Drop those zero oPOM isotope values
  df <- df %>%
    dplyr::mutate(
      `oPOM_15N%` = dplyr::na_if(`oPOM_15N%`, 0),
      `oPOM_13C%` = dplyr::na_if(`oPOM_13C%`, 0)
    )
  
  # --- Columns present ---
  cols_expected <- c("fPOM_C","oPOM_C","MAOM_C",
                     "fPOM_N","oPOM_N","MAOM_N",
                     "fPOM_13C%","oPOM_13C%","MAOM_13C%",
                     "fPOM_15N%","oPOM_15N%","MAOM_15N%")
  cols_present <- intersect(cols_expected, names(df))
  if (!length(cols_present)) stop("No MAOM/fPOM/oPOM columns found in data.")
  
  # --- Long form ---
  long_df <- df %>%
    tidyr::pivot_longer(dplyr::all_of(cols_present),
                        names_to = "PoolMetric", values_to = "Value") %>%
    dplyr::mutate(
      Pool   = sub("_(C|N|13C%|15N%)$", "", PoolMetric),
      Metric = sub("^.*_", "", PoolMetric),
      Pool   = factor(Pool,   levels = c("MAOM","fPOM","oPOM")),
      Metric = factor(Metric, levels = c("C","N","13C%","15N%"))
    ) %>%
    dplyr::filter(is.finite(Value))
  
  # --- Colors: by Site (O vs C) ---
  if (is.null(palette_base)) {
    if (exists("cbPalette2", inherits = TRUE)) {
      palette_base <- cbPalette2[1:2]
      names(palette_base) <- site_order
    } else {
      palette_base <- c("C" = "#1b9e77", "O" = "#d95f02")
      # reorder to match site_order
      palette_base <- palette_base[site_order]
    }
  } else {
    # ensure palette_base is named by site_order
    if (is.null(names(palette_base)) || !all(site_order %in% names(palette_base))) {
      names(palette_base) <- site_order
    }
    palette_base <- palette_base[site_order]
  }
  
  dodge <- ggplot2::position_dodge(width = 0.9)
  
  agg_labels <- c("A"    = ">2 mm",
                  "B"    = "2–0.25 mm",
                  "C"    = "0.25–0.053 mm",
                  "D"    = "<0.053 mm",
                  "Bulk" = "Bulk")
  
  metric_labs <- c(
    "C"   = "Total C (g kg^-1)",
    "N"   = "Total N (g kg^-1)",
    "13C%" = "13C (%)",
    "15N%" = "15N (%)"
  )
  
  p <- ggplot2::ggplot(
    long_df,
    ggplot2::aes(x = Aggregate, y = Value, fill = Site, color = Site)
  ) +
    ggplot2::geom_boxplot(
      ggplot2::aes(group = interaction(Aggregate, Site)),
      position     = dodge,
      width        = 0.75,
      outlier.shape = NA,
      alpha        = 0.25,
      show.legend  = TRUE
    ) +
    ggplot2::geom_point(
      position    = dodge,
      size        = 1.8,
      alpha       = 0.9,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = palette_base) +
    ggplot2::scale_color_manual(values = palette_base, guide = "none") +
    ggplot2::scale_x_discrete(labels = agg_labels, drop = FALSE) +
    ggplot2::xlab("Aggregate fraction") +
    ggplot2::ylab(NULL) +
    theme_CKMBP2() +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Site"))
  
  # --- Faceting options ---
  if (facet_by == "metric_pool") {
    p <- p +
      ggplot2::facet_wrap(
        ~ Metric + Pool,
        scales = "free_y",    # each panel gets its own y-scale
        nrow   = 4,           # 4 rows (TC, TN, 13C%, 15N%), 3 cols (MAOM, fPOM, oPOM)
        labeller = ggplot2::labeller(Metric = metric_labs)
      )
  } else if (facet_by == "metric") {
    # old behavior: just by metric
    p <- p +
      ggplot2::facet_wrap(
        ~ Metric,
        labeller = ggplot2::labeller(Metric = metric_labs),
        nrow = 2,
        scales = "free_y"
      )
  } else if (facet_by == "none") {
    # no facets; everything on one panel
    # (you’ll see all pools & metrics overlaid)
  }
  
  p
}




plot_MAOM_pairs_within_pool3 <- function(sample_data_4,
                                         facet_by     = c("metric_pool","metric","none"),
                                         site_order   = c("C","O"),   # order of C/O
                                         palette_base = NULL) {
  facet_by <- match.arg(facet_by)
  
  # --- Standardize column names: 13C. / 15N. -> 13C% / 15N% ---
  df <- sample_data_4
  nm <- names(df)
  nm <- sub("(fPOM|oPOM|MAOM)_13C\\.$", "\\1_13C%", nm)
  nm <- sub("(fPOM|oPOM|MAOM)_15N\\.$", "\\1_15N%", nm)
  names(df) <- nm
  
  sample_data_4<- sample_data_4 %>%
    mutate( fPOMC = fPOM_TC * proportion.fPOM.by.mass,
            oPOMC= oPOM_TC * proportion.oPOM.by.mass,
            MAOMC= MAOM_TC * proportion.MAOM.by.mass,
            fPOMN = fPOM_TN * proportion.fPOM.by.mass,
            oPOMN= oPOM_TN * proportion.oPOM.by.mass,
            MAOMN= MAOM_TN * proportion.MAOM.by.mass
    )
  
  # --- Factors & order ---
  lvl_aggr   <- c("A","B","C","D","Bulk")
  site_order <- match.arg(site_order, choices = c("C","O"), several.ok = TRUE)
  if (length(site_order) != 2) site_order <- c("C","O")
  
  df <- df %>%
    dplyr::mutate(
      Aggregate = factor(Aggregate, levels = lvl_aggr),
      Site      = factor(Site, levels = site_order)
    )
  
  # Drop those zero oPOM isotope values
  df <- df %>%
    dplyr::mutate(
      `oPOM_15N%` = dplyr::na_if(`oPOM_15N%`, 0),
      `oPOM_13C%` = dplyr::na_if(`oPOM_13C%`, 0)
    )
  
  # --- Columns present ---
  cols_expected <- c("fPOM_TC","oPOM_TC","MAOM_TC",
                     "fPOM_TN","oPOM_TN","MAOM_TN",
                     "fPOM_13C%","oPOM_13C%","MAOM_13C%",
                     "fPOM_15N%","oPOM_15N%","MAOM_15N%")
  cols_present <- intersect(cols_expected, names(df))
  if (!length(cols_present)) stop("No MAOM/fPOM/oPOM columns found in data.")
  
  # --- Long form ---
  long_df <- df %>%
    tidyr::pivot_longer(dplyr::all_of(cols_present),
                        names_to = "PoolMetric", values_to = "Value") %>%
    dplyr::mutate(
      Pool   = sub("_(TC|TN|13C%|15N%)$", "", PoolMetric),
      Metric = sub("^.*_", "", PoolMetric),
      Pool   = factor(Pool,   levels = c("MAOM","fPOM","oPOM")),
      Metric = factor(Metric, levels = c("TC","TN","13C%","15N%"))
    ) %>%
    dplyr::filter(is.finite(Value))
  
  # --- Colors: by Site (O vs C) ---
  if (is.null(palette_base)) {
    if (exists("cbPalette2", inherits = TRUE)) {
      palette_base <- cbPalette2[1:2]
      names(palette_base) <- site_order
    } else {
      palette_base <- c("C" = "#1b9e77", "O" = "#d95f02")
      # reorder to match site_order
      palette_base <- palette_base[site_order]
    }
  } else {
    # ensure palette_base is named by site_order
    if (is.null(names(palette_base)) || !all(site_order %in% names(palette_base))) {
      names(palette_base) <- site_order
    }
    palette_base <- palette_base[site_order]
  }
  
  dodge <- ggplot2::position_dodge(width = 0.9)
  
  agg_labels <- c("A"    = ">2 mm",
                  "B"    = "2–0.25 mm",
                  "C"    = "0.25–0.053 mm",
                  "D"    = "<0.053 mm",
                  "Bulk" = "Bulk")
  
  metric_labs <- c(
    "TC"   = "Total C (g kg^-1)",
    "TN"   = "Total N (g kg^-1)",
    "13C%" = "13C (%)",
    "15N%" = "15N (%)"
  )
  
  p <- ggplot2::ggplot(
    long_df,
    ggplot2::aes(x = Aggregate, y = Value, fill = Site, color = Site)
  ) +
    ggplot2::geom_boxplot(
      ggplot2::aes(group = interaction(Aggregate, Site)),
      position     = dodge,
      width        = 0.75,
      outlier.shape = NA,
      alpha        = 0.25,
      show.legend  = TRUE
    ) +
    ggplot2::geom_point(
      position    = dodge,
      size        = 1.8,
      alpha       = 0.9,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = palette_base) +
    ggplot2::scale_color_manual(values = palette_base, guide = "none") +
    ggplot2::scale_x_discrete(labels = agg_labels, drop = FALSE) +
    ggplot2::xlab("Aggregate fraction") +
    ggplot2::ylab(NULL) +
    theme_CKMBP2() +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Site"))
  
  # --- Faceting options ---
  if (facet_by == "metric_pool") {
    p <- p +
      ggplot2::facet_wrap(
        ~ Metric + Pool,
        scales = "free_y",    # each panel gets its own y-scale
        nrow   = 4,           # 4 rows (TC, TN, 13C%, 15N%), 3 cols (MAOM, fPOM, oPOM)
        labeller = ggplot2::labeller(Metric = metric_labs)
      )
  } else if (facet_by == "metric") {
    # old behavior: just by metric
    p <- p +
      ggplot2::facet_wrap(
        ~ Metric,
        labeller = ggplot2::labeller(Metric = metric_labs),
        nrow = 2,
        scales = "free_y"
      )
  } else if (facet_by == "none") {
    # no facets; everything on one panel
    # (you’ll see all pools & metrics overlaid)
  }
  
  p
}
