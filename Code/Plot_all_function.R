plot_all <- function(sample_data_4,
                     sample_data_11 = NULL,
                     families = c("respiration","respiration2","DNA_Yield",
                                  "biomass","enzyme","FDA","POXC","soil","CUE"),
                     respiration_mode = c("by_addition","by_site")) {
  
  respiration_mode <- match.arg(respiration_mode)
  lvl_aggr <- c("A","B","C","D","Bulk")
  sample_data_4 <- sample_data_4 %>%
    dplyr::mutate(Aggregate = factor(Aggregate, levels = lvl_aggr),
                  Site = as.factor(Site))
  if ("soil" %in% families && !is.null(sample_data_11)) {
    sample_data_4 <- sample_data_4 %>% dplyr::left_join(sample_data_11, by = "ID")
  }
  
  ylab_map <- list(
    CO2   = expression(CO[2] ~ (mu*g ~ day^{-1} ~ g^{-1} ~ dry ~ aggregate)),
    N2O   = expression(N[2]*O ~ (mu*g ~ day^{-1} ~ g^{-1} ~ dry ~ aggregate)),
    CH4   = expression(CH[4] ~ (mu*g ~ day^{-1} ~ g^{-1} ~ dry ~ aggregate)),
    MBC   = expression(MBC ~ (mu*g ~ C ~ g^{-1} ~ dry ~ aggregate)),
    MBN   = expression(MBN ~ (mu*g ~ N ~ g^{-1} ~ dry ~ aggregate)),
    TOC   = expression(NPOC ~ (mu*g ~ C ~ g^{-1} ~ dry ~ aggregate)),
    TN    = expression(TDN ~ (mu*g ~ N ~ g^{-1} ~ dry ~ aggregate)),
    BG    = expression(BG ~ (nmol ~ g^{-1} ~ aggregate ~ hr^{-1})),
    CBH   = expression(CBH ~ (nmol ~ g^{-1} ~ aggregate ~ hr^{-1})),
    LAP   = expression(LAP ~ (nmol ~ g^{-1} ~ aggregate ~ hr^{-1})),
    NAG   = expression(NAG ~ (nmol ~ g^{-1} ~ aggregate ~ hr^{-1})),
    PHOS  = expression(PHOS ~ (nmol ~ g^{-1} ~ aggregate ~ hr^{-1})),
    FDA   = expression(FDA ~ (mu*g ~ hr^{-1} ~ g^{-1} ~ dry ~ aggregate)),
    POXC  = expression(POXC ~ (mu*g ~ g^{-1} ~ dry ~ aggregate)),
    CUE   = "CUE",
    NO3   = "NO3 (ppm)", NH4 = "NH4 (ppm)", OlsenP = "OlsenP (ppm)",
    X_Ca  = "X-Ca (meq/100g)", X_Mg = "X-Mg (meq/100g)", X_Na = "X-Na (ppm)",
    pH = "pH", CEC = "CEC (meq/100g)", LOI = "LOI (%)", X_K = "X-K (ppm)"
  )
  agg_labels <- c("A"=">2 mm","B"="2-0.25 mm","C"="0.25-0.053 mm","D"="< 0.053 mm","Bulk"="Bulk")
  
  make_boxplot <- function(df, var, facet = c("Site","Addition","none"),
                           palette = cbPalette6, x_title = "", use_theme_bp = TRUE) {
    facet <- match.arg(facet)
    yl <- ylab_map[[var]]
    base <- ggplot2::ggplot(df, ggplot2::aes(x = as.factor(Aggregate), y = .data[[var]], color = Aggregate)) +
      ggplot2::geom_boxplot(show.legend = FALSE, outlier.colour = NA, outlier.fill = NA, alpha = 0.2) +
      ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.8)) +
      ggplot2::scale_color_manual(values = palette, labels = agg_labels) +
      ggplot2::scale_x_discrete(labels = agg_labels) +
      { if (is.language(yl) || is.expression(yl)) ggplot2::ylab(yl) else ggplot2::ylab(yl %||% var) } +
      ggplot2::xlab(x_title) +
      { if (use_theme_bp) theme_CKMBP() else theme_CKM2() }
    
    if (facet == "Site") {
      base <- base + ggplot2::facet_wrap(~ Site, labeller = ggplot2::labeller(Site = c("C"="Conventional","O"="Organic")))
    } else if (facet == "Addition") {
      base <- base + ggplot2::facet_wrap(~ Addition)
    }
    base
  }
  
  `%||%` <- function(a,b) if (!is.null(a)) a else b
  plots <- list()
  
  if ("respiration" %in% families) {
    for (v in c("CO2","N2O","CH4")) {
      plots[[paste0(v,"_plot")]] <- make_boxplot(sample_data_4, v, facet="Addition", x_title="")
    }
  }
  
  if ("respiration2" %in% families) {
    for (v in c("CO2","N2O","CH4")) {
      plots[[paste0(v,"_plot_site")]] <- make_boxplot(sample_data_4, v, facet="Site", x_title="")
    }
  }
  
  
  if ("biomass" %in% families) {
    for (v in c("MBC","MBN","TOC","TN")) {
      plots[[paste0(v,"_plot")]] <- make_boxplot(sample_data_4, v, facet="Site", x_title="Field ID")
    }
  }
  
  if ("enzyme" %in% families && all(c("BG","LAP","NAG","PHOS") %in% names(sample_data_4))) {
    for (v in c("BG","CBH","LAP","NAG","PHOS")) {
      plots[[paste0(v,"_plot")]] <- make_boxplot(sample_data_4, v, facet="Site", x_title="Field ID")
    }
    PHOSL <- ggplot2::ggplot(sample_data_4, ggplot2::aes(x=as.factor(Aggregate), y=PHOS, color=Aggregate))+
      ggplot2::geom_boxplot(show.legend = TRUE, outlier.colour = NA, outlier.fill = NA, alpha = 0.2)+
      ggplot2::geom_point(position = ggplot2::position_dodge(width=0.8))+
      ggplot2::scale_color_manual(values=cbPalette6, labels = agg_labels)+theme_CKMBP()
    legend <- cowplot::get_legend(PHOSL)
    plots$gg_enzyme <- cowplot::plot_grid(plots$BG_plot, plots$CBH_plot, plots$LAP_plot,
                                          plots$NAG_plot, plots$PHOS_plot, legend,
                                          nrow=2, align="v", label_size=14, vjust=-0.5)
  }
  
  if ("FDA" %in% families)  plots$FDA_plot  <- make_boxplot(sample_data_4,"FDA",facet="Site",x_title="Field ID")
  if ("POXC" %in% families) plots$POXC_plot <- make_boxplot(sample_data_4,"POXC",facet="Site",x_title="")
  if ("soil" %in% families) {
    for (v in c("NO3","NH4","OlsenP","X_Ca","X_Mg","X_Na","pH","CEC","LOI","X_K")) {
      plots[[paste0(v,"_plot")]] <- make_boxplot(sample_data_4, v, facet="Site", x_title="Field ID")
    }
  }
  if ("CUE" %in% families && "CUE" %in% names(sample_data_4)) {
    sdf <- sample_data_4 %>% dplyr::filter(CUE >= 0, CUE <= 1)
    plots$CUE_plot <- make_boxplot(sdf, "CUE", facet="Site", x_title="")
  }
  # ---- NEW: MAOM / POM plots integrated via existing plot_MAOM() ----
  if ("MAOM" %in% families) {
    maom_plots <- plot_MAOM(sample_data_4)
    # names are fPOM_TC, oPOM_TC, MAOM_TC, etc.
    # They won't collide with existing names, so we can just append:
    plots <- c(plots, maom_plots)
  }
  return(plots)
}
