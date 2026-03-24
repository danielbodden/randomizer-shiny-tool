library(shiny)
library(bslib)
library(randomizeR)
library(rmarkdown)
library(ggplot2)
library(scales)

# Load all modules
for (f in list.files("modules", full.names = TRUE, pattern = "\\.R$")) {
  source(f)
}

# Shared helper: bias summary as styled metric cards
bias_summary_ui <- function(res, alpha = 0.05) {
  if (inherits(res, "error")) {
    return(div(class = "rd-bias-error",
               tags$strong("Error: "), conditionMessage(res)))
  }

  sm      <- summary(res)
  col_nm  <- colnames(sm)[1]
  vals    <- sm[, 1]
  label   <- sub("P\\(rej\\)\\((.+)\\)", "\\1", col_nm)
  label   <- sub("testDec\\((.+)\\)", "\\1", label)

  # Detect simulation: testDec column contains binary 0/1 values
  d      <- res@D
  is_sim <- length(unique(d[[3]])) <= 2

  fmt <- function(x) formatC(x, digits = 4, format = "f")

  # Highlight mean in red if it exceeds alpha
  mean_color <- if (!is.na(vals["mean"]) && vals["mean"] > alpha) "#c9614f" else "#00774A"

  metric <- function(lbl, val, color = "#312f30", dimmed = FALSE) {
    div(class = if (dimmed) "rd-bias-metric rd-bias-metric-dim" else "rd-bias-metric",
        div(class = "rd-bias-metric-val", style = paste0("color:", color), fmt(val)),
        div(class = "rd-bias-metric-lbl", lbl))
  }

  quant_row <- function(lbl, key) {
    v <- vals[key]
    tags$tr(
      tags$td(lbl),
      tags$td(class = "rd-bias-td-val", fmt(v))
    )
  }

  tagList(
    # Simulation warning
    if (is_sim) {
      div(class = "rd-bias-sim-note",
          div(class = "rd-bias-sim-icon", "\u26a0"),
          div(class = "rd-bias-sim-text",
              tags$strong("Simulation result"),
              tags$br(),
              "Each sequence yields a binary reject / not-reject decision.
              Only the \u2014 mean \u2014 equals the empirical rejection rate and is
              interpretable. Median, standard deviation, and quantiles
              reflect the binary (0/1) distribution and are shown for
              completeness only."))
    },
    # Primary metrics
    div(class = "rd-bias-metrics-row",
        metric("Mean P(reject H\u2080)", vals["mean"], mean_color),
        metric("Median",                vals["x50"], dimmed = is_sim),
        metric("Std. deviation",        vals["sd"],  dimmed = is_sim)
    ),
    # Quantile table
    div(class = if (is_sim) "rd-bias-quant-wrap rd-bias-quant-dim" else "rd-bias-quant-wrap",
        tags$table(class = "rd-bias-table",
          tags$thead(tags$tr(
            tags$th("Quantile"), tags$th("P(reject H\u2080)")
          )),
          tags$tbody(
            quant_row("Minimum",  "min"),
            quant_row("5th pct",  "x05"),
            quant_row("25th pct", "x25"),
            quant_row("75th pct", "x75"),
            quant_row("95th pct", "x95"),
            quant_row("Maximum",  "max")
          )
        ),
        div(class = "rd-bias-alpha-note",
            tags$span(class = "rd-bias-alpha-dot",
                      style = if (vals["mean"] > alpha) "background:#c9614f" else "background:#00774A"),
            if (vals["mean"] > alpha)
              paste0("Mean (", fmt(vals["mean"]), ") exceeds \u03b1 = ", alpha,
                     " \u2014 inflated type I error")
            else
              paste0("Mean (", fmt(vals["mean"]), ") within \u03b1 = ", alpha)
        )
    )
  )
}

# Shared helper: bias assessment plot
bias_assessment_plot <- function(res, alpha = 0.05) {
  library(ggplot2)
  d        <- res@D
  wt_col   <- names(d)[2]   # "Probability" or "Relative_Frequency"
  val_col  <- names(d)[3]   # "P(rej)(...)" or "testDec(...)"
  vals     <- d[[val_col]]
  wts      <- d[[wt_col]]
  is_sim   <- length(unique(vals)) <= 2   # binary testDec

  mean_rej <- sum(vals * wts)
  label    <- sub("P\\(rej\\)\\((.+)\\)", "\\1", val_col)
  label    <- sub("testDec\\((.+)\\)", "\\1", label)

  base_theme <- theme_minimal(base_family = "sans", base_size = 12) +
    theme(
      plot.background  = element_rect(fill = "#f5f7f5", color = NA),
      panel.background = element_rect(fill = "#f5f7f5", color = NA),
      panel.grid.major = element_line(color = "#d1d2d4", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", color = "#312f30", size = 13),
      plot.subtitle    = element_text(color = "#5a5756", size = 10),
      axis.text        = element_text(color = "#5a5756"),
      axis.title       = element_text(color = "#5a5756")
    )

  if (is_sim) {
    rej_rate <- mean_rej
    df_bar <- data.frame(
      status = c("Rejected", "Not rejected"),
      prop   = c(rej_rate, 1 - rej_rate)
    )
    df_bar$status <- factor(df_bar$status, levels = c("Not rejected", "Rejected"))
    ggplot(df_bar, aes(x = status, y = prop, fill = status)) +
      geom_col(width = 0.45, show.legend = FALSE) +
      geom_hline(yintercept = alpha, linetype = "dashed",
                 color = "#f6a092", linewidth = 0.9) +
      scale_fill_manual(values = c("Rejected" = "#00774A", "Not rejected" = "#d1d2d4")) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
      labs(title   = paste("Empirical rejection rate \u2014", label),
           subtitle = paste0("Rejection rate: ", round(rej_rate * 100, 1),
                             "% | \u03b1 = ", alpha * 100, "%"),
           x = NULL, y = "Proportion") +
      base_theme
  } else {
    df_hist <- data.frame(val = vals, wt = wts)
    ggplot(df_hist, aes(x = val, weight = wt)) +
      geom_histogram(bins = 30, fill = "#00774A", alpha = 0.75,
                     color = "white", linewidth = 0.3) +
      geom_vline(xintercept = mean_rej, color = "#00774A",
                 linetype = "solid", linewidth = 1) +
      geom_vline(xintercept = alpha, color = "#f6a092",
                 linetype = "dashed", linewidth = 0.9) +
      annotate("text", x = mean_rej, y = Inf, vjust = 2, hjust = -0.15,
               label = paste("Mean:", round(mean_rej, 4)),
               color = "#00774A", size = 3.5, fontface = "bold") +
      annotate("text", x = alpha, y = Inf, vjust = 2, hjust = 1.1,
               label = paste0("\u03b1 = ", alpha),
               color = "#f6a092", size = 3.5) +
      labs(title    = paste("Rejection probability distribution \u2014", label),
           subtitle = paste0("Mean P(reject H\u2080) = ", round(mean_rej, 4)),
           x = "P(reject H\u2080) per sequence", y = "Weighted frequency") +
      base_theme
  }
}

# Shared helper: imbalance assessment plot
imbal_assessment_plot <- function(res) {
  library(ggplot2)
  if (inherits(res, "error")) return(NULL)

  d       <- res@D
  wt_col  <- names(d)[2]   # "Probability" or "Relative_Frequency"
  val_col <- names(d)[3]   # "imb", "absImb", or "loss"
  vals    <- d[[val_col]]
  wts     <- d[[wt_col]]

  mean_val <- sum(vals * wts)
  p_bal    <- sum(wts[vals == 0])

  label <- switch(val_col,
    "imb"    = "Signed Imbalance",
    "absImb" = "Absolute Imbalance",
    "loss"   = "Loss",
    val_col
  )
  x_label <- switch(val_col,
    "imb"    = "N\u1D07 \u2212 N\u1D04 at trial end",
    "absImb" = "|N\u1D07 \u2212 N\u1D04| at trial end",
    "loss"   = "Loss at trial end",
    val_col
  )

  base_theme <- theme_minimal(base_family = "sans", base_size = 12) +
    theme(
      plot.background  = element_rect(fill = "#f5f7f5", color = NA),
      panel.background = element_rect(fill = "#f5f7f5", color = NA),
      panel.grid.major = element_line(color = "#d1d2d4", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", color = "#312f30", size = 13),
      plot.subtitle    = element_text(color = "#5a5756", size = 10),
      axis.text        = element_text(color = "#5a5756"),
      axis.title       = element_text(color = "#5a5756")
    )

  # Discrete bar chart — aggregate by unique value
  df <- aggregate(wts, by = list(val = vals), FUN = sum)
  names(df) <- c("val", "prob")

  ggplot(df, aes(x = factor(val), y = prob)) +
    geom_col(fill = "#00774A", alpha = 0.80, width = 0.55) +
    geom_col(data = df[df$val == 0, ],
             aes(x = factor(val), y = prob),
             fill = "#005c38", alpha = 1, width = 0.55) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(
      title    = paste("Imbalance distribution \u2014", label),
      subtitle = paste0("Mean: ", round(mean_val, 3),
                        " | P(perfect balance) = ", round(p_bal * 100, 1), "%"),
      x = x_label,
      y = "Probability"
    ) +
    base_theme
}

# Shared helper: imbalance summary UI
imbal_summary_ui <- function(res) {
  if (inherits(res, "error")) {
    return(div(class = "rd-bias-error",
               tags$strong("Error: "), conditionMessage(res)))
  }

  d       <- res@D
  wt_col  <- names(d)[2]
  val_col <- names(d)[3]
  vals    <- d[[val_col]]
  wts     <- d[[wt_col]]

  sm       <- summary(res)
  sv       <- sm[, 1]
  mean_val <- sv["mean"]
  p_bal    <- sum(wts[vals == 0])

  fmt  <- function(x) formatC(x, digits = 4, format = "f")
  fmtp <- function(x) paste0(round(x * 100, 1), "%")

  type_label <- switch(val_col,
    "imb"    = "Signed imbalance",
    "absImb" = "Absolute imbalance",
    "loss"   = "Loss",
    val_col
  )

  metric <- function(lbl, val, color = "#312f30") {
    div(class = "rd-bias-metric",
        div(class = "rd-bias-metric-val", style = paste0("color:", color), val),
        div(class = "rd-bias-metric-lbl", lbl))
  }

  quant_row <- function(lbl, key) {
    tags$tr(tags$td(lbl), tags$td(class = "rd-bias-td-val", fmt(sv[key])))
  }

  tagList(
    div(class = "rd-bias-metrics-row",
        metric(paste("Mean", type_label), fmt(mean_val)),
        metric("P(perfect balance)",      fmtp(p_bal), "#00774A"),
        metric("Std. deviation",          fmt(sv["sd"]))
    ),
    div(class = "rd-bias-quant-wrap",
        tags$table(class = "rd-bias-table",
          tags$thead(tags$tr(tags$th("Quantile"), tags$th(type_label))),
          tags$tbody(
            quant_row("Minimum",  "min"),
            quant_row("5th pct",  "x05"),
            quant_row("25th pct", "x25"),
            quant_row("75th pct", "x75"),
            quant_row("95th pct", "x95"),
            quant_row("Maximum",  "max")
          )
        )
    )
  )
}

# Shared helper: randomization walk plot — RealiseD brand colors
rand_walk_plot <- function(M) {
  par(
    bg  = "#f4f7f5",
    col.axis = "#312f30",
    col.lab  = "#312f30",
    col.main = "#312f30",
    family   = "sans",
    mar      = c(4, 4, 3, 2)
  )
  plot(
    0:length(M), cumsum(c(0, 2 * M - 1)),
    type = "l",
    lwd  = 2,
    col  = "#00774A",
    xlab = "Patient",
    ylab = "Cumulative imbalance",
    main = "Randomization Walk",
    axes = FALSE,
    panel.first = {
      grid(col = "#d1d2d4", lty = 1, lwd = 0.5)
      abline(h = 0, lty = 2, col = "#f6a092", lwd = 1.5)
    }
  )
  points(0:length(M), cumsum(c(0, 2 * M - 1)),
         pch = 19, col = "#00774A", cex = 0.5)
  axis(1, col = "#d1d2d4", col.ticks = "#d1d2d4")
  axis(2, col = "#d1d2d4", col.ticks = "#d1d2d4", las = 1)
  box(col = "#d1d2d4")
}

# Shared helper: collect treatment group names from dynamic inputs
get_groups <- function(input, prefix, k) {
  sapply(seq_len(k), function(i) {
    val <- input[[paste0(prefix, i)]]
    if (is.null(val) || !nzchar(val)) LETTERS[i] else val
  })
}

# Shared helper: collect allocation ratios from dynamic inputs
get_ratio <- function(input, prefix, k) {
  sapply(seq_len(k), function(i) {
    val <- input[[paste0(prefix, i)]]
    if (is.null(val) || is.na(val)) 1L else as.integer(val)
  })
}
