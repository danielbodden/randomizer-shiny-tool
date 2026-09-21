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

# Shared helper: 4-step workflow indicator for procedure pages
# Step 1 is shown as completed (dimmed) since user has already chosen a procedure
rand_steps_ui <- function() {
  step <- function(n, label, done = FALSE) {
    cls_step <- if (done) "rd-step rd-step-done" else "rd-step"
    div(class = cls_step,
        div(class = "rd-step-num", if (done) "\u2713" else n),
        div(class = "rd-step-text", label))
  }
  div(class = "rd-steps-strip rd-steps-strip-page",
    step("1", "Choose a procedure", done = TRUE),
    div(class = "rd-step-sep"),
    step("2", "Configure parameters"),
    div(class = "rd-step-sep"),
    step("3", "Generate sequence"),
    div(class = "rd-step-sep"),
    step("4", "Download report")
  )
}

# Shared helper: info-circle icon (inline SVG so it always renders, unlike a
# unicode glyph that depends on the client's installed fonts)
rd_info_icon <- function() {
  HTML('<svg width="14" height="14" viewBox="0 0 24 24" fill="#fff">
    <circle cx="12" cy="7" r="2"/>
    <rect x="10" y="11" width="4" height="10" rx="1.5"/>
  </svg>')
}

# ── Input validation helpers ────────────────────────────────────────────────
# Values that reach randomizeR unchecked either abort the app with a raw R
# error or are silently coerced (e.g. a non-integer ratio truncated, p > 1
# treated as p = 1), so every module validates its inputs first and reports
# the problems back to the user instead.

# set.seed() only accepts integers in the range of a 32-bit signed integer
RD_MAX_SEED <- .Machine$integer.max   # 2147483647

# Largest sample size that can be entered. Lists for more than a few hundred
# patients are rarely generated interactively, and the former limit of 10000
# only made the app slow; change this constant to raise the limit again.
RD_MAX_N <- 1000

# Largest number of treatment arms offered by the multi-arm procedures
RD_MAX_ARMS <- 6

# Largest sample size for which the exact imbalance assessment is offered.
# getAllSeq() enumerates every possible sequence, so runtime and memory grow
# exponentially in N (N = 24 already runs for several minutes).
RD_EXACT_MAX_N <- 16

# Is x a single, finite, whole number?
rd_is_whole <- function(x) {
  length(x) == 1 && !is.null(x) && !is.na(x) && is.finite(x) &&
    abs(x - round(x)) < .Machine$double.eps^0.5
}

# Returns NULL if x is a valid integer input, otherwise a message string
rd_check_int <- function(x, label, min = NULL, max = NULL) {
  if (is.null(x) || length(x) != 1 || is.na(x)) {
    return(sprintf("%s: please enter a value.", label))
  }
  if (!rd_is_whole(x)) {
    return(sprintf("%s must be a whole number (entered: %s).",
                   label, format(x, scientific = FALSE)))
  }
  if (!is.null(min) && x < min) {
    return(sprintf("%s must be at least %s (entered: %s).",
                   label, format(min, scientific = FALSE),
                   format(x, scientific = FALSE)))
  }
  if (!is.null(max) && x > max) {
    return(sprintf("%s must not exceed %s (entered: %s).",
                   label, format(max, scientific = FALSE),
                   format(x, scientific = FALSE)))
  }
  NULL
}

# Returns NULL if x lies in [min, max], otherwise a message string
rd_check_num <- function(x, label, min, max) {
  if (is.null(x) || length(x) != 1 || is.na(x) || !is.finite(x)) {
    return(sprintf("%s: please enter a value.", label))
  }
  if (x < min || x > max) {
    return(sprintf("%s must lie in [%s, %s] (entered: %s).",
                   label, min, max, format(x, scientific = FALSE)))
  }
  NULL
}

rd_check_seed <- function(x) {
  rd_check_int(x, "Seed", min = 0, max = RD_MAX_SEED)
}

# Validate the dynamic allocation-ratio inputs: crPar()/rarPar() require
# positive integers; non-integer entries were previously truncated silently.
rd_check_ratio <- function(input, prefix, k) {
  msgs <- character(0)
  for (i in seq_len(k)) {
    msg <- rd_check_int(input[[paste0(prefix, i)]],
                        sprintf("Factor of treatment %d", i), min = 1)
    if (!is.null(msg)) msgs <- c(msgs, msg)
  }
  msgs
}

# Shared helper: seed input. The admissible range (0 ... .Machine$integer.max)
# is not shown in the label; a seed outside it is reported as an input error.
rd_seed_input <- function(id) {
  numericInput(id, "Seed (for reproducibility):",
               value = sample.int(RD_MAX_SEED, 1), step = 1)
}

# Shared helper: sample size input, limited to RD_MAX_N
rd_n_input <- function(id, label, value) {
  numericInput(id, label, value = value, min = 2, max = RD_MAX_N, step = 1)
}

# Shared helper: input problems rendered as a coral error box
rd_error_ui <- function(msgs) {
  if (is.null(msgs) || length(msgs) == 0) return(NULL)
  div(class = "rd-bias-error",
      tags$strong("Please check the parameters:"),
      tags$ul(style = "margin:6px 0 0 0; padding-left:18px;",
              lapply(msgs, function(m) tags$li(m))))
}

# Shared helper: explanatory note in the main panel (green info box)
rd_note_ui <- function(...) {
  div(class = "rd-info-note",
      div(class = "rd-info-icon", rd_info_icon()),
      div(class = "rd-info-text", ...))
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

  # Simulation ("sim") vs exact ("Relative_Frequency" vs "Probability" weight column)
  d      <- res@D
  is_sim <- colnames(d)[2] == "Relative_Frequency"
  r      <- nrow(d)

  fmt <- function(x) formatC(x, digits = 4, format = "f")

  # Highlight mean in red if it exceeds alpha
  mean_color <- if (!is.na(vals["mean"]) && vals["mean"] > alpha) "#c9614f" else "#00774A"

  metric <- function(lbl, val, color = "#312f30") {
    div(class = "rd-bias-metric",
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

  if (is_sim) {
    # Each simulated sequence yields a binary reject/not-reject decision, so the
    # mean (empirical rejection rate) is the only informative moment; median, sd,
    # and quantiles of a 0/1 variable carry no extra information and are dropped.
    p_hat <- unname(vals["mean"])
    mc_se <- sqrt(p_hat * (1 - p_hat) / r)

    tagList(
      div(class = "rd-info-note",
          div(class = "rd-info-icon", rd_info_icon()),
          div(class = "rd-info-text",
              tags$strong(paste0("Simulation result (r = ", r, ")")),
              tags$br(),
              "Mean is a Monte Carlo estimate; its standard error is shown below.")),
      div(class = "rd-bias-metrics-row",
          metric("Mean P(reject H\u2080)",       p_hat, mean_color),
          metric("Monte Carlo Standard Error", mc_se)
      )
    )
  } else {
    tagList(
      div(class = "rd-bias-metrics-row",
          metric("Mean P(reject H\u2080)", vals["mean"], mean_color),
          metric("Median",                vals["x50"]),
          metric("Std. deviation",        vals["sd"])
      ),
      div(class = "rd-bias-quant-wrap",
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
          )
      )
    )
  }
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
  val_col <- names(d)[3]   # "imb", "absImb" or "maxImb"
  vals    <- d[[val_col]]
  wts     <- d[[wt_col]]

  mean_val <- sum(vals * wts)
  # For the maximum imbalance attained during the trial, a value of 0 is
  # impossible, so the best attainable value (1) is reported instead.
  p_bal    <- if (val_col == "maxImb") sum(wts[vals <= 1]) else sum(wts[vals == 0])
  p_lab    <- if (val_col == "maxImb") "P(max. imbalance <= 1)" else "P(perfect balance)"

  label <- switch(val_col,
    "imb"    = "Signed Imbalance",
    "absImb" = "Absolute Imbalance",
    "maxImb" = "Maximum Imbalance",
    val_col
  )
  x_label <- switch(val_col,
    "imb"    = "N\u1D07 \u2212 N\u1D04 at trial end",
    "absImb" = "|N\u1D07 \u2212 N\u1D04| at trial end",
    "maxImb" = "Maximum |N\u1D07 \u2212 N\u1D04| during the trial",
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
                        " | ", p_lab, " = ", round(p_bal * 100, 1), "%"),
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
  p_bal    <- if (val_col == "maxImb") sum(wts[vals <= 1]) else sum(wts[vals == 0])
  p_lab    <- if (val_col == "maxImb") "P(max. imbalance <= 1)" else "P(perfect balance)"

  # Simulation ("Relative_Frequency") vs exact ("Probability") weight column.
  # Under simulation, mean/median/quantiles are all estimated from only r draws,
  # so the median and quantile table are dropped in favor of the mean's Monte
  # Carlo standard error.
  is_sim <- wt_col == "Relative_Frequency"
  r      <- nrow(d)
  mc_se  <- if (is_sim) sv["sd"] / sqrt(r) else NA

  fmt  <- function(x) formatC(x, digits = 4, format = "f")
  fmtp <- function(x) paste0(round(x * 100, 1), "%")

  type_label <- switch(val_col,
    "imb"    = "Signed imbalance",
    "absImb" = "Absolute imbalance",
    "maxImb" = "Maximum imbalance",
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

  if (is_sim) {
    tagList(
      div(class = "rd-info-note",
          div(class = "rd-info-icon", rd_info_icon()),
          div(class = "rd-info-text",
              tags$strong(paste0("Simulation result (r = ", r, ")")),
              tags$br(),
              "Mean is a Monte Carlo estimate; its standard error is shown below.")),
      div(class = "rd-bias-metrics-row",
          metric(paste("Mean", type_label),      fmt(mean_val)),
          metric(p_lab,                           fmtp(p_bal), "#00774A"),
          metric("Monte Carlo Standard Error",    fmt(mc_se))
      )
    )
  } else {
    tagList(
      div(class = "rd-bias-metrics-row",
          metric(paste("Mean", type_label), fmt(mean_val)),
          metric(p_lab,                    fmtp(p_bal), "#00774A"),
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
}

# Shared helper: integer tick positions covering a range, thinned out so that
# the axis never gets crowded (used for the cumulative imbalance, which can
# only take integer values)
rd_integer_ticks <- function(rng, max_ticks = 10) {
  lo <- floor(min(rng)); hi <- ceiling(max(rng))
  by <- max(1, ceiling((hi - lo) / max_ticks))
  seq(lo, hi, by = by)
}

# Shared helper: randomization walk plot
# mti: optional maximum tolerated imbalance — drawn as boundaries at +/- mti
rand_walk_plot <- function(M, mti = NULL) {
  walk <- cumsum(c(0, 2 * M - 1))
  ylim <- range(c(walk, if (!is.null(mti)) c(-mti, mti)))

  par(
    bg  = "#f4f7f5",
    col.axis = "#312f30",
    col.lab  = "#312f30",
    col.main = "#312f30",
    family   = "sans",
    mar      = c(4, 4, 3, 2)
  )
  plot(
    0:length(M), walk,
    type = "l",
    lwd  = 2,
    col  = "#00774A",
    ylim = ylim,
    xlab = "Patient",
    ylab = "Cumulative imbalance (difference in group sizes)",
    main = "Randomization Walk",
    axes = FALSE,
    panel.first = {
      grid(col = "#d1d2d4", lty = 1, lwd = 0.5)
      abline(h = 0, lty = 2, col = "#f6a092", lwd = 1.5)
      if (!is.null(mti)) {
        abline(h = c(-mti, mti), lty = 3, col = "#c9614f", lwd = 1.5)
      }
    }
  )
  points(0:length(M), walk, pch = 19, col = "#00774A", cex = 0.5)
  axis(1, col = "#d1d2d4", col.ticks = "#d1d2d4")
  # the cumulative imbalance is a count difference, so only integer ticks
  axis(2, at = rd_integer_ticks(ylim), col = "#d1d2d4",
       col.ticks = "#d1d2d4", las = 1)
  box(col = "#d1d2d4")
  if (!is.null(mti)) {
    mtext(paste0("dotted lines: ± MTI = ", mti), side = 3, line = 0.2,
          adj = 1, cex = 0.8, col = "#5a5756")
  }
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
