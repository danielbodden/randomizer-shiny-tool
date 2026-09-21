# =============================================================================
# Module: Imbalance Assessment
#
# Workflow:
#   exact: getAllSeq(par) -> assess(seqs, imbal(...))
#   sim:   genSeq(par, r, seed) -> assess(seqs, imbal(...))
# =============================================================================

mod_imbal_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 320,

      h5("Randomization Procedure"),
      selectInput(ns("procedure"), NULL,
                  choices = c(
                    "Big Stick Design"          = "bsd",
                    "Complete Randomization"    = "cr",
                    "Efron's Biased Coin"       = "ebc",
                    "Permuted Block"            = "pbr",
                    "Random Permuted Block"     = "rpbr",
                    "Random Allocation Rule"    = "rar",
                    "Chen's Design"             = "chen"
                  ), selectize = FALSE),

      uiOutput(ns("proc_params")),

      hr(),
      h5("Imbalance Measure"),

      # The available measures depend on the procedure, see below
      uiOutput(ns("type_ui")),

      hr(),
      h5("Computation"),

      uiOutput(ns("method_ui")),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'sim' || input['%s'] == 'rpbr'",
                            ns("method"), ns("procedure")),
        numericInput(ns("r"), "Number of simulated sequences:",
                     value = 1000, min = 100, max = 10000, step = 100)
      ),

      # The exact method enumerates all sequences and uses their probabilities;
      # no random numbers are drawn, so a seed would be meaningless there.
      conditionalPanel(
        condition = sprintf("input['%s'] != 'exact'", ns("method")),
        rd_seed_input(ns("seed"))
      ),

      actionButton(ns("run"), "\u25b6  Assess", class = "btn-primary w-100 mt-2")
    ),

    div(
      class = "rd-main-inner",

      div(class = "rd-pattern-strip"),

      h2(class = "rd-method-title", "Imbalance Assessment"),

      # the subscripts are written as inline HTML: separate tags would be
      # pretty-printed on their own line and render as "n E" instead of "nE"
      p(class = "rd-description",
        HTML("Computes the distribution of the chosen imbalance measure across
              all (exact) or many (simulated) allocations, with
              n<sub>E</sub> and n<sub>C</sub> denoting the group sizes of the
              two treatments.")),

      tags$ul(class = "rd-description",
        tags$li(tags$strong("Signed imbalance"),
                HTML(" I = n<sub>E</sub> &minus; n<sub>C</sub> at the end of the
                      trial. It gives both the magnitude and the direction of
                      the imbalance.")),
        tags$li(tags$strong("Absolute imbalance"),
                HTML(" |I| = |n<sub>E</sub> &minus; n<sub>C</sub>| at the end of
                      the trial. It ignores which treatment has more
                      participants.")),
        tags$li(tags$strong("Maximum imbalance"),
                " the largest absolute imbalance attained at any point during
                the trial, not only at its end. It is informative for procedures
                that are balanced at the end but may drift in between.")),

      div(class = "rd-reference-block",
        p(class = "rd-reference",
          "Lachin JM (1988). Statistical properties of randomization in clinical trials.",
          tags$em("Controlled Clinical Trials,"), " 9(4), 289\u2013311."),
        p(class = "rd-reference",
          HTML("Uschner D, Schindler D, Hilgers RD, Heussen N (2018). randomizeR: An R
          package for the assessment and implementation of randomization in clinical trials.
          <em>Journal of Statistical Software,</em> 85(8), 1\u201333.
          doi:10.18637/jss.v085.i08"))
      ),

      uiOutput(ns("error_ui")),
      uiOutput(ns("results_ui"))
    )
  )
}


mod_imbal_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    result_obj <- reactiveVal(NULL)
    r_code_str <- reactiveVal(NULL)
    errors     <- reactiveVal(NULL)
    notes      <- reactiveVal(NULL)

    # ── Measure selector ──────────────────────────────────────────────────────
    # The Random Allocation Rule and Permuted Block Randomization are balanced
    # at the end of the trial by construction (PBR is assessed with complete
    # blocks only), so the imbalance at the end is always 0 there and only the
    # maximum imbalance attained during the trial remains informative. Random
    # Permuted Block keeps all measures, because the last block may be
    # incomplete.
    # isTRUE() keeps this FALSE while input$procedure is still NULL, i.e. before
    # the sidebar inputs have been flushed to the server for the first time
    terminal_balance <- reactive(isTRUE(input$procedure %in% c("rar", "pbr")))

    output$type_ui <- renderUI({
      tb      <- terminal_balance()
      choices <- c(
        "Absolute imbalance" = "absImb",
        "Signed imbalance"   = "imb",
        "Maximum imbalance"  = "maxImb"
      )
      if (tb) {
        choices <- choices[choices %in% "maxImb"]
      }
      current  <- isolate(input$type)
      selected <- if (!is.null(current) && current %in% choices) {
        current
      } else if (tb) "maxImb" else "absImb"

      tagList(
        selectInput(session$ns("type"), "Measure type:", choices = choices,
                    selected = selected, selectize = FALSE),
        if (tb)
          div(class = "rd-sidebar-note",
              tags$small("This procedure is balanced at the end of the trial by
                          construction, so the absolute and the signed imbalance
                          are 0 and are not offered here. The maximum imbalance
                          attained during the trial remains informative."))
      )
    })

    # ── Method selector (hide exact for RPBR) ─────────────────────────────────
    output$method_ui <- renderUI({
      if (input$procedure == "rpbr") {
        div(
          tags$small(class = "text-muted",
                     "Random Permuted Block always uses simulation."),
          tags$input(type = "hidden", id = session$ns("method"), value = "sim")
        )
      } else {
        selectInput(session$ns("method"), "Method:",
                    choices  = c("Simulation" = "sim", "Exact" = "exact"),
                    selected = "sim", selectize = FALSE)
      }
    })

    # ── Dynamic procedure parameters ──────────────────────────────────────────
    output$proc_params <- renderUI({
      switch(input$procedure,
        "bsd" = tagList(
          numericInput(session$ns("n"),   "Sample size:", value = 10, min = 2, max = RD_MAX_N),
          numericInput(session$ns("mti"), "MTI:",         value = 3,  min = 1)
        ),
        "cr" = tagList(
          numericInput(session$ns("n"), "Sample size:", value = 10, min = 2, max = RD_MAX_N)
        ),
        "ebc" = tagList(
          numericInput(session$ns("n"), "Sample size:", value = 10, min = 2, max = RD_MAX_N),
          numericInput(session$ns("p"), "Biasing probability (p):",
                       value = 0.75, min = 0.5, max = 1, step = 0.05)
        ),
        "pbr" = tagList(
          numericInput(session$ns("n"),          "Target sample size:", value = 12, min = 2, max = RD_MAX_N),
          numericInput(session$ns("block_size"), "Block size:",         value = 4,  min = 2, step = 2)
        ),
        "rpbr" = tagList(
          numericInput(session$ns("n"),  "Sample size:", value = 20, min = 2, max = RD_MAX_N),
          textInput(session$ns("rb"), "Block sizes (comma-separated):", value = "2, 4, 6")
        ),
        "rar" = tagList(
          numericInput(session$ns("n"), "Sample size:", value = 10, min = 2, max = RD_MAX_N)
        ),
        "chen" = tagList(
          numericInput(session$ns("n"),   "Sample size:", value = 10, min = 2, max = RD_MAX_N),
          numericInput(session$ns("mti"), "MTI:",         value = 3,  min = 1),
          numericInput(session$ns("p"),   "Biasing probability (p):",
                       value = 0.75, min = 0.5, max = 1, step = 0.05)
        )
      )
    })

    # ── Build randomization parameter object ──────────────────────────────────
    # Returns the parameter object, the sample size that is actually assessed,
    # the R code that reproduces it, and notes about any adjustment made.
    make_par <- function(proc, n) {
      info     <- character(0)
      par_code <- NULL

      par <- switch(proc,
        "bsd" = {
          mti <- as.integer(input$mti)
          par_code <- sprintf("par <- bsdPar(N = %d, mti = %d)", n, mti)
          bsdPar(N = n, mti = mti)
        },
        "cr" = {
          par_code <- sprintf("par <- crPar(N = %d)", n)
          crPar(N = n)
        },
        "ebc" = {
          par_code <- sprintf("par <- ebcPar(N = %d, p = %s)", n, format(input$p))
          ebcPar(N = n, p = input$p)
        },
        "pbr" = {
          bs <- as.integer(input$block_size)
          if (bs %% 2 != 0) {
            bs   <- bs + 1L
            info <- c(info, sprintf("The block size must be an even number; the
                                     next even number, b = %d, was used.", bs))
          }
          nb <- ceiling(n / bs)
          if (nb * bs != n) {
            info <- c(info, sprintf("Permuted Block Randomization is assessed for
                                     complete blocks only: the imbalance refers to
                                     %d = %d × %d patients rather than to the
                                     sample size N = %d entered.",
                                    nb * bs, nb, bs, n))
            n <- nb * bs
          }
          par_code <- sprintf("par <- pbrPar(bc = rep(%d, %d))", bs, nb)
          pbrPar(bc = rep(bs, nb))
        },
        "rpbr" = {
          raw <- strsplit(trimws(input$rb), "[,;[:space:]]+")[[1]]
          raw <- raw[nzchar(raw)]
          num <- suppressWarnings(as.numeric(raw))
          ok  <- !is.na(num) & vapply(num, rd_is_whole, logical(1)) &
                 num >= 2 & num %% 2 == 0
          rb  <- sort(unique(as.integer(num[ok])))
          if (any(!ok)) {
            info <- c(info, sprintf("Ignored block sizes (only even whole numbers
                                     ≥ 2 are allowed): %s.",
                                    paste(raw[!ok], collapse = ", ")))
          }
          par_code <- sprintf("par <- rpbrPar(N = %d, rb = c(%s))",
                              n, paste(rb, collapse = ", "))
          rpbrPar(N = n, rb = rb)
        },
        "rar" = {
          # rarPar() aborts when N is not a multiple of sum(ratio) = 2
          if (n %% 2 != 0) {
            info <- c(info, sprintf("The Random Allocation Rule requires N to be a
                                     multiple of the sum of the allocation ratios
                                     (here 2). The imbalance was assessed for
                                     N = %d instead of %d.", n + 1L, n))
            n <- n + 1L
          }
          par_code <- sprintf("par <- rarPar(N = %d)", n)
          rarPar(N = n)
        },
        "chen" = {
          mti <- as.integer(input$mti)
          par_code <- sprintf("par <- chenPar(N = %d, mti = %d, p = %s)",
                              n, mti, format(input$p))
          chenPar(N = n, mti = mti, p = input$p)
        }
      )

      list(par = par, n = n, code = par_code, info = gsub("\\s+", " ", info))
    }

    # ── Run assessment ─────────────────────────────────────────────────────────
    observeEvent(input$run, {
      result_obj(NULL)
      r_code_str(NULL)
      notes(NULL)

      proc   <- input$procedure
      method <- if (proc == "rpbr") "sim" else isolate(input$method)
      req(input$type)

      # ── Validate inputs ────────────────────────────────────────────────────
      # A sample size below 2 is corrected to N = 2 instead of being rejected
      n_in     <- input$n
      n_raised <- FALSE
      if (rd_is_whole(n_in) && n_in < 2) {
        n_in     <- 2L
        n_raised <- TRUE
        updateNumericInput(session, "n", value = 2)
      }
      msgs <- rd_check_int(n_in, "Sample size", min = 2, max = RD_MAX_N)
      if (proc %in% c("bsd", "chen")) {
        msgs <- c(msgs, rd_check_int(input$mti,
                                     "Maximum tolerated imbalance (MTI)", min = 1))
      }
      if (proc %in% c("ebc", "chen")) {
        msgs <- c(msgs, rd_check_num(input$p, "Biasing probability (p)",
                                     min = 0.5, max = 1))
      }
      if (proc == "pbr") {
        msgs <- c(msgs, rd_check_int(input$block_size, "Block size",
                                     min = 2, max = RD_MAX_N))
      }
      if (proc == "rpbr") {
        num <- suppressWarnings(as.numeric(
          strsplit(trimws(input$rb), "[,;[:space:]]+")[[1]]))
        num <- num[!is.na(num)]
        if (!any(vapply(num, rd_is_whole, logical(1)) & num >= 2 & num %% 2 == 0)) {
          msgs <- c(msgs, "Block sizes: please enter at least one even whole
                           number ≥ 2 (e.g. 2, 4, 6).")
        }
      }
      if (method == "exact") {
        # getAllSeq() enumerates every possible sequence, so runtime and memory
        # grow exponentially in N (N = 24 already takes several minutes).
        if (rd_is_whole(n_in) && n_in > RD_EXACT_MAX_N) {
          msgs <- c(msgs, sprintf("The exact method enumerates all possible
                                   sequences and is therefore restricted to
                                   N ≤ %d in this app. Please reduce the
                                   sample size or use the simulation method.",
                                  RD_EXACT_MAX_N))
        }
      } else {
        msgs <- c(msgs,
                  rd_check_int(input$r, "Number of simulated sequences",
                               min = 100, max = 100000),
                  rd_check_seed(input$seed))
      }
      if (length(msgs) > 0) {
        errors(gsub("\\s+", " ", msgs))
        return()
      }
      errors(NULL)

      # ── Build parameters ───────────────────────────────────────────────────
      built <- tryCatch(make_par(proc, as.integer(n_in)),
                        error = function(e) e)
      if (inherits(built, "error")) {
        errors(conditionMessage(built))
        return()
      }
      info <- built$info
      if (n_raised) {
        info <- c(info, "The sample size must be at least 2; N = 2 was used.")
      }

      imb_obj <- imbal(type = input$type)

      seqs <- tryCatch({
        if (method == "exact") {
          getAllSeq(built$par)
        } else {
          genSeq(built$par, r = as.integer(input$r),
                 seed = as.integer(input$seed))
        }
      }, error = function(e) e)

      if (inherits(seqs, "error")) {
        errors(conditionMessage(seqs))
        return()
      }

      res <- tryCatch(assess(seqs, imb_obj), error = function(e) e)
      if (inherits(res, "error")) {
        errors(conditionMessage(res))
        return()
      }
      notes(gsub("\\s+", " ", info))
      result_obj(res)

      # ── Build R code string ──────────────────────────────────────────────────
      seq_code <- if (method == "exact") {
        "seqs <- getAllSeq(par)"
      } else {
        sprintf("seqs <- genSeq(par, r = %d, seed = %d)",
                as.integer(isolate(input$r)), as.integer(isolate(input$seed)))
      }
      r_code_str(paste(
        "library(randomizeR)",
        "",
        built$code,
        seq_code,
        sprintf('imb  <- imbal(type = "%s")', isolate(input$type)),
        "res  <- assess(seqs, imb)",
        "summary(res)",
        sep = "\n"
      ))
    })

    output$error_ui <- renderUI({ rd_error_ui(errors()) })


    # ── UI ────────────────────────────────────────────────────────────────────
    output$results_ui <- renderUI({
      req(result_obj())
      tagList(
        div(class = "rd-section-label", tags$span("Imbalance Distribution")),
        if (length(notes()) > 0)
          rd_note_ui(tags$strong("Note"), tags$br(),
                     tags$ul(style = "margin:4px 0 0 0; padding-left:18px;",
                             lapply(notes(), function(m) tags$li(m)))),
        div(class = "rd-plot-wrap",
            plotOutput(session$ns("imbal_plot"), height = "300px")),
        div(class = "rd-section-label", tags$span("Summary Statistics")),
        rd_note_ui("Mean, standard deviation and quantiles are weighted values:
                    for the exact method the weights are the probabilities of the
                    individual sequences, for the simulation their relative
                    frequencies."),
        uiOutput(session$ns("imbal_summary")),
        div(class = "rd-section-label", tags$span("R Code")),
        div(class = "rd-code-wrap",
            verbatimTextOutput(session$ns("r_code")))
      )
    })

    output$imbal_plot <- renderPlot({
      req(result_obj())
      if (inherits(result_obj(), "error")) return(NULL)
      print(imbal_assessment_plot(result_obj()))
    })

    output$imbal_summary <- renderUI({
      req(result_obj())
      imbal_summary_ui(result_obj())
    })

    output$r_code <- renderText({
      req(r_code_str())
      r_code_str()
    })
  })
}
