# =============================================================================
# Module: Random Permuted Block Randomization (RPBR)
#
# rpbrPar(N, rb, K, ratio, groups, filledBlock) — supports 2–6 arms and
# unequal allocation
# rb: vector of possible block sizes (one chosen at random per block)
# genSeq(par, seed = seed)
# Walk plot is only shown for 2-arm designs.
# =============================================================================

mod_rpbr_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      h5("Parameters"),

      rd_n_input(ns("n"), "Total sample size:", 20),

      textInput(ns("rb"), "Possible block sizes (comma-separated):",
                value = "2, 4, 6"),

      checkboxInput(ns("filled_block"), "Complete blocks only (filledBlock)",
                    value = FALSE),

      selectInput(ns("k"), "Number of treatment arms:",
                  choices = as.character(2:RD_MAX_ARMS), selected = "2",
                  selectize = FALSE),

      # Dynamic ratio and name inputs (rendered server-side)
      uiOutput(ns("ratio_inputs")),
      uiOutput(ns("name_inputs")),

      hr(),
      rd_seed_input(ns("seed")),

      actionButton(ns("generate"), "▶  Generate", class = "btn-primary w-100 mt-2"),

      div(class = "rd-sidebar-note",
          tags$small("Possible block sizes must be positive whole numbers and
                      multiples of the sum of the allocation ratios (even numbers
                      for a 1:1 two-arm design); other entries are ignored.
                      “Complete blocks only” (filledBlock = TRUE) requires
                      a block constellation whose lengths sum to exactly N; if
                      it is unticked, block lengths are drawn until N is reached
                      and the last block may remain incomplete."))
    ),

    # ── Main panel ────────────────────────────────────────────────────────
    div(
      class = "rd-main-inner",

      div(class = "rd-pattern-strip"),
      rand_steps_ui(),

      h2(class = "rd-method-title", "Random Permuted Block Randomization"),
      span(class = "rd-method-badge badge-green", "2 – 6 arms"),

      p(class = "rd-description",
        "Random Permuted Block Randomization applies the permuted block approach
        with block sizes chosen at random from a specified set for each block,
        retaining approximate balance. With “complete blocks only”
        (filledBlock = TRUE) the randomly drawn block lengths must add up exactly
        to N, so that the list contains complete blocks only; otherwise the last
        block is cut at N and may be incomplete."),

      p(class = "rd-reference",
        "Matts JP, Lachin JM (1988). Properties of permuted-block randomization in
        clinical trials. Controlled Clinical Trials, 9(4), 327–344.
        doi:10.1016/0197-2456(88)90049-2"),

      uiOutput(ns("error_ui")),
      uiOutput(ns("results_ui"))
    )
  )
}


mod_rpbr_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    seq_obj <- reactiveVal(NULL)
    errors  <- reactiveVal(NULL)
    notes   <- reactiveVal(NULL)

    # ── Dynamic sidebar inputs ─────────────────────────────────────────────
    output$ratio_inputs <- renderUI({
      k <- as.integer(req(input$k))
      lapply(seq_len(k), function(i) {
        numericInput(
          session$ns(paste0("ratio", i)),
          paste0("Factor of treatment ", i, ":"),
          value = 1, min = 1, step = 1
        )
      })
    })

    output$name_inputs <- renderUI({
      k <- as.integer(req(input$k))
      lapply(seq_len(k), function(i) {
        textInput(
          session$ns(paste0("name", i)),
          paste0("Name of treatment ", i, ":"),
          value = LETTERS[i]
        )
      })
    })

    # ── Generate sequence ──────────────────────────────────────────────────
    observeEvent(input$generate, {
      # the arm selector is always present, but the dependent ratio and
      # name inputs are rendered server-side
      req(input$k)
      k <- as.integer(input$k)

      msgs <- c(
        rd_check_int(input$n, "Total sample size", min = 2, max = RD_MAX_N),
        rd_check_ratio(input, "ratio", k),
        rd_check_seed(input$seed)
      )
      if (length(msgs) > 0) {
        errors(gsub("\\s+", " ", msgs)); seq_obj(NULL); notes(NULL); return()
      }

      ratio     <- get_ratio(input, "ratio", k)
      groups    <- get_groups(input, "name", k)
      ratio_sum <- sum(ratio)

      # Parse the block sizes and report which entries were discarded instead
      # of dropping them silently. Each block must contain the treatments in the
      # given ratio, so a block length has to be a multiple of sum(ratio).
      raw <- strsplit(trimws(input$rb), "[,;[:space:]]+")[[1]]
      raw <- raw[nzchar(raw)]
      num <- suppressWarnings(as.numeric(raw))
      ok  <- !is.na(num) & vapply(num, rd_is_whole, logical(1)) &
             num >= ratio_sum & num %% ratio_sum == 0
      rb  <- as.integer(num[ok])

      if (length(rb) == 0) {
        msg <- sprintf("Possible block sizes: please enter at least one whole
                        number that is a multiple of the sum of the allocation
                        ratios (%d), e.g. %d or %d.",
                       ratio_sum, ratio_sum, 2 * ratio_sum)
        errors(gsub("\\s+", " ", msg)); seq_obj(NULL); notes(NULL); return()
      }
      errors(NULL)

      info <- character(0)
      if (any(!ok)) {
        info <- c(info, sprintf("Ignored entries (only whole multiples of
                                 sum(ratio) = %d are allowed): %s.",
                                ratio_sum, paste(raw[!ok], collapse = ", ")))
      }

      n      <- as.integer(input$n)
      rb     <- sort(unique(rb))
      filled <- isTRUE(input$filled_block)

      # With filledBlock = TRUE randomizeR draws block lengths one at a time and
      # aborts with "No block constellation possible with filled blocks" when the
      # drawn lengths cannot be completed to exactly N (e.g. N = 20, rb = 4, 6).
      res <- tryCatch({
        par <- rpbrPar(N = n, rb = rb, K = k, ratio = ratio, groups = groups,
                       filledBlock = filled)
        genSeq(par, seed = as.integer(input$seed))
      }, error = function(e) e)

      if (inherits(res, "error")) {
        msg <- conditionMessage(res)
        if (grepl("block constellation", msg, ignore.case = TRUE)) {
          msg <- sprintf("With the seed %d, block lengths were drawn from %s that
                          cannot be completed to exactly N = %d with complete
                          blocks, so no list could be generated. Try a different
                          seed or sample size, or untick “complete blocks
                          only” to allow an incomplete last block.",
                         as.integer(input$seed), paste(rb, collapse = ", "), n)
        }
        errors(gsub("\\s+", " ", msg)); seq_obj(NULL); notes(NULL); return()
      }

      info <- c(info, if (filled) {
        "Complete blocks only: the block lengths add up to exactly N."
      } else {
        "The last block may be incomplete: block lengths are drawn until N is
         reached and the sequence is cut at N."
      })
      info <- c(info, sprintf("Block lengths drawn: %s (%d patients in total).",
                              paste(unlist(res@bc[[1]]), collapse = ", "),
                              length(getRandList(res))))
      notes(gsub("\\s+", " ", info))
      seq_obj(res)
    })

    output$error_ui <- renderUI({ rd_error_ui(errors()) })

    output$results_ui <- renderUI({
      req(seq_obj())
      show_plot <- as.integer(input$k) == 2
      tagList(
        div(class = "rd-section-label", tags$span("Randomization Sequence")),
        if (length(notes()) > 0)
          rd_note_ui(tags$strong("Note"), tags$br(),
                     tags$ul(style = "margin:4px 0 0 0; padding-left:18px;",
                             lapply(notes(), function(m) tags$li(m)))),
        div(
          class = "rd-readout-wrap",
          div(class = "rd-readout-header",
              span(class = "rd-readout-title-bar", "Sequence")),
          verbatimTextOutput(session$ns("sequence"))
        ),
        if (show_plot) tagList(
          div(class = "rd-section-label", tags$span("Randomization Walk")),
          div(class = "rd-plot-wrap",
              plotOutput(session$ns("walk_plot"), height = "280px"))
        ),
        div(class = "rd-section-label", tags$span("Export")),
        downloadButton(session$ns("download"), "↓  Download Report")
      )
    })

    output$sequence <- renderPrint({
      req(seq_obj())
      cat(as.vector(getRandList(seq_obj())))
    })

    output$walk_plot <- renderPlot({
      req(seq_obj())
      if (as.integer(input$k) != 2) return(NULL)
      rand_walk_plot(seq_obj()@M)
    })

    output$download <- downloadHandler(
      filename = function() "rpbr_randomization.html",
      content  = function(file) {
        rmarkdown::render(
          "Report/Report.Rmd",
          output_file = file,
          params      = list(Seq = seq_obj()),
          envir       = new.env(parent = globalenv())
        )
      }
    )
  })
}
