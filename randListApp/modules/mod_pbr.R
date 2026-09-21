# =============================================================================
# Module: Permuted Block Randomization (PBR)
#
# pbrPar(bc, K, ratio, groups) — supports 2–6 arms and unequal allocation
# bc: vector of block sizes, one entry per block
# genSeq(par, seed = seed)
#
# If an incomplete last block is allowed, the sequence is generated with
# rpbrPar(N, rb = <one block length>, filledBlock = FALSE), which is permuted
# block randomization with a fixed block length that is cut at N.
# Walk plot is only shown for 2-arm designs.
# =============================================================================

mod_pbr_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      h5("Parameters"),

      rd_n_input(ns("n"), "Target sample size:", 12),

      numericInput(ns("block_size"), "Block size:",
                   value = 4, min = 2, max = RD_MAX_N, step = 2),

      checkboxInput(ns("allow_incomplete"), "Allow incomplete last block",
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
          tags$small("Sample size and block size must be whole numbers. The block
                      size must be a multiple of the sum of the allocation ratios
                      (2 for a 1:1 two-arm design, i.e. an even number); if it is
                      not, the next multiple is used. Without an incomplete last
                      block, the sample size is rounded up so that the list
                      contains complete blocks only."))
    ),

    # ── Main panel ────────────────────────────────────────────────────────
    div(
      class = "rd-main-inner",

      div(class = "rd-pattern-strip"),
      rand_steps_ui(),

      h2(class = "rd-method-title", "Permuted Block Randomization"),
      span(class = "rd-method-badge badge-green", "2 – 6 arms"),

      p(class = "rd-description",
        "Permuted Block Randomization divides the patient sequence into blocks of a
        fixed size. Within each block, treatment assignments are a uniformly random
        permutation of the numbers of patients given by the allocation ratio.
        Balance according to the allocation ratio is guaranteed at the end of every
        block. By default only complete blocks are generated, i.e. the total sample
        size is a multiple of the block length; alternatively the last block may
        remain incomplete, in which case the sequence ends exactly at the sample
        size entered."),

      p(class = "rd-reference",
        "Matts JP, Lachin JM (1988). Properties of permuted-block randomization in
        clinical trials. Controlled Clinical Trials, 9(4), 327–344.
        doi:10.1016/0197-2456(88)90049-2"),

      uiOutput(ns("error_ui")),
      uiOutput(ns("results_ui"))
    )
  )
}


mod_pbr_server <- function(id) {
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
        rd_check_int(input$n, "Target sample size", min = 2, max = RD_MAX_N),
        rd_check_int(input$block_size, "Block size", min = 2, max = RD_MAX_N),
        rd_check_ratio(input, "ratio", k),
        rd_check_seed(input$seed)
      )
      if (length(msgs) > 0) {
        errors(msgs); seq_obj(NULL); notes(NULL); return()
      }
      errors(NULL)

      n          <- as.integer(input$n)
      block_size <- as.integer(input$block_size)
      ratio      <- get_ratio(input, "ratio", k)
      groups     <- get_groups(input, "name", k)
      ratio_sum  <- sum(ratio)
      incomplete <- isTRUE(input$allow_incomplete)

      # Every block must contain each treatment in the given ratio, so the block
      # length has to be a multiple of sum(ratio) — for a 1:1 two-arm design this
      # is the familiar "even block size".
      info <- character(0)
      if (block_size %% ratio_sum != 0) {
        new_bs <- ceiling(block_size / ratio_sum) * ratio_sum
        info   <- c(info, sprintf("The block size must be a multiple of the sum of
                                   the allocation ratios (%d); the next multiple,
                                   b = %d, was used instead of %d.",
                                  ratio_sum, new_bs, block_size))
        block_size <- as.integer(new_bs)
      }

      res <- tryCatch({
        if (incomplete) {
          # A single possible block length with filledBlock = FALSE is permuted
          # block randomization that is cut at N; randomizeR labels the design
          # RPBR(b).
          par <- rpbrPar(N = n, rb = block_size, K = k, ratio = ratio,
                         groups = groups, filledBlock = FALSE)
        } else {
          n_blocks <- ceiling(n / block_size)
          par <- pbrPar(bc = rep(block_size, n_blocks), K = k, ratio = ratio,
                        groups = groups)
        }
        genSeq(par, seed = as.integer(input$seed))
      }, error = function(e) e)

      if (inherits(res, "error")) {
        errors(conditionMessage(res)); seq_obj(NULL); notes(NULL); return()
      }

      if (incomplete) {
        bc <- unlist(res@bc[[1]])
        info <- c(info, sprintf("The last block may remain incomplete: the
                                 sequence ends exactly at N = %d.", n))
      } else {
        bc       <- res@bc
        n_actual <- sum(bc)
        if (n_actual != n) {
          info <- c(info, sprintf("Only complete blocks are generated: the target
                                   sample size N = %d is not a multiple of the
                                   block size b = %d, so the list was extended to
                                   %d = %d x %d patients. Tick “allow
                                   incomplete last block” to keep N = %d.",
                                  n, block_size, n_actual, n_actual / block_size,
                                  block_size, n))
        }
      }
      info <- c(info, sprintf("Block lengths: %s (%d patients in total).",
                              paste(bc, collapse = ", "),
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
      filename = function() "pbr_randomization.html",
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
