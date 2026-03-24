ui <- page_navbar(

  # ── Navbar brand ────────────────────────────────────────────────────────────
  title = tags$div(
    style = "display:flex; align-items:center; gap:16px; height:64px; padding:0;",
    tags$img(
      src    = "logo_textless_light.png",
      class  = "rd-navbar-logo",
      alt    = "RealiseD"
    ),
    tags$div(
      class = "rd-navbar-tool-label",
      tags$span(class = "rd-tool-name",  "Randomization"),
      tags$span(class = "rd-tool-sub",   "List Generator")
    )
  ),

  # ── Theme ───────────────────────────────────────────────────────────────────
  theme = bs_theme(
    version      = 5,
    bg           = "#f5f7f5",
    fg           = "#312f30",
    primary      = "#00774A",
    secondary    = "#d1d2d4",
    success      = "#00774A",
    danger       = "#f6a092",
    `navbar-bg`  = "#00774A",
    base_font    = font_google("Montserrat"),
    heading_font = font_google("Montserrat"),
    font_scale   = 0.95
  ),

  # ── Global head ─────────────────────────────────────────────────────────────
  header = tags$head(
    tags$link(rel = "stylesheet", href = "style.css"),
    tags$script(HTML("
      function rdGoToTab(tabName) {
        var all = document.querySelectorAll('.nav-link, .dropdown-item');
        for (var i = 0; i < all.length; i++) {
          if (all[i].textContent.trim() === tabName) { all[i].click(); return; }
        }
      }
    "))
  ),

  # ── Introduction ────────────────────────────────────────────────────────────
  nav_panel(
    "Introduction",
    div(
      class = "rd-intro-wrap",

      # Hero
      div(
        class = "rd-hero",
        div(
          class = "rd-hero-inner",
          p(class = "rd-hero-eyebrow", "Clinical Trial Randomization"),
          h1(class = "rd-hero-title",
             "Generating Randomization Lists for Clinical Trials"),
          p(class = "rd-hero-sub",
            "Select a randomization procedure, configure the parameters for your
            trial, and click Generate to produce an allocation sequence with a
            fixed seed for reproducibility and a downloadable report documenting
            the method, parameters, and generated list.")
        )
      ),

      # Workflow steps
      div(
        class = "rd-steps-strip",
        div(class = "rd-step",
            div(class = "rd-step-num", "1"),
            div(class = "rd-step-text", "Choose a procedure")),
        div(class = "rd-step-sep"),
        div(class = "rd-step",
            div(class = "rd-step-num", "2"),
            div(class = "rd-step-text", "Configure parameters")),
        div(class = "rd-step-sep"),
        div(class = "rd-step",
            div(class = "rd-step-num", "3"),
            div(class = "rd-step-text", "Generate sequence")),
        div(class = "rd-step-sep"),
        div(class = "rd-step",
            div(class = "rd-step-num", "4"),
            div(class = "rd-step-text", "Download report"))
      ),

      # Clickable method cards — grouped by category
      div(
        class = "rd-methods-section",

        # Group 1: Unbounded randomization
        p(class = "rd-methods-heading", "Unbounded Randomization"),
        div(
          class = "rd-methods-grid",

          div(class = "rd-method-card rd-card-link",
              onclick = "rdGoToTab('Complete Randomization')",
              p(class = "rd-card-name", "Complete Randomization"),
              p(class = "rd-card-desc",
                "Randomization achieved by flipping a fair coin. Sometimes also
                referred to as simple or full randomization."),
              div(class = "rd-card-meta",
                  span(class = "rd-card-arms", "2\u20136 arms"),
                  span(class = "rd-card-arrow", "\u2192")),
              ),

          div(class = "rd-method-card rd-card-link",
              onclick = "rdGoToTab(\"Efron's Biased Coin\")",
              p(class = "rd-card-name", "Efron's Biased Coin Design"),
              p(class = "rd-card-desc",
                "Randomization using a biased coin in favor of the treatment
                with fewer allocations and a fair coin in case of equal
                allocations."),
              div(class = "rd-card-meta",
                  span(class = "rd-card-arms", "2 arms"),
                  span(class = "rd-card-arrow", "\u2192")))
        ),

        # Group 2: Terminal balance
        p(class = "rd-methods-heading", "Terminal Balance"),
        div(
          class = "rd-methods-grid",

          div(class = "rd-method-card rd-card-link",
              onclick = "rdGoToTab('Permuted Block')",
              p(class = "rd-card-name", "Permuted Block Randomization"),
              p(class = "rd-card-desc",
                "Allocation in blocks of fixed length, with randomization
                within each block according to the Random Allocation Rule."),
              div(class = "rd-card-meta",
                  span(class = "rd-card-arms", "2 arms"),
                  span(class = "rd-card-arrow", "\u2192"))),

          div(class = "rd-method-card rd-card-link",
              onclick = "rdGoToTab('Random Permuted Block')",
              p(class = "rd-card-name", "Random Permuted Block Randomization"),
              p(class = "rd-card-desc",
                "Permuted block randomization with block sizes for each block
                randomly selected from a predefined set."),
              div(class = "rd-card-meta",
                  span(class = "rd-card-arms", "2 arms"),
                  span(class = "rd-card-arrow", "\u2192"))),

          div(class = "rd-method-card rd-card-link",
              onclick = "rdGoToTab('Random Allocation Rule')",
              p(class = "rd-card-name", "Random Allocation Rule"),
              p(class = "rd-card-desc",
                "Randomization assigning the same proportion of patients to
                each treatment. Guarantees exact balance at trial end."),
              div(class = "rd-card-meta",
                  span(class = "rd-card-arms", "2\u20136 arms"),
                  span(class = "rd-card-arrow", "\u2192")))
        ),

        # Group 3: Maximum tolerated imbalance
        p(class = "rd-methods-heading", "Maximum Tolerated Imbalance (MTI)"),
        div(
          class = "rd-methods-grid",

          div(class = "rd-method-card rd-card-link",
              onclick = "rdGoToTab('Big Stick Design')",
              p(class = "rd-card-name", "Big Stick Design"),
              p(class = "rd-card-desc",
                "Complete randomization with a deterministic assignment when
                the MTI is reached."),
              div(class = "rd-card-meta",
                  span(class = "rd-card-arms", "2 arms"),
                  span(class = "rd-card-arrow", "\u2192"))),

          div(class = "rd-method-card rd-card-link",
              onclick = "rdGoToTab(\"Chen's Design\")",
              p(class = "rd-card-name", "Chen's Design"),
              p(class = "rd-card-desc",
                "Efron's Biased Coin Design with a deterministic assignment
                when the MTI is reached."),
              div(class = "rd-card-meta",
                  span(class = "rd-card-arms", "2 arms"),
                  span(class = "rd-card-arrow", "\u2192")))
        )
      ),

      # Credits
      div(
        class = "rd-credits",
        "Shiny Tool developed at\u00a0",
        tags$a(href = "https://realised-ihi.eu/", target = "_blank", "RealiseD"),
        "\u00a0by ",
        tags$span(style = "color:#00774A; font-weight:600;", "Stefanie Schoenen"),
        " and\u00a0",
        tags$a(href = "https://danielbodden.de/", target = "_blank", "Daniel Bodden")
      )
    )
  ),

  # ── Unbounded Randomization ──────────────────────────────────────────────────
  nav_menu(
    "Unbounded Randomization",
    nav_panel("Complete Randomization", mod_cr_ui("cr")),
    nav_panel("Efron's Biased Coin",    mod_ebc_ui("ebc"))
  ),

  # ── Terminal Balance ─────────────────────────────────────────────────────────
  nav_menu(
    "Terminal Balance",
    align = "right",
    nav_panel("Permuted Block",        mod_pbr_ui("pbr")),
    nav_panel("Random Permuted Block", mod_rpbr_ui("rpbr")),
    nav_panel("Random Allocation Rule",mod_rar_ui("rar"))
  ),

  # ── Maximum Tolerated Imbalance ──────────────────────────────────────────────
  nav_menu(
    "Maximum Tolerated Imbalance",
    align = "right",
    nav_panel("Big Stick Design", mod_bsd_ui("bsd")),
    nav_panel("Chen's Design",    mod_chen_ui("chen"))
  ),

  # ── Bias Evaluation ───────────────────────────────────────────────────────────
  nav_menu(
    "Bias Evaluation",
    align = "right",
    nav_panel("Imbalance",          mod_imbal_ui("imbal")),
    nav_panel("Selection Bias",     mod_selbias_ui("selbias")),
    nav_panel("Chronological Bias", mod_chronbias_ui("chronbias"))
  ),

  # ── About ─────────────────────────────────────────────────────────────────────
  nav_panel(
    "About",
    div(
      class = "rd-about-wrap",

      # ── Hero ──────────────────────────────────────────────────────────────────
      div(class = "rd-about-hero",
        div(class = "rd-about-hero-inner",
          p(class = "rd-hero-eyebrow", "Randomization List Generator"),
          h1(class = "rd-hero-title", "About This Tool"),
          p(class = "rd-about-hero-sub",
            "A free, open-source Shiny application for generating and evaluating
            randomization sequences in clinical trials, built on the ",
            tags$strong("randomizeR"), " R package.")
        )
      ),

      div(class = "rd-about-body",

        # ── What is this tool ──────────────────────────────────────────────────
        div(class = "rd-about-section",
          h3(class = "rd-about-section-title", "What is this tool?"),
          p("The Randomization List Generator provides a graphical interface for
            generating reproducible randomization sequences and assessing their
            statistical properties."),
          p("The tool is designed for clinical trial statisticians, methodologists,
            and researchers who need to plan, document, and evaluate randomization
            strategies. All sequences are generated with a user-defined seed for
            full reproducibility, and results can be exported as a PDF report.")
        ),

        # ── Powered by ────────────────────────────────────────────────────────
        div(class = "rd-about-section",
          h3(class = "rd-about-section-title", "Powered by randomizeR"),
          p(HTML("This tool is a Shiny interface built on the
            <a href='https://cran.r-project.org/package=randomizeR' target='_blank'>randomizeR</a>
            R package, which implements randomization procedures and bias
            assessment methods for clinical trials.")),
          div(class = "rd-about-cite-box",
            p(class = "rd-about-cite",
              HTML("Uschner D, Schindler D, Hilgers RD, Heussen N (2018).
              <em>randomizeR: An R package for the assessment and implementation
              of randomization in clinical trials.</em>
              Journal of Statistical Software, 85(8), 1\u201333.
              doi:10.18637/jss.v085.i08"))
          )
        ),

        # ── Contributors ──────────────────────────────────────────────────────
        div(class = "rd-about-section",
          h3(class = "rd-about-section-title", "Contributors"),
          tags$ul(class = "rd-about-team",

            tags$li(class = "rd-about-person",
              div(class = "rd-about-person-left",
                tags$a(class = "rd-about-person-name",
                       href = "https://danielbodden.de/", target = "_blank",
                       "Daniel Bodden"),
                div(class = "rd-about-person-role", "Co-developer")
              )
            ),

            tags$li(class = "rd-about-person",
              div(class = "rd-about-person-left",
                tags$a(class = "rd-about-person-name",
                       href = "https://www.linkedin.com/in/stefanie-schoenen-486b3a270/",
                       target = "_blank", "Stefanie Schoenen"),
                div(class = "rd-about-person-role", "Co-developer")
              )
            ),

            tags$li(class = "rd-about-person",
              div(class = "rd-about-person-left",
                span(class = "rd-about-person-name rd-about-person-nolink",
                     "Ralf-Dieter Hilgers"),
                div(class = "rd-about-person-role", "Supervisor")
              )
            ),

            tags$li(class = "rd-about-person rd-about-person-contrib",
              div(class = "rd-about-person-left",
                tags$a(class = "rd-about-person-name",
                       href = "https://drazsolkov.com/", target = "_blank",
                       "Denis Raszolkov"),
                div(class = "rd-about-person-role", "Initial concept")
              )
            )
          )
        ),

        # ── Source ────────────────────────────────────────────────────────────
        div(class = "rd-about-section",
          h3(class = "rd-about-section-title", "Source code"),
          p(HTML("The source code is openly available on
            <a href='https://github.com/danielbodden/randomizer-shiny-tool'
            target='_blank'>GitHub</a>.
            Contributions and issue reports are welcome."))
        ),

        # ── Disclaimer ────────────────────────────────────────────────────────
        div(class = "rd-about-section",
          h3(class = "rd-about-section-title", "Disclaimer"),
          p("Users are responsible for ensuring that the generated sequences
            meet the requirements of their specific trial protocol and
            regulatory context.")
        )

      )
    )
  )
)




