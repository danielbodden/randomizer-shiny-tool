server <- function(input, output, session) {
  mod_cr_server("cr")
  mod_ebc_server("ebc")
  mod_pbr_server("pbr")
  mod_rpbr_server("rpbr")
  mod_rar_server("rar")
  mod_bsd_server("bsd")
  mod_chen_server("chen")
  mod_imbal_server("imbal")

  # Selection Bias and Chronological Bias are under construction and are not
  # part of the UI (see ui.R). Re-enable together with their nav_panel entries:
  #   mod_selbias_server("selbias")
  #   mod_chronbias_server("chronbias")
}
