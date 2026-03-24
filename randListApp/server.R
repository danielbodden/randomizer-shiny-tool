server <- function(input, output, session) {
  mod_cr_server("cr")
  mod_ebc_server("ebc")
  mod_pbr_server("pbr")
  mod_rpbr_server("rpbr")
  mod_rar_server("rar")
  mod_bsd_server("bsd")
  mod_chen_server("chen")
  mod_selbias_server("selbias")
  mod_chronbias_server("chronbias")
  mod_imbal_server("imbal")
}
