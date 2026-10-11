.onAttach <- function(libname, pkgname) {
  font <- fao_font()
  if (font == "sans") {
    packageStartupMessage(
      "faofishingreport: no Chinese font was found on this ", fao_os(), " computer, ",
      "so Chinese text in charts may not display. Install one (e.g. ",
      fao_font_candidates[[fao_os()]][1], ") or choose one with fao_font(\"<font name>\")."
    )
  } else {
    packageStartupMessage("faofishingreport: ", fao_os(), ", chart font \"", font,
                          "\" (change it with fao_font())")
  }
}
