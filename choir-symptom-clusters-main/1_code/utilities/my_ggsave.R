# Plot saving function
# Saves in png, svg, and eps in case journals want specific formats.
my_ggsave <- function(p, pname, w = 18.4, ar = "4:3") {
  if(ar == "4:3"){ 
    h = 3*(18.4/4)
  } else if(ar == "16:9") {
    h = 9*(18.4/16)
  } else {
    print("Invalid aspect ratio.")
    return(-1)
  }
  ggplot2::ggsave(
    glue::glue("{pname}.png")
    , plot=p
    , device="png"
    , dpi="retina"
    , width = w
    , height = h
    , units = 'cm'
  )
  ggplot2::ggsave(
    glue::glue("{pname}.svg")
    , plot=p
    , device="svg"
    , width = w
    , height = h
    , units = 'cm'
  )
  ggplot2::ggsave(
    glue::glue("{pname}.eps")
    , plot=p
    , device="eps"
    , width = w
    , height = h
    , units = 'cm'
  )
}
