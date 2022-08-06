box::use(
  shiny,
  shinyjs[useShinyjs],
  htmltools[tags, tagList],
  app/view/new_game,
  app/view/play_game,
  app/view/game_result
)

#' @export
ui <- function(id) {
  ns <- shiny::NS(id)

  ui_tag <- tagList(
    tags$head(
      tags$title("Rock, Paper, Scissors"),
      useShinyjs()
    ),
    tags$body(
      tags$h1("Rock, Paper, Scissors!"),
      new_game$ui(ns("new_game")),
      play_game$ui(ns("play")),
      game_result$ui(ns("results"))
    )
  )

  attr(ui_tag, "lang") <- "en"
  ui_tag
}

#' @export
server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    game_info <- new_game$server("new_game")
    play_game$server("play", game_info = game_info)
    game_result$server("results", game_info = game_info)
  })
}
