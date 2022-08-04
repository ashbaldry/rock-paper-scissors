box::use(
  shiny,
  shinyjs[hidden, show, useShinyjs],
  htmltools[tags, tagList],
  utils[read.csv, write.csv],
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
      tags$section(
        "Game ID:",
        tags$b(shiny::textOutput(ns("game_id"), inline = TRUE))
      ),
      hidden(
        tags$section(
          id = ns("game_buttons"),
          play_game$ui(ns("play"))
        )
      ),
      hidden(
        tags$section(
          id = ns("game_results"),
          game_result$ui(ns("results"))
        )
      )
    )
  )

  attr(ui_tag, "lang") <- "en"
  ui_tag
}

#' @export
server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    game_info <- new_game$server("new_game")

    shiny::observe({
      shiny::req(game_info$game_dir())
      show(id = "game_buttons")
      show(id = "game_results")
    }) |>
      shiny::bindEvent(game_info$game_dir())

    output$game_id <- shiny::renderText({
      shiny::req(game_info$game())
      game_info$game()
    })

    play_game$server("play", game_info = game_info)

    game_result$server("results", game_info = game_info)
  })
}
