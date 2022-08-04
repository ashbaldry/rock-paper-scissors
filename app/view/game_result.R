box::use(
  shiny,
  shinyjs,
  htmltools[tags, tagList],
  utils[tail],
  app/ui/ui_elems[button],
  app/logic/game[score_player]
)

#' @export
ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    # shinyjs::hidden(
      tags$section(
        style = "display: flex;",
        id = ns("results"),
        tags$section(
          tags$h3("Player"),
          shiny::uiOutput(ns("player_choice"))
        ),
        tags$section(
          tags$h3("Opponent"),
          shiny::uiOutput(ns("opponent_choice"))
        )
      # )
    ),
    tags$aside(
      class = "player-score",
      tags$h4("Player:", shiny::textOutput(ns("player_score"), inline = TRUE))
    ),
    tags$aside(
      class = "oppositon-score",
      tags$h4("Opponent:", shiny::textOutput(ns("opponent_score"), inline = TRUE))
    ),
    tags$section(
      shinyjs::hidden(
        button(ns("player_ready"), "New Game")
      )
    )
  )

}

#' @export
server <- function(id, game_info) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe(shinyjs::hide(id = "player_ready")) |>
      shiny::bindEvent(game_info$ready())

    move_timer <- shiny::reactiveTimer(100)

    player_choice <- shiny::reactive({
      shiny::req(game_info$player_choices())
      tail(game_info$player_choices(), 1)
    })

    output$player_choice <- shiny::renderUI({
      tags$img(
        style = "height: 200px; width: 200px;",
        class = "button-image-icon",
        src = paste0("static/", player_choice(), ".jpeg"),
        alt = player_choice(),
        title = player_choice()
      )
    })

    opponent_choice <- shiny::reactive({
      shiny::req(game_info$opponent_choices())
      tail(game_info$opponent_choices(), 1)
    })

    output$opponent_choice <- shiny::renderUI({
      tags$img(
        style = "height: 200px; width: 200px;",
        class = "button-image-icon",
        src = paste0("static/", opponent_choice(), ".jpeg"),
        alt = opponent_choice(),
        title = opponent_choice()
      )
    })

    player_score <- shiny::reactive({
      score_player(game_info$player_choices(), game_info$opponent_choices())
    })
    output$player_score <- shiny::renderText(player_score())

    opponent_score <- shiny::reactive({
      score_player(game_info$opponent_choices(), game_info$player_choices())
    })
    output$opponent_score <- shiny::renderText(opponent_score())
  })
}
