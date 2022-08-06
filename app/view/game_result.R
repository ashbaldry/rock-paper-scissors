box::use(
  shiny,
  shinyjs,
  htmltools[tags, tagList],
  utils[tail],
  app/ui/ui_elems[button, image_jpeg],
  app/logic/game[get_player_score, get_player_result]
)

#' @export
ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    tags$section(
      class = "scores",
      tags$aside(
        class = "player-score",
        tags$h2("Player:", shiny::textOutput(ns("player_score"), inline = TRUE))
      ),
      tags$aside(
        class = "opponent-score",
        tags$h2("Opponent:", shiny::textOutput(ns("opponent_score"), inline = TRUE))
      )
    ),
    tags$section(
      class = "rps-countdown",
      shiny::textOutput(ns("rps"))
    ),
    shinyjs::hidden(
      tags$section(
        id = ns("results"),
        tags$div(
          id = ns("player_result"),
          class = "shiny-text-output player-result"
        ),
        tags$section(
          shinyjs::hidden(
            button(ns("player_ready"), "New Game", class = "ready-button")
          )
        ),
        tags$div(
          class = "game-result",
          tags$section(
            class = "person-result",
            tags$h3("Player"),
            shiny::uiOutput(ns("player_choice"), class = "person-choice")
          ),
          tags$section(
            class = "person-result",
            tags$h3("Opponent"),
            shiny::uiOutput(ns("opponent_choice"), class = "person-choice")
          )
        )
      )
    )
  )

}

#' @export
server <- function(id, game_info) {
  shiny::moduleServer(id, function(input, output, session) {
    move_timer <- shiny::reactiveTimer(100)
    countdown <- shiny::reactiveVal(0)

    shiny::observe({
      if (game_info$ready()) {
        countdown(1500)
      }
    }) |>
      shiny::bindEvent(game_info$ready())

    shiny::observe({
      if (countdown() > 0) {
        countdown(max(countdown() - 100, 0))
        if (countdown() == 0) {
          shinyjs::show(id = "results")
          shinyjs::show(id = "player_ready")
        }
      }
    }) |>
      shiny::bindEvent(move_timer())

    rps <- shiny::reactive({
      if (countdown() == 0) {
        NULL
      } else {
        c("Scissors", "Paper", "Rock")[countdown() %/% 501 + 1]
      }
    })

    output$rps <- shiny::renderText(rps())

    #### Current Result ####
    player_choice <- shiny::reactive({
      shiny::req(game_info$player_choices())
      tail(game_info$player_choices(), 1)
    })
    output$player_choice <- shiny::renderUI(image_jpeg(player_choice()))
    shiny::outputOptions(output, "player_choice", suspendWhenHidden = FALSE)

    opponent_choice <- shiny::reactive({
      shiny::req(game_info$opponent_choices())
      if (length(game_info$opponent_choices()) == game_info$n_games()) {
        tail(game_info$opponent_choices(), 1)
      } else {
        tail(game_info$opponent_choices(), 2)[1]
      }
    })
    output$opponent_choice <- shiny::renderUI(image_jpeg(opponent_choice()))
    shiny::outputOptions(output, "opponent_choice", suspendWhenHidden = FALSE)

    output$player_result <- shiny::renderText(get_player_result(player_choice(), opponent_choice()))
    shiny::outputOptions(output, "player_result", suspendWhenHidden = FALSE)

    #### Overall Score ####
    player_score <- shiny::reactiveVal(0)
    opponent_score <- shiny::reactiveVal(0)

    shiny::observe({
      if (countdown() == 0) {
        player_score(
          get_player_score(
            game_info$player_choices(),
            game_info$opponent_choices()
          )
        )
        opponent_score(
          get_player_score(
            game_info$opponent_choices(),
            game_info$player_choices()
          )
        )
      }
    }) |>
      shiny::bindEvent(
        countdown(),
        game_info$game()
      )

    output$player_score <- shiny::renderText(player_score())
    output$opponent_score <- shiny::renderText(opponent_score())

    #### New Game ####
    shiny::observe({
      shinyjs::enable(selector = ".image-button")
      shinyjs::hide(id = "player_ready")
      shinyjs::hide(id = "results")
    }) |>
      shiny::bindEvent(input$player_ready)
  })
}
