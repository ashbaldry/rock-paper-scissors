box::use(
  shiny,
  htmltools[tags],
  app/ui/ui_elems[button, text_input, modal],
  app/logic/game[create_game, get_game_dir]
)

#' @export
ui <- function(id) {
  ns <- shiny::NS(id)

  tags$section(
    button(ns("create"), "Create Game"),
    button(ns("join"), "Join Game")
  )
}

#' @export
server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    game_dir <- shiny::reactiveVal()
    game_id <- shiny::reactiveVal()
    player_id <- shiny::reactiveVal(0)

    shiny::observe({
      shiny::insertUI(
        selector = "body",
        where = "beforeEnd",
        modal(
          id = ns("join_modal"),
          title = "Join Game",
          tags$form(
            tags$fieldset(
              id = ns("game_field"),
              text_input(ns("game_id"), "Game ID")
            ),
            button(
              ns("submit"),
              "Join"
            )
          )
        )
      )
    }) |>
      shiny::bindEvent(
        input$join
      )

    shiny::observe({
      player_id(1)
      game_dir <- create_game()
      game_dir(game_dir)
      game_id(basename(game_dir))
    }) |>
      shiny::bindEvent(
        input$create
      )

    shiny::observe({
      game_dir <- get_game_dir(input$game_id)

      if (is.null(game_dir)) {
        shiny::removeUI(selector = paste0("#", ns("invalid_game")))
        shiny::insertUI(
          selector = paste0("#", ns("game_field")),
          where = "beforeEnd",
          tags$div(
            id = ns("invalid_game"),
            class = "input-error",
            "Cannot find selected game"
          )
        )
      } else {
        player_id(2)
        game_dir(game_dir)
        game_id(input$game_id)
        shiny::removeUI(selector = paste0("#", ns("join_modal")))
      }
    }) |>
      shiny::bindEvent(
        input$submit
      )

    #### Extra Reactives ####
    player_file <- shiny::reactive({
      shiny::req(game_dir())
      file.path(game_dir(), paste0("player_", player_id(), ".txt"))
    })
    player_choices <- shiny::reactivePoll(
      100,
      session, \() if (is.null(game_dir())) "" else file.info(player_file())$mtime,
      \() readLines(player_file())
    )

    player_ready_file <- shiny::reactive({
      shiny::req(game_dir())
      file.path(game_dir(), paste0("player_", player_id(), "_ready.txt"))
    })
    player_ready <- shiny::reactivePoll(
      100,
      session, \() if (is.null(game_dir())) "" else file.info(player_ready_file())$mtime,
      \() as.logical(readLines(player_ready_file()))
    )

    opponent_file <- shiny::reactive({
      shiny::req(game_dir())
      file.path(game_dir(), paste0("player_", 3 - player_id(), ".txt"))
    })
    opponent_choices <- shiny::reactivePoll(
      100,
      session, \() if (is.null(game_dir())) "" else file.info(opponent_file())$mtime,
      \() readLines(opponent_file())
    )

    opposition_ready_file <- shiny::reactive({
      shiny::req(game_dir())
      file.path(game_dir(), paste0("player_", 3 - player_id(), "_ready.txt"))
    })
    opposition_ready <- shiny::reactivePoll(
      100,
      session, \() if (is.null(game_dir())) "" else file.info(opposition_ready_file())$mtime,
      \() as.logical(readLines(opposition_ready_file()))
    )

    ngames_file <- shiny::reactive({
      shiny::req(game_dir())
      file.path(game_dir(), "game_number.txt")
    })
    n_games <- shiny::reactivePoll(
      100,
      session, \() if (is.null(game_dir())) "" else file.info(ngames_file())$mtime,
      \() as.numeric(readLines(ngames_file()))
    )

    ready <- shiny::reactive({
      all(
        player_choices()[1] != "",
        opponent_choices()[1] != "",
        n_games() + 1 == length(player_choices()),
        n_games() + 1 == length(opponent_choices())
      )
    })

    shiny::observe(priority = -1, {
      if (ready()) writeLines(as.character(n_games() + 1), ngames_file())
    }) |>
      shiny::bindEvent(ready())

    list(
      game = game_id,
      game_dir = game_dir,
      n_games = n_games,
      ngames_file = ngames_file,
      player = player_id,
      player_file = player_file,
      player_choices = player_choices,
      player_ready_file = player_ready_file,
      player_ready = player_ready,
      opponent_file = opponent_file,
      opponent_choices = opponent_choices,
      opposition_ready_file = opposition_ready_file,
      opposition_ready = opposition_ready,
      ready = ready
    )
  })
}
