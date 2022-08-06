box::use(
  shiny,
  htmltools[tags, tagList],
  app/ui/ui_elems[button, text_input, modal],
  app/logic/game[create_game, get_game_dir]
)

#' @export
ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    tags$section(
      button(ns("create"), "Create Game"),
      button(ns("join"), "Join Game")
    ),
    tags$section(
      class = "game-id",
      "Game ID:",
      tags$b(shiny::textOutput(ns("game_id"), inline = TRUE))
    )
  )
}

#' @export
server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    game_dir <- shiny::reactiveVal()
    game_id <- shiny::reactiveVal()
    player_id <- shiny::reactiveVal(0)

    n_games <- shiny::reactiveVal(0)
    player_ready <- shiny::reactiveVal(FALSE)
    opponent_ready <- shiny::reactiveVal(FALSE)
    ready <- shiny::reactiveVal(FALSE)

    shiny::observe({
      shiny::insertUI(
        selector = ".game-id",
        where = "afterEnd",
        modal(
          id = ns("join_modal"),
          class = "join-modal",
          title = "Join Game",
          tags$form(
            tags$fieldset(
              id = ns("game_field"),
              text_input(ns("game_id"), "Game ID"),
              button(
                ns("submit"),
                "Join"
              ),
              button(
                ns("cancel"),
                "Cancel"
              )
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
      n_games(0)

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
        n_games(0)

        game_dir(game_dir)
        game_id(input$game_id)
        shiny::removeUI(selector = paste0("#", ns("join_modal")))
      }
    }) |>
      shiny::bindEvent(
        input$submit
      )

    shiny::observe({
      shiny::removeUI(selector = paste0("#", ns("invalid_game")))
      shiny::removeUI(selector = paste0("#", ns("join_modal")))
    }) |>
      shiny::bindEvent(
        input$cancel
      )

    output$game_id <- shiny::renderText(game_id())

    #### Extra Reactives ####
    player_file <- shiny::reactive({
      shiny::req(game_dir())
      file.path(game_dir(), paste0("player_", player_id(), ".txt"))
    })

    opponent_file <- shiny::reactive({
      shiny::req(game_dir())
      file.path(game_dir(), paste0("player_", 3 - player_id(), ".txt"))
    })

    choices <- shiny::reactivePoll(
      100,
      session,
      \() {
        if (is.null(game_dir())) {
          ""
        } else {
          max(file.info(c(player_file(), opponent_file()))$mtime)
        }
      },
      \() {
        list(
          player = readLines(player_file()),
          opponent = readLines(opponent_file())
        )
      }
    )

    player_choices <- shiny::reactive(choices()$player)
    opponent_choices <- shiny::reactive(choices()$opponent)

    shiny::observe({
      player_chosen <- length(choices()$player) > n_games()
      opponent_chosen <- length(choices()$opponent) > n_games()

      player_ready(player_chosen)
      opponent_ready(opponent_chosen)
      ready(player_chosen && opponent_chosen)
    }) |>
      shiny::bindEvent(
        choices()
      )

    shiny::observe(priority = -1, {
      if (ready()) {
        n_games(n_games() + 1)
        ready(FALSE)
      }
    }) |>
      shiny::bindEvent(
        ready()
      )

    list(
      game = game_id,
      game_dir = game_dir,
      n_games = n_games,
      player = player_id,
      player_file = player_file,
      player_choices = player_choices,
      opponent_file = opponent_file,
      opponent_choices = opponent_choices,
      player_ready = player_ready,
      opponent_ready = opponent_ready,
      ready = ready
    )
  })
}
