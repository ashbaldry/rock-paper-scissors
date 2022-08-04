box::use(
  shiny,
  shinyjs[disable, enable],
  htmltools[tags],
  app/ui/ui_elems[image_button],
  app/logic/game[send_result]
)

#' @export
ui <- function(id) {
  ns <- shiny::NS(id)

  tags$section(
    image_button(ns("rock"), "rock"),
    image_button(ns("paper"), "paper"),
    image_button(ns("scissors"), "scissors")
  )
}

#' @export
server <- function(id, game_info) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe(shinyjs::disable(selector = ".image-button")) |>
      shiny::bindEvent(input$rock, input$paper, input$scissors, ignoreInit = TRUE, ignoreNULL = TRUE)

    shiny::observe(if (isTRUE(game_info$ready())) shinyjs::enable(selector = ".image-button")) |>
      shiny::bindEvent(game_info$ready())

    shiny::observe(send_result("rock", file = game_info$player_file())) |>
      shiny::bindEvent(input$rock)

    shiny::observe(send_result("paper", file = game_info$player_file())) |>
      shiny::bindEvent(input$paper)

    shiny::observe(send_result("scissors", file = game_info$player_file())) |>
      shiny::bindEvent(input$scissors)
  })
}
