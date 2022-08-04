box::use(
  htmltools[tags, tagList]
)

#' @export
button <- function(id, label, ...) {
  tags$button(
    id = id,
    class = "action-button",
    type = "button",
    label,
    ...
  )
}

#' @export
image_button <- function(id, img_name) {
  tags$button(
    id = id,
    class = "image-button action-button",
    type = "button",
    tags$img(
      style = "height: 200px; width: 200px;",
      class = "button-image-icon",
      src = paste0("static/", img_name, ".jpeg"),
      alt = img_name,
      title = img_name
    )
  )
}

#' @export
text_input <- function(id, label, placeholder = NULL) {
  tagList(
    tags$label(
      `for` = id,
      label
    ),
    tags$input(
      id = id,
      type = "text",
      placeholder = placeholder,
      value = NULL
    )
  )
}

#' @export
modal <- function(id, title, ...) {
  tags$div(
    id = id,
    class = "modal",
    tags$h2(
      class = "modal-header",
      title
    ),
    tags$section(
      class = "modal-body",
      ...
    )
  )
}
