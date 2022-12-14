box::use(

)

#' @export
create_game <- function() {
  temp_parent_dir <- tempdir()
  game_dir <- tempfile("", temp_parent_dir)
  dir.create(game_dir)

  file.create(file.path(game_dir, "player_1.txt"))
  file.create(file.path(game_dir, "player_2.txt"))

  game_dir
}

#' @export
get_game_dir <- function(game_id) {
  if (length(game_id) == 0 || is.na(game_id)) {
    return(NULL)
  }

  parent_temp_dir <- dirname(tempdir())
  all_temp_dirs <- list.dirs(parent_temp_dir)

  game_dir_loc <- which(game_id == basename(all_temp_dirs))
  if (length(game_dir_loc) == 1) {
    all_temp_dirs[game_dir_loc]
  } else {
    NULL
  }
}

#' @export
send_result <- function(rps, file) {
  cat(rps, "\n", file = file, sep = "", append = TRUE)
}

#' @export
get_player_score <- function(p1_score, p2_score) {
  min_length <- min(lengths(list(p1_score, p2_score)))
  if (min_length == 0) return(0)

  p1_vals <- match(p1_score, RPS)[1:min_length]
  p2_vals <- match(p2_score, RPS)[1:min_length]
  sum((p1_vals - 1) %% 3 == p2_vals %% 3)
}

#' @export
get_player_result <- function(p1_score, p2_score) {
  if (p1_score == p2_score) {
    "Draw!"
  } else if (get_player_score(p1_score, p2_score) == 1) {
    "You win!"
  } else {
    "You lose!"
  }
}

RPS <- c("rock", "paper", "scissors")
