library(readr)
library(tibble)
library(tidyr)
library(dplyr)
library(stringr)

# Part 1 ----

games <- tibble::tibble(data = readr::read_lines(file = "../adventOfCodeDay2.txt"))

color_limits <- c("red" = 12,"green" = 13,"blue" = 14)

games %>%
  tidyr::separate_wider_delim(data = .,cols = data,delim = ":",names = c("Game","Observations")) %>%
  tidyr::separate_longer_delim(data = .,cols = Observations,delim = ";") %>%
  dplyr::mutate(Game = stringr::str_extract(string = Game,pattern = "\\d{1,3}")) %>%
  dplyr::group_by(Game) %>%
  dplyr::mutate(Observation = 1:n()) %>%
  dplyr::ungroup() %>%
  tidyr::separate_longer_delim(data = .,
                               cols = Observations,
                               delim = ",") %>%
  dplyr::mutate(Observations = stringr::str_trim(string = Observations,side = 'both')) %>%
  tidyr::separate_wider_delim(data = .,cols = Observations,delim = " ",names = c("Count","Color")) %>%
  dplyr::mutate(observation_possible = dplyr::case_when(
    Color == "red" & as.numeric(Count) <= color_limits[["red"]] ~ TRUE,
    Color == "green" & as.numeric(Count) <= color_limits[["green"]] ~ TRUE,
    Color == "blue" & as.numeric(Count) <= color_limits[["blue"]] ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  dplyr::group_by(Game) %>%
  dplyr::mutate(game_possible = dplyr::case_when(
    all(observation_possible) ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(Game,game_possible) %>%
  dplyr::filter(game_possible == TRUE) %>%
  dplyr::summarise(sum(as.numeric(Game)))

# Part 2 ----

games <- tibble::tibble(data = readr::read_lines(file = "../adventOfCodeDay2.txt"))

color_limits <- c("red" = 12,"green" = 13,"blue" = 14)

games %>%
  tidyr::separate_wider_delim(data = .,cols = data,delim = ":",names = c("Game","Observations")) %>%
  tidyr::separate_longer_delim(data = .,cols = Observations,delim = ";") %>%
  dplyr::mutate(Game = stringr::str_extract(string = Game,pattern = "\\d{1,3}")) %>%
  dplyr::group_by(Game) %>%
  dplyr::mutate(Observation = 1:n()) %>%
  dplyr::ungroup() %>%
  tidyr::separate_longer_delim(data = .,
                               cols = Observations,
                               delim = ",") %>%
  dplyr::mutate(Observations = stringr::str_trim(string = Observations,side = 'both')) %>%
  tidyr::separate_wider_delim(data = .,cols = Observations,delim = " ",names = c("Count","Color")) %>%
  dplyr::select(-Observation) %>%
  dplyr::group_by(Game,Color) %>%
  dplyr::summarise(maximum = max(as.numeric(Count))) %>%
  dplyr::summarise(power = prod(maximum)) %>%
  dplyr::summarise(sum = sum(power))
