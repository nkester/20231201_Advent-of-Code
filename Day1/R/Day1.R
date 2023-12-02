library(readr)
library(tibble)
library(stringr)
library(dplyr)

# Part One ----
text <- tibble::tibble(calibrationdoc = readr::read_lines(file = "../adventOfCodeDay1.txt"))

tib_calibration <- dplyr::mutate(.data = text,
                                 calibration_values1 = stringr::str_extract(string = calibrationdoc,pattern = "(?<=^|[^0-9])[0-9]"),
                                 calibration_values2 = stringr::str_extract(string = calibrationdoc,pattern = "\\d{1}(?=($|\\D*$))"),
                                 combined = as.numeric(paste0(calibration_values1,calibration_values2)))


val_sumCalibration <- tib_calibration %>%
  dplyr::summarise(sum(combined))


val_sumCalibration
# Correct answer: 55029

# Part One attempt two ----
## Using the sane approach I used in part 2. This gives the correct answer again.
numbers_pattern <- c('(\\d)')

numbers_conversion <- tibble::tibble(text = c('one','two','three','four','five','six','seven','eight','nine','zero'),
                                     digit = c('1','2','3','4','5','6','7','8','9','0'))

text %>%
  dplyr::mutate(calibrationdoc = stringr::str_to_lower(calibrationdoc)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total_match = stringr::str_match_all(string = calibrationdoc,
                                                     pattern = numbers_pattern),
                first_match = total_match[[1]][1],
                last_match = total_match[nrow(total_match),1]) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(combined = as.numeric(paste0(first_match,last_match))) %>%
  dplyr::summarise(sum(combined))

rm(list = ls())
# Part Two ----

text <- tibble::tibble(calibrationdoc = readr::read_lines(file = "../adventOfCodeDay1.txt"))

numbers_pattern <- c('(one|two|three|four|five|six|seven|eight|nine|\\d)')

numbers_conversion <- tibble::tibble(text = c('one','two','three','four','five','six','seven','eight','nine'),
                                     digit = c('1','2','3','4','5','6','7','8','9'))

text %>%
  dplyr::mutate(calibrationdoc = stringr::str_to_lower(calibrationdoc)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total_match = stringr::str_match_all(string = calibrationdoc,
                                              pattern = numbers_pattern),
                first_match = total_match[[1]][1],
                last_match = total_match[nrow(total_match),1]) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(x = .,
                   y = numbers_conversion,
                   by = c('first_match' = 'text')) %>%
  dplyr::rename('first_match_original' = 'first_match',
                'first_match' = 'digit') %>%
  dplyr::left_join(x = .,
                   y = numbers_conversion,
                   by = c('last_match' = 'text')) %>%
  dplyr::rename('last_match_original' = 'last_match',
                'last_match' = 'digit') %>%
  dplyr::mutate(first_match = dplyr::case_when(is.na(first_match) ~ first_match_original,
                                               TRUE ~ first_match),
                last_match = dplyr::case_when(is.na(last_match) ~ last_match_original,
                                               TRUE ~ last_match),
                combined = as.numeric(paste0(first_match,last_match))) %>%
  dplyr::summarise(sum(combined))

# My answer is: 55680 but this is not correct. It is too low. I have been unable to find the error.
