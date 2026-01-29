library(googlesheets4)

gs4_deauth()

my_data <- read_sheet("1fMPQsyStKzdFJN1d68Mfcq9ZZEBDP1y-VfXOZZG-eD4")

my_data

library(googlesheets4)
library(dplyr)
library(purrr)
library(stringr)

gs4_deauth()


extract_hyperlink_data <- function(cell) {
  cell_unlisted <- unlist(cell)
  list(
    text = cell_unlisted[["formattedValue"]],
    hyperlink = if ("hyperlink" %in% names(cell_unlisted)) cell_unlisted[["hyperlink"]] else NA
  )
}

# df_test <- read_sheet("https://docs.google.com/spreadsheets/d/1oLj5YyBG2-ucvhhMUrl2K0Ko8jJkX4uvDChVfSfxT0o/edit#gid=0")
df_test <- read_sheet("1fMPQsyStKzdFJN1d68Mfcq9ZZEBDP1y-VfXOZZG-eD4")

df_test2 <- range_read_cells(ss = "1fMPQsyStKzdFJN1d68Mfcq9ZZEBDP1y-VfXOZZG-eD4",
                             sheet = NULL,
                             range = NULL,
                             cell_data = "full")
Org_URL_data <- df_test2 %>%
  filter(stringr::str_starts(loc, "D") & loc != "D1") %>%
  pull(cell) %>%
  purrr::map_df(., extract_hyperlink_data) %>%
  rename(Organisation = text, Org_URL = hyperlink)

Info_URL_data <- df_test2 %>%
  filter(stringr::str_starts(loc, "F") & loc != "F1") %>%
  pull(cell) %>%
  purrr::map_df(., extract_hyperlink_data) %>%
  rename(`Information/Links` = text, Info_URL = hyperlink)

df_test %>%
  left_join(Org_URL_data) %>%
  left_join(Info_URL_data) %>%
  View
