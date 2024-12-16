## code to prepare `mockdata` dataset goes here

USERNAME <- Sys.getenv("USERNAME")
company_name <- "<Enter name>"
project_name <- "<Enter name>"
folder_name <- "query28-11-2024"

path_to_data <- file.path(
  "C:\\Users", USERNAME, company_name,
  paste0(project_name, " - Documents"),
  "Project", "Study1", "Development", "Data",
  "Raw", "SQLdata", folder_name)

file_name_json <- "mockdata.json"

# read in raw json
mockdata <- jsonlite::fromJSON(txt = file.path(path_to_data, file_name_json))

####
# create random user ids -------
####

# read existing ids
user_ids <- mockdata |>
  dplyr::pull(user_pseudo_id) |>
  unique()

# random ids with length of n
create_random_id <- function(n){
  stopifnot(n>5)

  random_letters <- paste0(sample(letters, n-2, replace = TRUE), collapse = "")

  random_numbers <- sample(as.character(0:9), 2)

  paste0(random_numbers[1], random_letters, random_numbers[2])
}

set.seed(123)

id_lookup <- data.frame(
  user_pseudo_id = user_ids,
  scrambled_id = sapply(rep(10, length(user_ids)), create_random_id)
)

# replace pseudo_ids with scrambled ids
mockdata <- mockdata |>
  dplyr::left_join(id_lookup, by = "user_pseudo_id") |>
  dplyr::mutate(user_pseudo_id = scrambled_id) |>
  dplyr::select(-scrambled_id) |>
  dplyr::select(
    event_date, event_timestamp, event_name, event_params,
    user_pseudo_id, user_properties
  )


## Save dataset to data folder

usethis::use_data(mockdata, overwrite = TRUE)
