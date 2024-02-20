# libraries ----
library(googledrive)
library(progress)
# downloading processed files ----
project_files <- drive_ls(path = "tm_project")$name
project_files <- project_files[regexpr(pattern = "^(tagged).*sqlite$", text = project_files) > 0]

pb <- progress_bar$new(
  format = " downloading :what [:bar] :percent eta: :eta elapsed :elapsed",
  clear = FALSE, total = length(project_files), width = 100)

for (sqlite_file in project_files) {
  pb$tick(tokens = list(what = sqlite_file))
  # download file from Google Drive 
  googledrive::drive_download(file = file.path("tm_project", sqlite_file), path = paste0("data/", sqlite_file), overwrite = TRUE)
}
