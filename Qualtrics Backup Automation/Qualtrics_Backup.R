library(httr)
library(zip)
library(dplyr)
# Set your Qualtrics API key, survey ID, and data center ID
api_key <- "KxJrC2XjvEK9toaoHPik4mu5tJ3IJZUFpq6pju81"
data_center_id <- "sjc1"

#Get list of survey IDs - this is only for generating the survey id list
qualtRics::qualtrics_api_credentials(api_key = api_key, 
                                     base_url = "sjc1.qualtrics.com")

surveys <- qualtRics::all_surveys()
survey_updates <- surveys %>% filter(lastModified > (Sys.Date() - 7)) 
survey_lists <- as.list(survey_updates$id)

# Create a folder to store QSF files
folder_path <- "C:/Users/boost/Documents/Qualtrics_Survey_Backup"
dir.create(folder_path, showWarnings = FALSE)

#making a loop to repeat the processes 
for (survey_id in survey_lists) {

# Set the API endpoint for exporting survey definition
endpoint <- paste0("https://", data_center_id, ".qualtrics.com/API/v3/surveys/", survey_id)

# Set headers with the API key
headers <- c(Authorization = paste('X-API-TOKEN', api_key))

# Make the API request to export the survey definition
response <- httr::GET(url = endpoint, httr::add_headers('X-API-TOKEN' = api_key))

# Check if the request was successful (status code 200)
if (httr::status_code(response) == 200) {
  # Save the exported survey definition to a .qsf file
  survey_definition <- httr::content(response, "text")
  file_name <- paste0(folder_path, "/survey_", survey_id, ".qsf")
  writeLines(survey_definition, file_name)
  
  print(paste("Survey definition for ID", survey_id, "updated in back-up folder."))
} else {
  # Print an error message with details if the request was not successful
  error_message <- httr::content(response, "text")
  print(paste("Error exporting survey definition for ID", survey_id, ". Status code:", httr::status_code(response)))
  print(error_message)
}
}

#save survey key
write.csv(surveys, file = paste0(folder_path, "/Survey_Key.csv"))


