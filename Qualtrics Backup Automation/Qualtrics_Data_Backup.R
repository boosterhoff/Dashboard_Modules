library(zip)
library(dplyr)

# Set your Qualtrics API key, survey ID, and data center ID
api_key <- "KxJrC2XjvEK9toaoHPik4mu5tJ3IJZUFpq6pju81"
data_center_id <- "sjc1"

#Get list of survey IDs - this is only for generating the survey id list
qualtRics::qualtrics_api_credentials(api_key = api_key, 
                                     base_url = "sjc1.qualtrics.com")

surveys <- qualtRics::all_surveys()
survey_lists <- as.list(surveys$id)

# Create a folder to store QSF files
folder_path <- "C:/Users/boost/Documents/Qualtrics_Data_Backup"
dir.create(folder_path, showWarnings = FALSE)

#making a loop to repeat the processes 
for (survey_id in survey_lists) {

# Save the exported survey definition to a .sav file
  survey_definition <- qualtRics::fetch_survey(surveyID = survey_id, label=FALSE, convert = FALSE,
                                               unanswer_recode = 999, unanswer_recode_multi = 999)

  survey_definition <-  janitor::clean_names(survey_definition)
  file_name <- paste0(folder_path, "/data_", survey_id, ".sav")
  
  haven::write_sav(survey_definition, file_name) 
 
  print(paste("Survey data for ID", survey_id, "updated in back-up folder."))

}

surveys$id

#save survey key
write.csv(surveys, file = paste0(folder_path, "/Survey_Data_Key.csv"))


