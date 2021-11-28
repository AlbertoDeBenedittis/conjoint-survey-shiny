# Needed libraries
library("googlesheets4")
library("DT")
library(gargle)
library(googledrive)

# With the following few lines of code we want to: 
 # firstly, connected R with my Google Drive 
# secondly, to create an empty spreadsheet 

options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = "C:/Users/alber/Desktop/UniTn/Data Science/Third Semester/Laboraotry of Business and Customer analytics/Project"
)

googledrive::drive_auth()
googlesheets4::gs4_auth()
# Run once on set up
Sur_Data <-  googlesheets4::gs4_create(name = "suvey_results", 
                          # Create a sheet called main for all data to 
                          # go to the same place
                          sheets = c("Demography", 'Survey_Answers'))

# Creating the sheet ID
sheet_id <- drive_get("suvey_results")$id


# Defining the headers for our survey. 
headers_d <- as.data.frame(cbind("Age", "Sex", "Education", "Nationality", 'Marital Status'))
headers_s <-  as.data.frame(cbind('Choice 1', 'Choice 2', 'Choice 3', 'Choice 4', 
                                    'Choice 5', 'Choice 6', 'Choice 7', 'Choice 8',
                                    'Choice 9', 'Choice 10', 'Choice 11', 'Choice 12'))


googlesheets4::sheet_write(headers_d, ss =sheet_id, sheet = 'Demography')
googlesheets4::sheet_write(headers_s, ss =Sur_Data, sheet = 'Survey_Answers')


