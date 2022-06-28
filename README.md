# scheduler
The culinary schedule app that IF employees use to request a culinary validation request. The app is an R Shiny application that is interfaced with google sheets using the googlesheets4 and googledrive packages. The App acceses the [Google Sheet](https://docs.google.com/spreadsheets/d/1csfs8FjOVuRANGkj28F7FJqLAG2FFV3FA4qvZppgu5A/edit?usp=sharing) through a service account culinaryrequests@request-sheet-301118.iam.gserviceaccount.com. 

The Google Cloud Project is named **Request-Sheet** with the project number: **563839821516** and the project-ID: **request-sheet-301118**. As of right now Tyler Simons has been made an owner of the GCP project. 

The app is in a very stable state. The code only needs to be updated when a new project SKU is added to the culinary validation test suite. Further changes to the code could possibly get rid of this step; however, it only happens about 2-3 times a year so it would be a low yield task.

The app itself interacts with several different tabs within the sheet. 
1. Requests
* Where the results feed into
