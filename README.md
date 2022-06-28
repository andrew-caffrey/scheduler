# scheduler
The culinary schedule app that IF employees use to request a culinary validation request. The app is an R Shiny application that is interfaced with google sheets using the googlesheets4 and googledrive packages. The App acceses the [Google Sheet](https://docs.google.com/spreadsheets/d/1csfs8FjOVuRANGkj28F7FJqLAG2FFV3FA4qvZppgu5A/edit?usp=sharing) through a service account culinaryrequests@request-sheet-301118.iam.gserviceaccount.com. 

The Google Cloud Project is named **Request-Sheet** with the project number: **563839821516** and the project-ID: **request-sheet-301118**. As of right now Tyler Simons has been made an owner of the GCP project. 

The app is in a very stable state. The code only needs to be updated when a new project SKU is added to the culinary validation test suite. Further changes to the code could possibly get rid of this step; however, it only happens about 2-3 times a year so it would be a low yield task.

The app itself interacts with several different tabs within the sheet. 
1. Requests
* Where the results feed into
2. One offs
* Not recorded by the app but is a way to keep track of one off requests from various 
3. Budgets
* Hourly budgets given to each team are stored here. Each request costs some amount of time. If the amount of time in open/imcompleted requests exceeds the budget, the ADD button within the app is disabled
4. Logistics
* This is where the time and the amount of sample needed for each test is stored. The table would ideally be in tidy format, but user complaince for team members was low in this format and caused too many errors/typos. The columns that matter are the Time (hours) column and every column directly calling out a specific SKU name ex: chameleon or eschen

# These next two tables control the check boxes for each tier

5. IR Tests
* The Strategic Ingredients team (formorally known as IR) has their own sets of tests based on their prioritzations. STING team tests individual ingredients and have certain tests only available for those ingredients. 
* Group 1 ingredients include solanic or TSPC as examples. Those Group 1 will have the corresponding tier columns in the same table avaiable in the app. 
6. General Tests
* Everyone else that is not Strategic Ingredients (STING team) will be pulling data from this table
* The nomenclature is to label the tier for the product SKU. Within the code for the app itself, it is told to pull the values from that column and use those for the checkboxes



The app is an extremely simple form and only two real "errors" ever occur that are on the user end. The first common error is when the user does not hit the button at the top that says "press this button to make the app work". The other error is the add button is disabled, which means they are over budget. Real bugs that may appear is the time or product needed is not updating correctly or has NA, that means there is an invalid value in the Logistics table. 

