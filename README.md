# lebanon
Shiny app designed to interactively visualize data on refugee flows, conflict, and demography at the subnational level in Lebanon collected for my dissertation

This repository contains a number of files and folders needed to run the Shiny app

- The ui.R and server.R are the two primary files needed to run the shiny app locally 
- The app.css and helpers.R allow for further customization of the app if other users prefer to change the style, look, and feel 
(note the the ui.R file must be altered to exclude the shinytheme('united) that would otherwise override changes to those files)
- Data: this folder includes the necessary refugee, conflict, and demographic data as well as a lebanon shapefile
- www: this folder includes additional stylistic elements such as an image file used in the home page of the app (note the bootsrap.css is
also not used in the app for the same reason as mentioned above)

Note: The file paths may need to be changed when running the app to their appropriate downloaded locations on your local drives.

Enjoy!
