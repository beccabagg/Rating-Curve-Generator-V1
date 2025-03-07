# Shiny App for Rating Curve Analysis

This project is a Shiny application that processes .xlsx files containing field measurement data to generate a rating curve based on gage height and discharge values using a segmented power law approach.

## Project Structure

- **app.R**: The main code for the Shiny app. It includes UI components for file upload, parameter settings, and output displays for the rating table and curve plot. The app processes the uploaded .xlsx file, performs necessary calculations, fits a segmented power law model, and generates the outputs.
  
- **data/field-measurements-export.xlsx**: A sample data file used by the app. It contains the columns "Gage height (ft)" and "Discharge (ft^3/s)" which are required for processing.

- **www/**: This directory can be used to store any additional resources such as CSS or JavaScript files for enhancing the app's UI.

## Features

- Upload and process field measurement data from .xlsx files
- Specify the stage at zero flow (datum correction)
- Define maximum discharge for rating table and plot display
- Set the number of segments for the segmented power law model
- View breakpoints between segments with corresponding discharge values
- Display rating table with stage-discharge pairs
- Visualize the rating curve with observed points and fitted model
- Review model fit statistics including R-squared and RMSE

## Instructions to Run the App

1. Ensure you have R and RStudio installed on your machine.
2. Install the required packages if you haven't already:

   ```R
   install.packages(c("shiny", "ggplot2", "readxl", "segmented"))
   ```

3. Open the `app.R` file in RStudio.
4. Run the app by clicking the "Run App" button in RStudio or by executing the following command in the R console:

   ```R
   shiny::runApp()
   ```

5. Use the app:
   - Upload your .xlsx file containing "Gage height (ft)" and "Discharge (ft^3/s)" columns
   - Set the stage at zero flow value (default is 0)
   - Specify the maximum discharge for display purposes
   - Choose the number of segments for your model (1-5)
   - Click "Process Data" to generate the rating curve and table

## Dependencies

- R (version 4.0 or higher)
- Shiny
- ggplot2
- readxl
- segmented

## Understanding the Model

The app uses a segmented power law model to fit the rating curve. This approach provides flexibility to handle different behaviors across the curve's range by allowing the relationship between stage and discharge to vary at different breakpoints.

The output includes:
- A table of breakpoints where segments connect
- A rating table with stage-discharge pairs
- A plot showing observed data points, the fitted curve, and segment breakpoints
- Model fit statistics including R-squared and RMSE
