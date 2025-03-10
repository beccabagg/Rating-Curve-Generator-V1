packages <- c("ggplot2", "readxl", "shiny", "segmented")
for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

ui <- fluidPage(
  titlePanel("Rating Curve Generator"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload .xlsx File", accept = c(".xlsx")),
      numericInput("zero_flow_stage", "Stage at Zero Flow (a)", value = 0, step = 0.01),
      numericInput("max_discharge", "Maximum Discharge (ft^3/s)", value = 3000, min = 0.01),
      sliderInput("segments", "Number of Segments", 
                  min = 1, max = 5, value = 2, step = 1),
      actionButton("process", "Process Data")
    ),
    
    mainPanel(
      wellPanel(
        h3("Rating Curve Generator"),
        p("This app creates discharge rating curves from stage and discharge measurements."),
        tags$ul(
          tags$li("Upload an Excel file containing 'Gage height (ft)' and 'Discharge (ft^3/s)' columns from Aquarius (delete any extra rows prior to import"),
          tags$li("Set the number of segments for the Segmented Power Law model"),
          tags$li("Horizontal red lines show where segments connect")
        ),
        p("The segmented model provides flexibility to handle different behaviors across the rating curve's range."),
        p("Note: This app is not yet configured to compute offset values."),
        p("For questions contact Rebecca Baggott rbaggott@usgs.gov.")
      ),
      conditionalPanel(
        condition = "input.process > 0 && output.hasBreakpoints == true",
        wellPanel(
          h4("Segment Breakpoints:"),
          tableOutput("breakpointsTable")
        )
      ),
      tableOutput("ratingTable"),
      plotOutput("ratingPlot"),
      h4("Model Fit Statistics:"),
      verbatimTextOutput("modelStats"),
      wellPanel(
        h4("Understanding the Statistics:"),
        p(strong("R-squared:"), "Indicates the proportion of variance in discharge explained by the model."),
        p(strong("Adjusted R-squared:"), "Adjusted for the number of predictors in the model."),
        p(strong("Root Mean Square Error (RMSE):"), "Measures the average deviation between observed and predicted values."),
        p(strong("Number of observations:"), "The total number of data points used in creating the model.")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Create dynamic UI inputs for segment offsets
  output$offsetInputs <- renderUI({
    return(NULL)
  })

  # Store offsets when apply_offsets button is clicked
  observeEvent(input$apply_offsets, {
    # No longer needed
  })
  
  # Add output to indicate whether breakpoints exist
  output$hasBreakpoints <- reactive({
    req(data_reactive())
    model_data <- data_reactive()
    !is.null(model_data$model$psi)
  })
  
  # Make breakpoints available as a table
  output$breakpointsTable <- renderTable({
    req(data_reactive())
    model_data <- data_reactive()
    
    if(is.null(model_data$model$psi)) {
      return(NULL)
    }
    
    segmented_model <- model_data$model
    zero_flow_stage <- model_data$zero_flow_stage
    
    # Get breakpoints in original scale
    breakpoints <- 10^segmented_model$psi[,2] - zero_flow_stage
    
    # Calculate discharge at each breakpoint
    breakpoint_discharges <- predict_discharge(model_data, breakpoints)
    
    # Prepare table with segment number, breakpoint stage, and discharge
    result_df <- data.frame(
      Segment = paste("Segment", 1:(length(breakpoints) + 1)),
      Stage_Range = c(
        paste0("< ", round(breakpoints[1], 2), " ft"),
        paste0(lapply(1:(length(breakpoints)-1), function(i) {
          paste0(round(breakpoints[i], 2), " to ", round(breakpoints[i+1], 2), " ft")
        })),
        paste0("> ", round(breakpoints[length(breakpoints)], 2), " ft")
      ),
      Breakpoint_Stage = c(NA, round(breakpoints, 2)),
      Discharge_at_Breakpoint = c(NA, round(breakpoint_discharges, 2))
    )
    
    return(result_df)
  })

  data_reactive <- eventReactive(input$process, {
    req(input$file)
    
    field_measurements_export <- read_excel(input$file$datapath)
    gage_height <- field_measurements_export$`Gage height (ft)`
    discharge <- field_measurements_export$`Discharge (ft^3/s)`
    
    data <- data.frame(gage_height, discharge)
    
    # Apply stage at zero flow (datum correction)
    zero_flow_stage <- input$zero_flow_stage
    data$adjusted_stage <- data$gage_height + zero_flow_stage
    
    # Log transform data
    data$log_adjusted_stage <- log10(data$adjusted_stage)
    data$log_discharge <- log10(data$discharge)
    
    # Always use segmented model
    initial_model <- lm(log_discharge ~ log_adjusted_stage, data = data)
    
    # Try to fit segmented model
    tryCatch({
      segments_count <- input$segments
      if (segments_count <= 1) {
        # Use linear model if only 1 segment requested
        model <- initial_model
        
        result <- list(
          data = data,
          model = model,
          zero_flow_stage = zero_flow_stage
        )
      } else {
        # Starting points for segmentation based on quantiles
        psi <- quantile(data$log_adjusted_stage, probs = seq(0.25, 0.75, length.out = segments_count-1))
        segmented_model <- segmented(initial_model, seg.Z = ~log_adjusted_stage, psi = psi)
        
        result <- list(
          data = data,
          model = segmented_model,
          zero_flow_stage = zero_flow_stage
        )
      }
    }, error = function(e) {
      # Fallback to linear model if segmentation fails
      model <- initial_model
      
      result <<- list(
        data = data,
        model = model,
        zero_flow_stage = zero_flow_stage
      )
    })
    
    return(result)
  })
  
  # Simplified predict_discharge function (removed offset handling)
  predict_discharge <- function(model_data, stage_values) {
    zero_flow_stage <- model_data$zero_flow_stage
    adjusted_stage <- stage_values + zero_flow_stage
    log_adjusted_stage <- log10(adjusted_stage)
    
    # Get log discharge predictions
    log_discharge <- predict(model_data$model, newdata = data.frame(log_adjusted_stage = log_adjusted_stage))
    discharge <- 10^(log_discharge)
    return(discharge)
  }
  
  rating_table_reactive <- reactive({
    req(data_reactive())
    
    model_data <- data_reactive()
    max_discharge <- input$max_discharge
    zero_flow_stage <- model_data$zero_flow_stage
    
    # Generate a sequence of gage heights based on the data range
    data <- model_data$data
    min_gage <- min(data$gage_height)
    max_gage <- max(data$gage_height) * 1.2  # Extend slightly beyond observed data
    
    # Add points to cover zero flow and max discharge
    # First, estimate a very low stage that would give near-zero discharge
    very_low_stage <- max(0.01, min_gage * 0.5)
    
    # Then, estimate a high stage that would exceed max discharge
    # We'll use a simple extrapolation approach
    highest_observed_discharge <- max(data$discharge)
    highest_observed_stage <- max(data$gage_height)
    extrapolation_factor <- (max_discharge / highest_observed_discharge) ^ 0.5
    estimated_max_stage <- highest_observed_stage * extrapolation_factor
    
    # Create non-uniform spacing with more points at lower values
    log_spacing <- exp(seq(log(very_low_stage + 0.001), log(max_gage + 0.001), length.out = 30)) - 0.001
    
    # Add extra points at both low and high ends
    low_end_points <- seq(very_low_stage, min_gage + (max_gage - min_gage) * 0.2, length.out = 10)
    high_end_points <- seq(max_gage, estimated_max_stage, length.out = 10)
    
    # Combine and sort all points, then remove duplicates
    gage_height_table <- sort(unique(c(low_end_points, log_spacing, high_end_points)))
    
    # Calculate discharge using the appropriate model
    predicted_discharge_table <- predict_discharge(model_data, gage_height_table)
    
    # Create the result dataframe with all points
    result_df <- data.frame(
      Gage_Height_ft = round(gage_height_table, 2),
      Discharge_ft3s = round(predicted_discharge_table, 2)
    )
    
    # Filter to only show from near-zero to max discharge
    result_df <- result_df[result_df$Discharge_ft3s <= max_discharge, ]
    result_df <- result_df[result_df$Discharge_ft3s >= 0.01, ]  # Minimum discharge of 0.01
    
    # Sort by gage height
    result_df <- result_df[order(result_df$Gage_Height_ft), ]
    
    return(result_df)
  })
  
  output$ratingTable <- renderTable({
    rating_table_reactive()
  })
  
  output$ratingPlot <- renderPlot({
    req(data_reactive())
    
    model_data <- data_reactive()
    data <- model_data$data
    max_discharge <- input$max_discharge
    
    # Generate a sequence of gage heights based on the data range
    min_gage <- min(data$gage_height)
    max_gage <- max(data$gage_height) * 1.2  # Extend slightly beyond observed data
    
    # Create non-uniform spacing with more points at lower values
    # Use logarithmic spacing to concentrate points at the low end
    log_spacing <- exp(seq(log(min_gage + 0.001), log(max_gage + 0.001), length.out = 200)) - 0.001
    
    # Add extra points at the low end for smoother curve representation
    low_end_points <- seq(min_gage, min_gage + (max_gage - min_gage) * 0.2, length.out = 50)
    
    # Combine and sort all points, then remove duplicates
    gage_height_seq <- sort(unique(c(low_end_points, log_spacing)))
    
    predicted_discharge <- predict_discharge(model_data, gage_height_seq)
    
    # Filter out values above max_discharge
    valid_indices <- which(predicted_discharge <= max_discharge)
    predicted_data <- data.frame(
      gage_height = gage_height_seq[valid_indices], 
      discharge = predicted_discharge[valid_indices]
    )
    
    p <- ggplot(data, aes(x = discharge, y = gage_height)) +
      geom_point(size = 3) +
      geom_line(data = predicted_data, aes(x = discharge, y = gage_height), color = "blue", linewidth = 1) +
      scale_x_log10(limits = c(0.01, max_discharge)) +
      scale_y_log10() +
      labs(title = "Rating Curve", x = "Discharge (ft^3/s)", y = "Gage Height (ft)") +
      theme_minimal()
    
    # Add segment breakpoint lines with labels
    segmented_model <- model_data$model
    zero_flow_stage <- model_data$zero_flow_stage
    
    # Get breakpoints (in original gage height units)
    if (!is.null(segmented_model$psi)) {
      breakpoints <- 10^segmented_model$psi[,2] - zero_flow_stage
      
      # For each breakpoint, add a horizontal line and a label
      for (bp in breakpoints) {
        # Calculate the discharge at this breakpoint for label placement
        bp_discharge <- predict_discharge(model_data, bp)[1]
        
        # Add horizontal line at breakpoint stage height
        p <- p + geom_hline(yintercept = bp, 
                           color = "red", linetype = "dashed", alpha = 0.7) +
                geom_point(data = data.frame(discharge = min(data$discharge), gage_height = bp),
                          color = "red", size = 4, shape = 4) +
                # Add label showing stage height value
                annotate("text", x = bp_discharge, y = bp * 1.05, 
                        label = paste0(round(bp, 2), " ft"), 
                        color = "red", size = 3, fontface = "bold")
      }
      
      # Add a legend for the breakpoints
      p <- p + annotate("text", x = min(data$discharge) * 2, y = max(data$gage_height),
                       label = "Segment Breakpoints", color = "red")
    }
    
    p
  })
  
  # Update model stats to reflect only the model fit (no offsets)
  output$modelStats <- renderText({
    req(data_reactive())
    model_data <- data_reactive()
    data <- model_data$data
    
    # For segmented models, calculate manually
    model <- model_data$model
    fitted_values <- fitted(model)
    observed_values <- data$log_discharge
    
    residuals <- observed_values - fitted_values
    SST <- sum((observed_values - mean(observed_values))^2)
    SSE <- sum(residuals^2)
    r_squared <- 1 - (SSE/SST)
    
    # Adjusted R-squared
    n <- length(observed_values)
    p <- length(coef(model))
    adj_r_squared <- 1 - (1-r_squared)*((n-1)/(n-p-1))
    
    rmse <- sqrt(mean(residuals^2))
    
    segments_text <- if(is.null(model$psi)) "Single segment" else paste(nrow(model$psi) + 1, "segments")
    
    paste0("Model: ", segments_text, " Power Law",
           "\nR-squared: ", round(r_squared, 4), 
           "\nAdjusted R-squared: ", round(adj_r_squared, 4),
           "\nRoot Mean Square Error (log scale): ", round(rmse, 4),
           "\nNumber of observations: ", nrow(data))
  })
}

shinyApp(ui = ui, server = server)