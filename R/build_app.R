#' @importFrom dplyr select pull `%>%`
#' @importFrom h2o h2o.init h2o.import_mojo h2o.varimp h2o.varimp_plot
#' @importFrom purrr map
#' @importFrom shinythemes shinytheme
#' @importFrom stats predict
#'
#' @import shiny
#'
build_app <- function(input_data, response_variable, predictors, model_file) {
  # Return a shiny app with ui and server specified.
  shinyApp(
    ui = build_ui(input_data, response_variable, predictors),
    server = build_server(input_data, response_variable, predictors, model_file)
  )
}

build_ui <- function(input_data, response_variable, predictors) {
  fluidPage(
    # Default pocaAgua theme.
    theme = shinytheme("darkly"),
    titlePanel(tagList(
      paste0("PoC - response variable: ", response_variable),
      downloadButton(outputId = "save_model", label = "Save Model")
    )),
    sidebarLayout(
      sidebarPanel(
        # For each predictor, create its shiny input.
        map(predictors, function(predictor) {
          input_data %>%
            select(!!predictor) %>%
            get_ui_object()
        }),
        # Add a button, to reduce reactivity.
        actionButton(inputId = "predict_button", label = "Predict!")
      ),
      mainPanel(
        checkboxInput(
          inputId = "model_summaries",
          label = "Show model summary",
          value = TRUE
        ),
        # Hide or show model summary info.
        conditionalPanel(
          "input.model_summaries",
          verbatimTextOutput(outputId = "model_summary"),
          # Hide variable importance plot, if it is not available for the model.
          conditionalPanel(
            "output.has_var_imp",
            plotOutput(outputId = "var_importance")
          )
        ),
        htmlOutput(outputId = "prediction")
      )
    )
  )
}

get_ui_object <- function(column) {
  # Depending on the class of the variable, it will return a specific shiny
  # input.
  switch(class(pull(column)),
    Date = dateInput(
      inputId = colnames(column),
      label = colnames(column)
    ),
    character = selectInput(
      inputId = colnames(column),
      label = colnames(column),
      choices = unique(pull(column))
    ),
    factor = selectInput(
      inputId = colnames(column),
      label = colnames(column),
      choices = unique(pull(column))
    ),
    numeric = numericInput(
      inputId = colnames(column),
      label = colnames(column),
      value = mean(pull(column), na.rm = TRUE)
    ),
    # For testing purpose, once I know all posibilities, I should remove this!
    browser()
  )
}

build_server <- function(input_data, response_variable, predictors, model_file) {
  # Start the H2O local server, and load the model.
  h2o.init()
  model <- h2o.import_mojo(model_file)
  function(input, output, session) {
    # Hide or show variable importance plot, if the model has it.
    has_var_imp <- suppressWarnings(nrow(h2o.varimp(model)) > 0)
    output$has_var_imp <- reactive(has_var_imp)
    outputOptions(output, "has_var_imp", suspendWhenHidden = FALSE)
    if (has_var_imp) {
      output$var_importance <- renderPlot(h2o.varimp_plot(model))
    }
    # Show model's summary info.
    output$model_summary <- renderText(get_metrics(model))
    # Get each provided input, and predict.
    observeEvent(input$predict_button, {
      # Get inputs, and copy them as a one-row data.frame to the H2O cluster.
      values <- map(predictors, function(predictor) {
        input[[predictor]]
      })
      names(values) <- predictors
      values <- as.data.frame(values)
      h2o_data <- as.h2o(values)
      # Get predictions.
      prediction <- predict(model, h2o_data)
      # If it was a classifier, it will return more data than the prediction.
      if (!is.null(dim(prediction))) {
        # So, get just the prediction.
        prediction <- prediction$predict
      }
      # To show the user that the prediction happened (because some times the
      # value does not change).
      showNotification("Predicted", duration = 1)
      # Show the prediction.
      output$prediction <- renderUI(h3(
        as.vector(prediction),
        style = "text-align:center"
      ))
    })
    # Model download handler.
    output$save_model <- downloadHandler(
      filename = function() "myPocaAguaModel.zip",
      content = function(con) file.copy(model_file, con)
    )
  }
}

get_metrics <- function(model) {
  # Get model's summary info as text.
  metrics <- unlist(model@model$training_metrics@metrics[c(
    "MSE", "RMSE", "mae", "rmsle", "mean_residual_deviance", "r2", "logloss",
    "mean_per_class_error"
  )])
  paste0(
    model@model$original_model_full_name,
    " metrics:\n",
    paste(names(metrics), round(unlist(metrics), 2), sep = ": ", collapse = "\n")
  )
}
