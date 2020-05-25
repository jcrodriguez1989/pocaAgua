#' Proof of concept for a prediction problem.
#'
#' Given a data.frame with observations as rows and variables as columns, and
#' the specification of the response variable, this function tries to fit
#' various prediction models for this data. The metrics of the best-calculated
#' model, as well as a prediction test interface will be provided through a web
#' page.
#'
#' @param input_data character or data.frame :
#'   * A character of length one, containing the file path of the table to load.
#'   * A data.frame with the table.
#' @param response_variable character : A character of length one, indicating
#'   the name of the column of `input_data` that will be predicted.
#' @param predictors character : Indicating the names of the columns of
#'   `input_data` that will be used to predict. If not provided, it will use
#'   all columns, except `response_variable`.
#' @param max_runtime_secs numeric : Maximum time that it will be training
#'   different models.
#' @param model_file character : A character of length one, containing the file
#'   path of a previously trained model (downloaded from this app). If
#'   provided, it will skip the training; it will just build the app.
#' @param ... Other arguments passed to [h2o::h2o.automl()].
#'
#' @inheritDotParams h2o::h2o.automl
#'
#' @seealso [h2o::h2o.automl()]
#'
#' @examples
#' \dontrun{
#' poca_agua(iris, "Species")
#'
#' poca_agua("my_iris_file.csv", "Species")
#' }
#'
#' @importFrom h2o h2o.automl
#' @importFrom shiny runApp
#'
#' @export
#'
poca_agua <- function(input_data, response_variable, predictors = NA,
                      max_runtime_secs = 60, model_file = NA, ...) {
  # If it is a file path, then try to open it.
  if (is.character(input_data)) {
    input_data <- read_file_data(input_data)
  }
  # If it was a table, or could not be loaded, then fail.
  if (!is.matrix(input_data) && !is.data.frame(input_data)) {
    stop("`input_data` is not a matrix or could not be loaded from file.")
  }
  # response_variable must be a column name present in input_data.
  if (!response_variable %in% colnames(input_data)) {
    stop("response_variable must be a column name of input_data.")
  }
  # If predictors was not provided, use all columns, except response_variable.
  if (is.na(predictors)) {
    predictors <- setdiff(colnames(input_data), response_variable)
  }
  predictors <- intersect(predictors, colnames(input_data))
  # There must be at least one predictor variable.
  if (length(predictors) == 0) {
    stop("predictors variable not provided.")
  }
  # If model was not provided then train models.
  if (is.na(model_file)) {
    # Create a temporary dir for the model.
    poc_dir <- paste0(tempdir(), "/myPocaAgua/")
    # Train the models.
    model_file <- train_model(
      input_data, response_variable, predictors, max_runtime_secs, poc_dir, ...
    )
  }
  model_file <- normalizePath(model_file)

  # Build and show the shiny app.
  app <- build_app(input_data, response_variable, predictors, model_file)
  runApp(app)
}
