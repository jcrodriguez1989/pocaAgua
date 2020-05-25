#' @importFrom dplyr mutate_if
#' @importFrom h2o h2o.init as.h2o h2o.automl h2o.saveMojo h2o.import_mojo
#' @importFrom tools file_ext
#'
train_model <- function(input_data, response_variable, predictors,
                        max_runtime_secs, poc_dir, ...) {
  # Start the H2O local server. We should inform the user that should close it.
  h2o.init()
  # Convert character variables to factor.
  input_data <- input_data %>% mutate_if(is.character, as.factor)
  # Send data to the server.
  h2o_data <- as.h2o(input_data[, c(response_variable, predictors)])
  # Train the models.
  aml <- h2o.automl(
    training_frame = h2o_data, y = response_variable,
    max_runtime_secs = max_runtime_secs, ...
  )
  print(aml@leaderboard)
  # Get the best-trained model.
  model <- aml@leader
  dir.create(poc_dir, showWarnings = FALSE)
  # Save the model in the temporary dir.
  mojo_file <- h2o.saveMojo(model, path = poc_dir, force = TRUE)
  new_mojo_file <- paste0(dirname(mojo_file), "/model.", file_ext(mojo_file))
  file.rename(mojo_file, new_mojo_file)
  new_mojo_file
}
