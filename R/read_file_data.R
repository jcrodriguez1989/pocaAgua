#' @importFrom readr read_csv read_tsv
#' @importFrom tools file_ext
#'
read_file_data <- function(file_name) {
  # Right now, we handle only csv and tsv files, we should extend to more
  # formats.
  switch(file_ext(file_name),
    csv = read_csv(file_name),
    tsv = read_tsv(file_name)
  )
}
