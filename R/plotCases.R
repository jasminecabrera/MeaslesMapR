#' Plot number of cases in region(s) by year.
#'
#' @param region A vector of region names. Valid regions include "Africa," "Americas," "Eastern Mediterranean," "Europe," "South East Asia," and "Western Pacific"
#' @param legend Whether a legend should be displayed (warning: typically illegible)
#'
#' @returns A plot displaying number of measles cases across all years in these regions.
#' @export
#'
#' @examples
#' plotCases("Eastern Mediterranean")

plotCases <- function(region, legend = F) {

  if (length(region) != 1) {
    stop("The `region` argument must be a single string, not a vector of multiple regions.")}

  # load data
  measles <- load_data()

  # validate region
  validateRegion(region, measles)

  # get top three countries
  max_point <- measles |>
    dplyr::filter(Region == region) |>
    dplyr::slice_max(`Total Measles per Million`, n = 3)

  # no legend
  if (legend == F) {

    # plot
    measles |>
      dplyr::filter(Region == region) |>
      ggplot2::ggplot(ggplot2::aes(x = Year,
                                   y = `Total Measles per Million`,
                                   color = Country)) +
      ggplot2::geom_line() + ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Measles Cases per Million People in ",
                                   paste(region, collapse = ", "))) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none",
                     panel.grid.minor = ggplot2::element_blank()) +
      ggrepel::geom_text_repel(
        data = max_point,
        ggplot2::aes(label = Country),
        show.legend = FALSE,
        nudge_x = -.25)}

  # w/ legend
  else {

    # plot
    measles |>
      dplyr::filter(Region == region) |>
      ggplot2::ggplot(ggplot2::aes(x = Year,
                                   y = `Total Measles per Million`,
                                   color = Country)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste0("Measles Cases per Million People in ",
                                   paste(region, collapse = ", "))) +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
      ggrepel::geom_text_repel(
        data = max_point,
        ggplot2::aes(label = Country),
        show.legend = FALSE,
        nudge_x = -.25)}}
