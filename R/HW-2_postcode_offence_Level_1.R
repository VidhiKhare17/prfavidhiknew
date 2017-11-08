## Submitted By : Vidhi Khare (vidhik)

##QUESTION -

#' Edit the function mentioned in class so that it will accept any of the offence_descriptions found in Offence Level 1 and will accept a 2-element vector of postcodes.
#'
#' \code{<correlation_offence_postcode >} < This function is taking paramters i.e.crime_data(object of data.table), offence_description, postcodes as input>
#' @param crime_data A data.table object with the following columns:
#'     "date" (POSIXct), "suburb" (chr), "postcode" (chr), "offence_level_1" (chr),
#'     "offence_level_2" (chr), "offence_level_3" (chr), "offence_count" (num).
#' @param offence_description A character string of <one- element string character vector from offence_level_1 description. Example: ACTS INTENDED TO CAUSE INJURY>.
#' @param postcodes A two-element character vector. Each element is an SA postcode.
#' @export
#' @return  A ggplot object showing the correlation in offence count between the two input postcodes.
#' @examples
#' <one or two examples showing how to use the function>
correlation_offence_postcode <- function(crime_data, offence_description, postcodes) {
  require(data.table)
  require(ggplot2)
  library(readxl)

  # Error catching

  crime_data = setDT(read_excel("data/crime-statistics-2012-13.xlsx"))
  setnames(crime_data, c("date", "suburb", "postcode", "offence_level_1",
                     "offence_level_2", "offence_level_3", "offence_count"))

  offence_description = "PROPERTY DAMAGE AND ENVIRONMENTAL"
  postcodes=c(5159,5042)
  if (length(postcodes) !=2) {
    stop("Please enter two postcodes")
  }


expected_colnames <- c("date", "suburb", "postcode", "offence_level_1", "offence_level_2",
                        "offence_level_3", "offence_count")

 # datacolumns <- colnames(crime_data)

  if (!all.equal(expected_colnames, names(crime_data) )) {
    stop(paste("Input table columns need to match: ",
               paste(expected_colnames, collapse = ", ")))
  }




  # Check that the input suburbs and offence description exist in crime_data
  if (any(!postcodes %in% crime_data$postcode) |
      !offence_description %in% crime_data$offence_level_1 ) {
    stop("Record not found for postcodes and offence description in crime_data")
  }

  # Make a data table for plotting using data.table transformations
  # You will need to filter, summarise and group by
  # Expect cols: "date", "postcode", "total_offence_count"


  plot_data <- crime_data[(postcode == postcodes[1]| postcode == postcodes[2]) & offence_level_1 == offence_description,
                          .(total_offence_count = sum(offence_count)),
                         by = .(month(date),postcode)]

  # These lines will transform the plot_data structure to allow us to plot
  # correlations. Try them out
  plot_data[, postcode := plyr::mapvalues(postcode, postcodes, c("x", "y"))]

  plot_data <- dcast(plot_data, month ~ postcode, fun = sum,
                     fill = 0, value.var = "total_offence_count")

  # Generate the plot

  Postcode <- factor(postcodes[1])
  postcode_2 <- factor(postcodes[2])

  ggplot(plot_data, aes(x=month) , fill=c(x,y)) +
    geom_line(aes(y = plot_data$y, colour = Postcode))+
    geom_line(aes(y = plot_data$x, colour = postcode_2))+
    scale_x_continuous(breaks= c(1:12))+
    labs(x = "Timeline(Month of a Date)",
         y = "Offence_Count")


}
