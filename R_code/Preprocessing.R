library(readxl)
library(readr)
library(anchors)
library(mice)



######################################################### Utility functions

#' Removes NA from a feature
#'
#' Takes a feature of a dataset and removes every NA value, returning only the the unique feature values
#' @param feature the feature to remove NA values from
#' @return the non-NA unique values of the input feature
#' @usage na.omit.unique(feature)
#' @export
na.omit.unique <- function(feature) {
  return (unique(na.omit(feature)))
}

#'
#' Renames a column of a dataset
#' 
#' Takes a dataset and a name, and renames the column with that name with a new one, returning the dataset itself
#' @param data the dataset containing the column to rename
#' @param name.old the old name for the column to rename
#' @param name.new the new name for the column
#' @usage rename(data, name.old, name.new)
#' @export
#'
rename <- function(data, name.old, name.new) {
  names(data)[names(data) == name.old] <- name.new
  return (data)
}



############################################################## Data Loading

list.files()

# Load dataset
data <- read_excel("Data/dataset.xlsx")
data <- data[!(is.na(data$approved) | data$approved==""), ]
data <- data[!( is.na(data$comfort_score) &
                  is.na(data$service_score) &
                  is.na(data$food_score) &
                  is.na(data$enjoyment_score) &
                  is.na(data$station_score) &
                  is.na(data$internet_connection_score) &
                  is.na(data$expendiency_score) &
                  is.na(data$final_vote) &
                  is.na(data$class) &
                  is.na(data$traveller_type) &
                  is.na(data$company)
), ]

