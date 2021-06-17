# Function for grouping CHOIR bodymap segments to a region 
#
# Author: Eric Cramer <emcramer@stanford.edu>
#
# Description: Groups individual segments of the CHOIR bodymap into larger regions for widespread area analysis.
#
# Arguments:
#   - mapping = a named list with a name of a region equal to a vector of numbers which are the bodymap segments
#   - bodymap = a CHOIR bodymap as a comma separated string (e.g. "24,25,69,71")

segmentsToRegions <- function(bodymap=NULL, mapping="default") {
  # if bodymap is null, then print error and return
  if(is.null(bodymap)) {
    print("Please enter a bodymap as a csv string.")
    return()
  }
  # check the value of mapping, if 'default' assign to default mapping and not user defined
  if(mapping == "default"){
    mapping <- list(
      "Front of Head" = c(101, 102, 103, 104),
      "Back of Head" = c(201, 202, 203, 204),
      "Neck" = c(105, 106, 205, 206),
      "Chest and Abdomen" = c(108, 109, 116, 117),
      "Upper Back" = c(208, 209, 212, 213),
      "Lower Back" = c(218, 219),
      "Pelvic" = c(121, 122),
      "Right Shoulder and Arm" = c(107, 111, 113, 115, 119, 125, 210, 214, 216, 220, 226, 230),
      "Left Shoulder and Arm" = c(110, 112, 114, 118, 124, 128, 207, 211, 215, 217, 221, 227),
      "Right Hip, Buttocks, and Leg" = c(120, 126, 129, 131, 133, 135, 224, 225, 229, 232, 234, 236, 238),
      "Left Hip, Buttocks, and Leg" = c(123, 127, 130, 132, 134, 136, 222, 223, 228, 231, 233, 235, 237)
    )
  } else if(class(mapping) != "list" || is.null(mapping)) {
    print("Please enter a valid bodymap mapping scheme as a named list. See default for example.")
    return()
  }
  # split the csv string into a list of individual numbers correspondign to each segment
  bodymap_segments <- strsplit(bodymap, ",")[[1]]
  # map each of the segments of the bodymap to their corresponding regions
  helper <- function(s) {
    # helper function which matches a given segment to its sublist and returns the name of the sublist in the mapping
    return(names(mapping)[as.logical(unlist(lapply((lapply(mapping, is.element, s)), sum)))])
  }
  # get the set of bodymap regions which describe the bodymap
  bodymap_regions <- unique(unlist(lapply(bodymap_segments, helper)))
  return(bodymap_regions)
}

# helper functions for data cleaning
# function to concatenate the bodymap regions into a csv string
concat_regions <- function(segments) {
  regions_list <- lapply(segments, segmentsToRegions)
  return(unlist(lapply(regions_list, paste, collapse="|")))
}

# function to count bodymap segments
count_segments <- function(x) {
  n_listed <- lapply(strsplit(x, ","), length)
  return(unlist(n_listed))
}