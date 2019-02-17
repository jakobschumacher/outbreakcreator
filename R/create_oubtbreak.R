#' Create an outbreak
#'
#' @section Background:
#' Infect the world and create an outbreak - or at least a dataset of an outbreak
#'
#' This dataset simulates an outbreak of varicella in german centers for foreigners. It is loosely based on
#' the situation in 2015, when the numbers of foreigners seeking asylum exeded the available places
#' in the center for foreigners. Varicella was the most frequent disease in these centers at that time.
#' comparable with kindergartens and other shelters.
#'
#' The data is useful to compute incidence rates.
#' @docType function
#'
#' @rdname create_outbreak
#'
#' @author Jakob Schumacher (\email{jakob.schumacher@web.de})
#'
#' @references
#'
#'
#' @examples
#'

create_outbreak <- function(size = 500,
                            startDate = "2015-01-01",
                            endDate = "2015-12-31",
                            dateOfInfection = "2015-05-06",
                            centerNames = c("Oranienburger Str", "Buchholzerstr", "Platz der Luftbruecke", "Bizetstr"),
                            meanStayAtCenter = 50,
                            disease = "varicella",
                            diseaseIncubationPeriod = 14
                            ) {

  startDate = as.numeric(as.Date(startDate, origin = "1970-01-01")) # get date into the right format
  endDate = as.numeric(as.Date(endDate, origin = "1970-01-01")) # get date into the right format
  dateOfInfection = as.Date(dateOfInfection, origin = "1970-01-01") # get date into the right format

  # Intermediate variables
  affectedCenter = sample(centerNames, size = 1)
  totalLength = endDate - startDate
  probCenters <- runif(length(centerNames), 0, 1)

  # Create random personal data
  suppressMessages(library(randomNames)) # load name generator package
  data <- data.frame(name = randomNames::randomNames(size, return.complete.data = TRUE))
  data$age = round(runif(size, min = 0, max = 35),0)

  # Add centers with arrival and leave date
  data$center1 = factor(sample(centerNames, size = size, replace = TRUE, prob = probCenters/sum(probCenters)))
  data$arrival1 = ((rgamma(1:size, 3, scale = totalLength/15) + sample(c(totalLength/5, -totalLength/5))) + startDate)
  data$leave1 = data$arrival1 + sample(rnorm(size, meanStayAtCenter, sd = meanStayAtCenter/2), size = size, replace = TRUE)
  data$center2 = factor(sample(centerNames, size = size, replace = TRUE, prob = probCenters/sum(probCenters)))
  data$arrival2 = data$leave1 + sample(5:15, size = size, replace = TRUE)
  data$leave2 = data$arrival2 + sample(rnorm(size, meanStayAtCenter, sd = meanStayAtCenter/2), size = size, replace = TRUE)

  # Generate background cases
  data$onset = sample(c(startDate:endDate, rep(NA,totalLength*9)), size = size, replace = TRUE)

  # Generate an outbreak in one center
  data$onset = ifelse(data$center1 == affectedCenter &
                        ((data$arrival1 < dateOfInfection &
                            data$leave1 > dateOfInfection) |
                           (data$arrival2 < dateOfInfection &
                              data$leave2 > dateOfInfection)),
                      (rgamma(1:5000, 2, scale = 20) + dateOfInfection + diseaseIncubationPeriod),
                      data$onset)

  data$disease <- disease

  # Cleaning up
  data$onset <- as.Date(data$onset, origin = "1970-01-01")
  data$arrival1 <- as.Date(data$arrival1, origin = "1970-01-01")
  data$leave1 <- as.Date(data$leave1, origin = "1970-01-01")
  data$arrival2 <- as.Date(data$arrival2, origin = "1970-01-01")
  data$leave2 <- as.Date(data$leave2, origin = "1970-01-01")

  names(data)[names(data) == 'name.first_name'] <- 'firstname'
  names(data)[names(data) == 'name.last_name'] <- 'lastname'
  names(data)[names(data) == 'name.ethnicity'] <- 'ethnicity'
  names(data)[names(data) == 'name.gender'] <- 'sex'

  data$sex = factor(data$sex, levels = c(0,1), labels = c("male", "female"))
  data$ethnicity = factor(data$ethnicity, levels = seq(1:6), labels = c("native-american", "asian", "african", "hispanic", "caucasian", "arabic"))

  data
}


data <- create_outbreak()
