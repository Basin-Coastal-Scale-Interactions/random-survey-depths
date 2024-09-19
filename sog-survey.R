# Strait of Georgia survey random

# set.seed(21)
# As used for Nacho's Euros 2024 (soccer) picks for a pool. 21
#  was the number of times he barked during a five minute period when
#  his dog friend in the house behind ours was over in his yard.
#  No less/more random than anything else to design a survey.


#6 tows in a day
#2 random at 0 depth
#2 at 15
#2 are a choice of 30, 45, 60, without replacement.

#2nd option - 7 tows. Last one gets full.

##' Randomly assign depths to tows for Strait of Georgia Juvenile Salmon Survey
##'
##' Generate depths for six or seven tows. Two are at depth of 0 m, two are at 15
##' m, and two are selected from 30, 45, and 60 m (without replacement). If six
##' tows then randomly omit one of 30, 45, or 60; if seven tows then use all
##' three. Gets called by `sog_random_depths_days()` for multiple days.
##'
##' @param n number of tows, must be 6 or 7
##' @param depths_twice vector of depths to be selected twice; default is c(0, 15).
##' @param depths_once vector of depths to be selected once; default is c(30,
##'   45, 60).
##' @return Vector of length `n` with depth for each tow.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' sog_random_depths(n = 7)
##' }
sog_random_depths <- function(n = 6,
                              depths_twice = c(0, 15),
                              depths_once = c(30, 45, 60)){
  if(!(n %in% c(6, 7))){
    stop("n currently needs to be 6 or 7")
  }

  if(n == 6){
    depths_once <- sample(depths_once)[1:2]       # Just need two of them
  }

  sample(c(depths_twice,
           depths_twice,
           depths_once))
}

##' Randomly assign depths to tows for Strait of Georgia Juvenile Salmon Survey
##' for multiple days
##'
##' Calls `sog_random_depths()` for each day, and for 6 and 7 tows.
##'
##' @param days
##' @param ... Arguments passed onto `sog_random_depths()`, except for `n`
##' @return list object of two data frames, `plan_6` and `plan_7` with random
##'   daily depth assigments depending on whether 6 or 7 depths are required.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' sog_random_depths_days()
##' }
sog_random_depths_days <- function(days = 12,
                                   ...){
  plan_6 <- data.frame("day" = NA,
                           tow_1 = NA,
                           tow_2 = NA,
                           tow_3 = NA,
                           tow_4 = NA,
                           tow_5 = NA,
                           tow_6 = NA)
  plan_7 <- cbind(plan_6,
                  tow_7 = NA)

  for(i in 1:days){
    plan_6[i, ] <- c(i,
                     sog_random_depths(n = 6,
                                       ...))
    plan_7[i, ] <- c(i,
                     sog_random_depths(n = 7,
                                       ...))
  }
  return(list("plan_6_depths" = plan_6,
              "plan_7_depths" = plan_7))
}

sog_random_depths_days() %>% print()
