# Strait of Georgia survey random assignment of depths. Just run this
# code. Could do with a bit of tidying up and checking of help. Maybe worth a
# package if design keeps changing and generality is desired.

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
##' @param specific_depths numeric vector of specific depths
##' @param specific_probs numeric vector of specific probabilities corresponding
##'   to depths in `specific_depths`
##' @param type "once_or_twice" for defining `depths_twice` and `depths_once`,
##'   or "specific" for defining `specific_depths` and `specific_probs`
##' @return Vector of length `n` with depth for each tow.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' sog_random_depths(n = 7)
##' }
sog_random_depths <- function(n = 6,
                              depths_twice = c(0, 15),
                              depths_once = c(30, 45, 60),
                              specific_depths = c(0, 15, 30, 45, 60),
                              specific_probs = c(45, 30, 15, 5, 5),
                              type = "once_or_twice"){
  if(!(n %in% c(6, 7))){
    stop("n currently needs to be 6 or 7")
  }

  if(type == "once_or_twice"){
    if(n == 6){
      depths_once <- sample(depths_once)[1:2]       # Just need two of them
    }

    ret <- sample(c(depths_twice,
                    depths_twice,
                    depths_once))
  }

  if(type == "specific"){
    # Assign depths based on specific_probs
    ret <- sample(specific_depths,
                  size = n,
                  prob = specific_probs,
                  replace = TRUE)
  }

  return(ret)
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

##' Randomly assign depths to tows for Strait of Georgia Juvenile Salmon Survey
##'  specifically for Fall 2024 6-tow days with 2 tows at 0
##'
##' For 6-tow days want, for example (this is for group 1 as used later):
##' 6 days with random (in terms of ordering) 2 tows at 0 m and 2 at 15 m, with
##' the remaining 2 sampled without replacement as 60% at 30m, 20% at 45 m, 20% at 60m.
##' Using these values as defaults, without thinking yet how adaptable code
##' might be to other values. See
##' Then another design for six other days; see `fall_2024_depths_6_tow_days()`
##' for calling this function twice.
##'
##' Note that argument definitions are slightly different to those in earlier functions.
##'
##' @param days number of days
##' @param depths_definite numeric vector of depths that definitely have to occur
##' @param depths_definite_freq numeric vector of frequency we want each `depths_definite` to occur
##' @param specific_depths depths to then randomly sample from, without replacement
##' @param specific_probs probabilities of each of those depths
##' @param ...
##' @param depths_twice depths to definitely have twice
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun
##' }
fall_2024_depths_6_tow_days_generate <- function(days = 6,
                                                 depths_definite = c(0, 15),
                                                 depths_definite_freq = c(2, 2),
                                                 specific_depths = c(30, 45, 60),
                                                 specific_probs = c(0.6, 0.2, 0.2)){
  res <- data.frame("day" = NA,
                    tow_1 = NA,
                    tow_2 = NA,
                    tow_3 = NA,
                    tow_4 = NA,
                    tow_5 = NA,
                    tow_6 = NA)

  for(i in 1:days){
    tows_today <- c(rep(depths_definite,
                        depths_definite_freq),
                    sample(specific_depths,
                           size = 6 - sum(depths_definite_freq),
                           prob = specific_probs))
    res[i, ] <- c(i,
                  sample(tows_today))
  }
  return(res)
}

##' Generate 12 days of tows for Fall 2024 survey, by calling
##'  `fall_2024_depths_6_tow_days_generate()` twice and then permuting
##'
##' Group 1:
##' 6 days with random (in terms of ordering) 2 tows at 0 m and 2 at 15 m, with
##' the remaining 2 sampled without replacement as 60% at 30m, 20% at 45 m, 20% at 60m.
##' These are the defaults in `fall_2024_depths_6_tow_days_generate()`.
##'
##' Group 2:
##' 6 days with random 3 tows at 0 m and 1 at 15 m with the remaining 2 as
##' previous line. This overall gives more 0 m than 15 m. These are the defaults
##' here, that then get passed onto `fall_2024_depths_6_tow_days_generate()`
##'
##' Then permute all the day so we don't get a block of group 1 then group 2.
##' @param days_each_group vector of number of days in group 1 and then group 2
##' @param depths_definite_group_2
##' @param depths_definite_group_2
##' @param specific_depths_group_2
##' @param specific_probs_group_2
##' @param ...
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
fall_2024_depths_6_tow_days <- function(days_each_group = c(6, 6),
                                        depths_definite_group_2 = c(0, 15),
                                        depths_definite_freq_group_2 = c(3, 1),
                                        specific_depths_group_2 = c(30, 45, 60),
                                        specific_probs_group_2 = c(0.6, 0.2, 0.2),
                                        ...){
  group_1 <- fall_2024_depths_6_tow_days_generate(days = days_each_group[1])
  group_2 <- fall_2024_depths_6_tow_days_generate(days = days_each_group[2],
                                                  depths_definite =
                                                    depths_definite_group_2,
                                                  depths_definite_freq = depths_definite_freq_group_2,
                                                  specific_depths = specific_depths_group_2,
                                                  specific_probs = specific_probs_group_2)

  groups_both <- rbind(group_1,
                       group_2)
  # Now randomise the days (so don't have group_1 then group_2):
  res <- groups_both[sample(nrow(groups_both)),
                     ]
  res$day <- 1:nrow(res)
  row.names(res) <- NULL

  return(res)
}

##' Protocol for 7-tow days
##'
##'
##' 7-tow day has 3 tows at 0 m, 2 at 15 m, 1 at 30 m and randomly 45 m or 60 m
##' (presumably with equal probability for the last two).
##'
##' @param days
##' @param depths_definite
##' @param depths_definite_freq
##' @param specific_depths
##' @param specific_probs
##' @param ...
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
fall_2024_depths_7_tow_days <- function(days = 12,
                                        depths_definite = c(0, 15, 30),
                                        depths_definite_freq = c(3, 2, 1),
                                        specific_depths = c(45, 60),
                                        specific_probs = c(0.5, 0.5)){
  res <- data.frame("day" = NA,
                    tow_1 = NA,
                    tow_2 = NA,
                    tow_3 = NA,
                    tow_4 = NA,
                    tow_5 = NA,
                    tow_6 = NA,
                    tow_7 = NA)

  for(i in 1:days){
    tows_today <- c(rep(depths_definite,
                        depths_definite_freq),
                    sample(specific_depths,
                           size = 7 - sum(depths_definite_freq),
                           prob = specific_probs))
    res[i, ] <- c(i,
                  sample(tows_today))
  }
  return(res)
}




# SoG June 2024 survey
# Amy asked for
# 6 tows in a day
# 2 random at 0 depth
# 2 at 15
# 2 are a choice of 30, 45, 60, without replacement.

# 2nd option - 7 tows. Last one gets full.

set.seed(21)
# As used for Nacho's Euros 2024 (soccer) picks for a pool. 21
#  was the number of times he barked during a five minute period when
#  his dog friend in the house behind ours was over in his yard.
#  No less/more random than anything else to design a survey.
sog_21 <- sog_random_depths_days()
sog_21

# Fall survey, 19/9/24. Given depths and probabilities of picking each depth,
# but not actually what was wanted. See next chunk.
set.seed(56)       # assigned by Jackie
sog_56_orig <- sog_random_depths_days(type = "specific")

# Okay, this is what we want.
set.seed(56)
sog_fall_2024_6_tow_days <- fall_2024_depths_6_tow_days()

set.seed(56)
sog_fall_2024_7_tow_days <- fall_2024_depths_7_tow_days()

print("SoG Fall 2024 survey 6-tow days are:")
sog_fall_2024_6_tow_days %>% print()

print("SoG Fall 2024 survey 7-tow days are:")
sog_fall_2024_7_tow_days %>% print()
