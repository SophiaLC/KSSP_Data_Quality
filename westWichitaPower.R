#### NOTES ####
# AUTHOR: Mark White
# CONTACT: markhwhiteii@gmail.com
# PURPOSE: To calculate observed power at different N and ES
# METHOD: Creates a 100,000 person population with different population proportions.
# For a given n and prevalence, the function generates 5,000 different samples,
# calculates a risk ratio and p-value for each sample, and then returns the 
# percentage of tests that were statistically significant (i.e., the power)
# ARGUMENTS: nexp = N of exposed people; nunexp = N of unexposed people;
# propexp = proportion of exposed people with outcome;
# propunexp = proportion of unexposed people with outcome.

#### FUNCTION ####
rrpower <- function(nexp, nunexp, propexp, propunexp) { # defining function
  require(epitools) # loading package
  require(dplyr) # loading package
  exposure <- c(rep(1,(nexp/(nexp+nunexp))*100000), # creating exposed cases
                rep(0,(nunexp/(nexp+nunexp))*100000)) # creating unexposed cases
  outcome <- c(rep(1,round(((nexp/(nexp+nunexp))*100000*propexp),0)), # creating yes outcome exposed cases
               rep(0,round(((nexp/(nexp+nunexp))*100000*(1-propexp)),0)), # creating no outcome exposed cases
               rep(1,round(((nunexp/(nexp+nunexp))*100000*propunexp),0)), # creating yes outcome unexposed cases
               rep(0,round(((nunexp/(nexp+nunexp))*100000*(1-propunexp)),0))) # creating no outcome unexposed cases
  d <- data.frame(exposure, outcome) # merging exposure and outcome into data frame
  t <- table(exposure, outcome) # creating table of results
  set.seed(1839) # setting seed for reproducibility
  results <- data.frame(sample=rep(NA,5000), rr=rep(NA,5000), p=rep(NA,5000)) # creating empty data frame to fill with results
  for (i in 1:5000) { # setting the loop to run 5000 times
    table <- with(bind_rows(subset(d, exposure==1)[sample(1:nrow(subset(d, exposure==1)), nexp, FALSE),], # random sample of expose
                            subset(d, exposure==0)[sample(1:nrow(subset(d, exposure==0)), nunexp, FALSE),]), # random sample of nonexpose
                  table(exposure, outcome)) # create a crosstab for this sample
    results$sample[i] <- i # saving sample number
    results$rr[i] <- epitab(table, method="riskratio")$tab[2,5] # saving risk ratio
    results$p[i] <- epitab(table, method="riskratio")$tab[2,8] # saving p-value
    if (i %% 100 == 0) { # the next three lines provide a progress report...
      cat("Progress:", paste0(round((i/5000*100),2),"%"), "\n") # ...because I am impatient...
    } # ...these can be commented out, if wanted
  }
  results$sig <- ifelse(results$p<.05, "yes", "no") # categorizing as significant or not significant
  cat("Power:",paste0(round(sum(results$sig=="yes")/length(results$sig)*100,2),"%")) # percentage of significant results/power
}

#### CALCULATING POWER ####
# Living and deceased subjects
rrpower(4660, 4660, .065, .050)
rrpower(4660, 4660, .075, .050)
rrpower(4660, 4660, .085, .050)
rrpower(4660, 4660, .100, .050)

rrpower(4660, 4660, .130, .100)
rrpower(4660, 4660, .150, .100)
rrpower(4660, 4660, .170, .100)
rrpower(4660, 4660, .200, .100)

rrpower(4660, 4660, .195, .150)
rrpower(4660, 4660, .225, .150)
rrpower(4660, 4660, .255, .150)
rrpower(4660, 4660, .300, .150)

rrpower(4660, 4660, .260, .200)
rrpower(4660, 4660, .300, .200)
rrpower(4660, 4660, .340, .200)
rrpower(4660, 4660, .400, .200)

# Questionnaire respondents only
rrpower(2660, 2660, .065, .050)
rrpower(2660, 2660, .075, .050)
rrpower(2660, 2660, .085, .050)
rrpower(2660, 2660, .100, .050)

rrpower(2660, 2660, .130, .100)
rrpower(2660, 2660, .150, .100)
rrpower(2660, 2660, .170, .100)
rrpower(2660, 2660, .200, .100)

rrpower(2660, 2660, .195, .150)
rrpower(2660, 2660, .225, .150)
rrpower(2660, 2660, .255, .150)
rrpower(2660, 2660, .300, .150)

rrpower(2660, 2660, .260, .200)
rrpower(2660, 2660, .300, .200)
rrpower(2660, 2660, .340, .200)
rrpower(2660, 2660, .400, .200)