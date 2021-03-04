# import data set in csv format 
rawData <-read.csv(file.choose(), header=T) 

# =================
# McNemar's test
# =================
# Assumptions 
# ---------------
# variabls are nominal, paired, random, independent from another, not normal
# number of discordant pairs >= 30
# --------------
# Hypothesis
# --------------
# Ho: numbers in discordant cells are equal 
# Ha: numbers in discordant cells are UNEQUAL 

## Agresti (1990), p. 350.
## Presidential Approval Ratings.
##  Approval of the President's performance in office in two surveys,
##  one month apart, for a random sample of 1600 voting-age Americans.
Performance <-
  matrix(c(794, 86, 150, 570),
         nrow = 2,
         dimnames = list("1st Survey" = c("approve", "disapprove"),
                         "2nd Survey" = c("Approve", "Disapprove")))
Performance
mcnemar.test(Performance)
## => significant change (in fact, drop) in approval ratings
