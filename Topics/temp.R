## testing tbl
#tbl <- matrix(c(12,34,58,44,10,11,31,88,89,17,15,10,43,55,50,13, 70, 23, 30, 12), 
#              ncol=4, byrow=TRUE, dimnames=list(
#                    Health=c("Unsatisfactory","Medium", "Good", 
#                             "Very good", "Excellent"),
#                    Treatment=c("Placebo", "VitaminB12", "Manganese", "VitaminC")))#
#
## for testing concordant pairs
#job <- matrix(c(1,3,10,6, 2,3,10,7, 1,6,14,12, 0,1,9,11),
#              ncol=4, byrow = TRUE, dimnames=list(
#                    Income=c("<15,000", "15,000-25,000", "25,000-40,000", ">40,000"),
#                    JobSatisfaction=c("VeryDis", "LittleDis","ModeratelySat", "VerySat")
#              ))

# agresti page 80 pdf
#religion <- matrix(c(178,138,108, 570,648,442, 138,252,252), ncol=3, byrow = TRUE,
#                   dimnames=list(
#                         HighestDegree=c("< highschool", "highschool/juniorcollege", 
#                                         "bachelor/graduate"),
#                         ReligiousBeliefs=c("Fundamentalist","Moderate","Liberal")
#                   ))