

# Question 1, part b)

# Given a radius, calculate the circumference and area
calcCircumferenceAndArea <- function(radius){
      circumference <- 2 * pi * radius 
      area <- pi * radius^2
      tbl <- cbind(Radius=radius, Circumference=circumference, Area=area)
      rownames(tbl) <- "Values"
      return(tbl)
}

# ANother way: using round and paste functions
sayCircumferenceAndArea <- function(radius) {
      circumference <- 2 * pi * radius 
      area <- pi * radius^2
      return(paste("The circumference = ", round(circumference,5), " and the area = ", 
                   round(area, 5), " when radius = ", round(radius,5), sep=""))
}