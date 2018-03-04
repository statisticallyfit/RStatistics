

# random sequence of 100 integers 1,2,3
#runif(100, min=1, max=3)
# this represents the door numbers the car is behind for each of the 100 nights
doorsWithCarEachNight <- sample(c('A', 'B', 'C'), size=100, replace=TRUE)
sum(doorsWithCarEachNight == 'A')
sum(doorsWithCarEachNight == 'B')
sum(doorsWithCarEachNight == 'C')

# represents the contestant's first choice on each of the 100 nights
personFirstChoiceEachNight <- sample(c('A', 'B', 'C'), size=100, replace=TRUE)
sum(personFirstChoiceEachNight == 'A')
sum(personFirstChoiceEachNight == 'B')
sum(personFirstChoiceEachNight == 'C')

# The number of times the numbers in the two cols agree is the number of
# times the person will win if he doesn't change doors. 
# And if the numbers in the two cols don't agree, then the contestant will
# win only if he decides to change doors (higher chance). 
df <- data.frame(CarDoor=doorsWithCarEachNight, PersonChoice=personFirstChoiceEachNight)
head(df)

agreements <- doorsWithCarEachNight == personFirstChoiceEachNight
probWinIfNoSwitch <- sum(agreements) / length(agreements)
probWinIfSwitch <- (length(agreements) - sum(agreements)) / length(agreements)
probWinIfSwitch
probWinIfNoSwitch




###### ACCEPTED ANSWER: 
car.door <- sample(c('A', 'B', 'C'), size=100, replace=TRUE)
# this is person's first choice on each of the 100 nights
first.choice <- sample(c('A', 'B', 'C'), size=100, replace=TRUE)
confusion.table <- table(car.door, first.choice)
confusion.table
agree <- sum(diag(confusion.table))

# would have won 30 times if they keep choice
probWinIfSwitch <- agree / sum(confusion.table); probWinIfSwitch
# would have won 70 times of 100 if they had changed. 
probWinIfNoSwitch <- 1 - probWinIfSwitch; probWinIfNoSwitch
