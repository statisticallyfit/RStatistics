
# ANother example of lda with wine data
UCI <- "ftp://ftp.ics.uci.edu/pub"
REPOS <- "machine-learning-databases"
wine.url <- sprintf("%s/%s/wine/wine.data", UCI, REPOS)
wine <- read.csv(wine.url, header=F) 
colnames(wine) <- c('Type', 'Alcohol', 'Malic', 'Ash', 
                    'Alcalinity', 'Magnesium', 'Phenols', 
                    'Flavanoids', 'Nonflavanoids',
                    'Proanthocyanins', 'Color', 'Hue', 
                    'Dilution', 'Proline')
wine$Type <- as.factor(wine$Type)

wine.lda = lda(Type ~., data=wine)
pwine = predict(wine.lda)
# first LD1 (first linear discriminant)
ldahist(data=pwine$x[,1], g=wine$Type)
# second, LD2 (second linear discriminant)
ldahist(data=pwine$x[,2], g=wine$Type)
# Scatterplots of discriminant functions
plot(pwine$x[,1], pwine$x[,2])
# Scatterplot ggplot
lda.df <- data.frame(Type=wine$Type, pwine$x)
head(lda.df)
ggplot(lda.df, aes(LD1, LD2, colour=Type)) + geom_point(size=2.5)

## QDA

wine.qda <- qda(Type ~., data=wine)
wine.qda
