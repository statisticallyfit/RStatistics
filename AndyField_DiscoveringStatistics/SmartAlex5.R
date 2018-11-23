# Smart Alex's Tasks

# TASK 1
chickFlick = read.delim("data/ChickFlick.dat", header=TRUE)
mementoFilm = subset(chickFlick, chickFlick$film=="Memento")
bridgetFilm = subset(chickFlick, chickFlick$film=="Bridget Jones' Diary")

# Basic
by(chickFlick$arousal, chickFlick$film, describe)
by(chickFlick$arousal, chickFlick$film, stat.desc, basic=F, norm=T)

# find skew/kurtosis
# NOTE: positive kurtosis = leptokurtic = thin
# negative kurtosis = platykurtic = broad, fat-tailed
skew.test(mementoFilm$arousal)
kurtosis.test(mementoFilm$arousal) # more platykurtic
skew.test(bridgetFilm$arousal)
kurtosis.test(bridgetFilm$arousal) # still platykurtic

skew.test(chickFlick$arousal)
kurtosis.test(chickFlick$arousal)


# graph

m = matrix(cbind(chickFlick$film, chickFlick$arousal), ncol=2)
colnames(m) = c("film", "arousal")
df = as.data.frame(m)
df$film = factor(df$film, levels=c(1:2), 
                 labels=c("Bridget Jones' Diary", 
                          "Memento"))
chickFlickGraph = ggplot(df, aes(x=df$arousal)) + 
      geom_density(aes(group=df$film, color=df$film), size=1)
chickFlickGraph

# Test normality: shows no significant non-normality
by(data=chickFlick$arousal,INDICES = chickFlick$film, FUN=shapiro.test)

# Test variance: the variances for the two films are not sig diff
leveneTest(chickFlick$arousal, chickFlick$film, center=median)



# TASK 2

# compare numeracy and lognumeracy
hist.numeracy
rexam$lognumeracy = log(rexam$numeracy)
rexam$sqrtnumeracy = sqrt(rexam$numeracy)
rexam$recnumeracy = 1/(rexam$numeracy)

# compare all
head(rexam)
sum(rexam$numeracy==0) # so it nevery = 0, so no need to add constant before taking the log
m = matrix(cbind(rexam$numeracy, 
                 rexam$lognumeracy, 
                 rexam$sqrtnumeracy, 
                 rexam$recnumeracy), ncol=4)
colnames(m) = c("numeracy", "lognumeracy", "sqrtnumeracy", "recnumeracy")
df = as.data.frame(m)
dfs = stack(df)
dfs
graph = ggplot(dfs, aes(values)) + 
      geom_density(aes(group=ind, col=ind), size=1)
graph


# now test if any transformation resulted in more normal data
shapiro.test(rexam$numeracy) # NO
shapiro.test(rexam$lognumeracy) # NO, but the best of all here
shapiro.test(rexam$sqrtnumeracy) # NO
shapiro.test(rexam$recnumeracy) # NO WAY
