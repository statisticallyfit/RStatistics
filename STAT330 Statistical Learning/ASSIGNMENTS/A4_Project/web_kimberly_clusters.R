# http://www.kimberlycoffey.com/blog/2016/8/k-means-clustering-for-customer-segmentation

models <- data.frame(k=integer(),
                     tot.withinss=numeric(),
                     betweenss=numeric(),
                     totss=numeric(),
                     rsquared=numeric())

for (k in 1:6 ) {
      
      # Run kmeans
      # nstart = number of initial configurations; the best one is used
      # $iter will return the iteration used for the final model
      output <- kmeans(, k, nstart = 20)
      
      # Add cluster membership to customers dataset
      var.name <- paste("cluster", k, sep="_")
      customers[,(var.name)] <- output$cluster
      customers[,(var.name)] <- factor(customers[,(var.name)], levels = c(1:k))
      
      # Graph clusters
      cluster_graph <- ggplot(customers, aes(x = frequency.log, y = monetary.log))
      cluster_graph <- cluster_graph + geom_point(aes(colour = customers[,(var.name)]))
      colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black')
      cluster_graph <- cluster_graph + scale_colour_manual(name = "Cluster Group", values=colors)
      cluster_graph <- cluster_graph + xlab("Log-transformed Frequency")
      cluster_graph <- cluster_graph + ylab("Log-transformed Monetary Value of Customer")
      title <- paste("k-means Solution with", k, sep=" ")
      title <- paste(title, "Clusters", sep=" ")
      cluster_graph <- cluster_graph + ggtitle(title)
      print(cluster_graph)
      
      # Cluster centers in original metrics
      library(plyr)
      print(title)
      cluster_centers <- ddply(customers, .(customers[,(var.name)]), summarize,
                               monetary=round(median(monetary),2),# use median b/c this is the raw, heavily-skewed data
                               frequency=round(median(frequency),1),
                               recency=round(median(recency), 0))
      names(cluster_centers)[names(cluster_centers)=="customers[, (var.name)]"] <- "Cluster"
      print(cluster_centers)
      cat("\n")
      cat("\n")
      
      # Collect model information
      models[k,("k")] <- k
      models[k,("tot.withinss")] <- output$tot.withinss # the sum of all within sum of squares
      models[k,("betweenss")] <- output$betweenss
      models[k,("totss")] <- output$totss # betweenss + tot.withinss
      models[k,("rsquared")] <- round(output$betweenss/output$totss, 3) # percentage of variance explained by cluster membership
      assign("models", models, envir = .GlobalEnv)
      
      remove(output, var.name, cluster_graph, cluster_centers, title, colors)
      
}