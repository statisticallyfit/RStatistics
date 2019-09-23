# Creating evenly spaced ggplot colors from the hcl color wheel: 
# g = number of groups or colors you need
ggplotColors <- function(g){
      d <- 360/g
      h <- cumsum(c(15, rep(d,g - 1)))
      hcl(h = h, c = 100, l = 65)
}
# SOURCE: https://data.library.virginia.edu/setting-up-color-palettes-in-r/


# Interaction plot:
interactionPlot <- function(x.factor, trace.factor, response, data){
      #detach(package:plyr)
      suppressMessages(require(dplyr))
      #summ <- ddply(drinkData, .(weeks, treat), summarise, wt=mean(wt))
      #cs = colnames(summ)
      #cs[3] = response # placing string value here
      #colnames(summ) = cs
      
      meanFunction = paste0('mean(', response, ')'); 
      meanName = response # paste0('mean_', response)
      
      mysumm = suppressWarnings(data %>% 
                                      group_by_(.dots = list(x.factor, trace.factor)) %>% 
                                      summarise_(.dots = setNames(meanFunction, meanName)))
      
      mysumm <- data.frame(mysumm)
      
      ggplot(data, aes_string(x=x.factor, y=response, color=trace.factor)) + 
            geom_point(data=mysumm, aes_string(group=trace.factor, color=trace.factor), 
                       size=2) + 
            geom_line(data=mysumm, aes_string(group=trace.factor), size=1)
}