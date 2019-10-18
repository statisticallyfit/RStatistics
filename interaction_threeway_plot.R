
# Here is the code for the dae library's interaction ABC plot (since on R 3.6.1, dae is not available)

interaction.ABC.plot <- function(response, x.factor, groups.factor, trace.factor, data, 
                                 fun="mean", title="A:B:C Interaction Plot", 
                                 xlab, ylab, key.title, lwd=4, columns=2, 
                                 ggplotFuncs = NULL, ...)
{
      # form data.frame containing the means of the response variable for the three factors
      name.r <- deparse(substitute(response))
      name.x <- deparse(substitute(x.factor))
      name.g <- deparse(substitute(groups.factor))
      name.t <- deparse(substitute(trace.factor))
      lev.x <- levels(as.factor(data[[match(name.x, names(data))]]))
      lev.g <- levels(as.factor(data[[match(name.g, names(data))]]))
      lev.t <- levels(as.factor(data[[match(name.t, names(data))]]))
      no.x <- length(lev.x)
      no.g <- length(lev.g)
      no.t <- length(lev.x)
      fnames <- list(x.factor = lev.x, groups.factor = lev.g, trace.factor = lev.t)
      data.means <- fac.gen(generate = fnames, order="yates")
      data.means <- data.frame(data.means, as.vector(tapply(data[[match(name.r, names(data))]], 
                                                            list(data[[match(name.x, names(data))]],
                                                                 data[[match(name.g, names(data))]], 
                                                                 data[[match(name.t, names(data))]]), FUN=mean, simplify=T)))
      dimnames(data.means)[[2]] <- c(name.x, name.g, name.t, name.r)
      levels.x <- levels(data.means[[name.x]])
      if (any(is.na(levels.x)))
            stop("The x.factor has NA as a level")
      # if x.factor has numeric levels then coerce it to a numberic for plotting
      if (!any(is.na(suppressWarnings(as.numeric(levels.x)))))
            data.means[[name.x]] <- as.numfac(data.means[[name.x]])
      # set up arguments for plot
      if (missing(xlab)) xlab <- deparse(substitute(x.factor))
      if (missing(ylab)) ylab <- deparse(substitute(response))
      if (missing(key.title)) key.title <- deparse(substitute(groups.factor))
      formula.plot <- formula(paste(deparse(substitute(response)), " ~ as.numeric(", 
                                    deparse(substitute(x.factor)), ") | ", deparse(substitute(trace.factor))))
      # do the plot
      addfacname <- function(string)
      {
            string <- paste(name.t,": ", as.character(string), sep="")
            return(string)
      }
      int.plot <- ggplot(data=data.means, 
                         aes_string(x = name.x, y = name.r, linetype=name.g, colour=name.g, group=name.g), ...) +
            geom_line() + geom_point() + 
            labs(x = xlab, y = ylab, title = title, group=key.title) +
            facet_grid(formula(paste("~ ", name.t, sep="")), 
                       labeller = labeller(.cols = addfacname))
      if (!is.null(ggplotFuncs))
            for (f in ggplotFuncs)
                  int.plot <- int.plot + f
      print(int.plot)
}



# --------------------------------------------------------------------------------------------

# FAC_functions

"factor.list" <- function(generate, order="standard")
      #takes a generate list and creates a list of factor names, with levels 
      #information, and a list of factor relative replications, both of which are 
      #returned as a list of the two parallel lists called factors and reps.
{ n <- length(generate)
which.ord <- pmatch(casefold(order), c("standard", "yates"), nomatch="")
if (which.ord == "")	stop("order must be either standard or yates")
# standard order
if (which.ord == "1")
      counter <- 1:n
else
      # Yates order
      counter <- n:1
kfac <- 0
for(i in counter) 
{ if(!(names(generate[i]) == ""))
{ kfac <- kfac+1
if (kfac == 1)
{ fnames <- list(generate[[i]])
names(fnames) <- names(generate)[i]
freps <- 1
}
else
{ knames <- list(generate[[i]])
names(knames) <- names(generate)[i]
fnames <- c(fnames, knames)
freps <- c(freps, 1)
}
}
      else
      { if (kfac == 0)
            if (which.ord == "1")
                  stop("Must start with a factor name - set times argument instead")
            else
                  stop("Must end with a factor name - set each argument instead")
            freps[kfac] <- generate[[i]]
      }
}
if (which.ord == "2") #reverse lists for Yates order
{ fnames <- fnames[kfac:1]
freps <- freps[kfac:1]
}
return(list(factors = fnames,reps = freps))
}

"fac.gen" <- 
      function(generate, each=1, times=1, order="standard")
      {
            #generate is a list of factors and numbers that specify the pattern in which 
            #the levels of the factors are to be generated.
            #If the component of the list is a factor name, it should be the name of a list 
            #that contains either a single numeric value that is the number of levels, a 
            #numeric vector that contains the levels of the factor or a character vector 
            #that contains the labels of the factor.
            if(!is.list(generate))
                  stop("generate must be a list")
            facs.reps <- factor.list(generate, order)
            fnames <- facs.reps$factors
            freps <- facs.reps$reps
            nfac <- length(fnames)
            levels <- rep(1, times=nfac)
            for (i in 1:nfac)
            { if (is.numeric(fnames[[i]]) | is.character(fnames[[i]]))
            { if (length(fnames[[i]]) == 1)
                  if (is.character(fnames[[i]]))
                        levels[i] <- 1
                  else
                        levels[i] <- fnames[[i]]
                  else
                        levels[i] <- length(fnames[[i]])
            }
                  else
                  { stop("Levels of factors must be specified using either numeric or character vectors")
                  }
            }
            n <- prod(levels)*prod(freps)*each*times
            which.ord <- pmatch(casefold(order), c("standard", "yates"), nomatch="")
            if (which.ord == "")	stop("order must be either standard or yates")
            # standard order
            if (which.ord == "1") 
                  counter <- nfac:1
            else
                  # Yates order
                  counter <- 1:nfac
            genlist <- vector("list", nfac)
            keach <- each
            for (i in counter)
            { lev <- 1:levels[i]
            keach <- keach*freps[i]
            ktimes <- n/(levels[i]*keach)
            { if (is.numeric(fnames[[i]]))
            { if (length(fnames[[i]]) != 1)
                  lev <- fnames[[i]]
            genlist[[i]] <- factor(rep(lev, times=ktimes, each=keach))   
            }
                  else
                  { genlist[[i]] <- factor(rep(lev, times=ktimes, each=keach), labels=fnames[[i]])
                  }
                  keach <- keach*levels[i] 
            }
            }
            genframe <- data.frame(genlist)
            names(genframe) <- names(fnames) 
            genframe
      }


######################################################

#Other fac functions

"as.numfac" <- function(factor)
{ if (!is.numeric(factor))
{ levs <- levels(factor)
if (any(is.na(suppressWarnings(as.numeric(levs[!is.na(levs)])))))
      warning("Some levels do not have values that are numeric in nature")
#see factor help
factor <- as.numeric(levels(factor))[factor]
}
      return(factor)
}

"mpone" <- function(factor) {2*as.numeric(factor)-3}

"fac.recode" <- function(factor, newlevels, ...)
      #function to form a new factor by changing the levels of factor
{ 
      nlev <- length(levels(factor))
      if (nlev != length(newlevels))
      { stop("Must supply a new level for every level in the supplied factor")}
      new.fac <- factor(newlevels[factor], ...)
      return(new.fac)
}

"fac.combine" <- function(factors, order="standard", combine.levels=FALSE, sep=",", ...)
{
      #
      # test arguments
      #
      if (mode(factors) != "list") stop("Must supply a list")
      if (!all(sapply(factors, inherits, what="factor"))) 
            stop("All elements of list must be factors or ordereds")
      new.fac <- factors[[1]]
      nfac <- length(factors)
      if (nfac > 1)
      {
            if (var(sapply(factors, length)) != 0) stop("All factors must be of the same length")
            which.ord <- pmatch(casefold(order), c("standard", "yates"), nomatch="")
            if (which.ord == "")  stop("order must be either standard or yates")
            # standard order
            if (which.ord == "1") counter <- nfac:1
            # Yates order
            else if (which.ord== "2") counter <- 1:nfac
            #
            # compute new factor
            #   
            radix <- 1
            new.fac <- rep(1, length(factors[[1]])) 
            na.lev <- FALSE
            for (i in counter)
                  #reassign factor so unused levels removed
            { 
                  f <- factor(factors[[i]])
                  nlev <- length(levels(f))
                  new.fac <- (as.numeric(f)-1) * radix + new.fac
                  if (combine.levels)
                  {
                        if (any(is.na(levels(f))))
                              na.lev <- TRUE
                        if (i == counter[1])
                              radix.lev <- paste(levels(f))
                        else
                        { if (which.ord == 1)
                              radix.lev <- paste(rep(levels(f), each=radix), rep(radix.lev, times=nlev), sep=sep)
                        else
                              radix.lev <- paste(rep(radix.lev, times=nlev), rep(levels(f), each=radix), sep=sep)
                        }
                  }
                  radix <- radix * nlev
            }
            if (combine.levels)
            { 
                  obslevs <- unique(new.fac)
                  obslevs <- obslevs[order(obslevs)]
                  if (!na.lev)
                        obslevs <- obslevs[!is.na(obslevs)]
                  new.fac <- factor(new.fac, labels=radix.lev[obslevs], ...)
            } 
            else
                  new.fac <- factor(new.fac, ...)
      }
      return(new.fac)
}

"fac.divide" <- function(combined.factor, factor.names, order="standard")
{
      #
      # test arguments
      #
      # if (mode(factor.names) != "list" | length(factor.names) == 1) 
      #    stop("Must supply a list of more than one component")
      if (mode(factor.names) != "list") 
            stop("Must supply a list of factor names")
      nfac <- length(factor.names)
      which.ord <- pmatch(casefold(order), c("standard", "yates"), nomatch="")
      if (which.ord == "")  stop("order must be either standard or yates")
      # standard order
      if (which.ord == "1") counter <- nfac:1
      # Yates order
      else if (which.ord== "2") counter <- 1:nfac
      #
      # convert factor or numeric to numeric with values 1:nlev and compute new factors
      #   
      if (is.factor(combined.factor))
            nlev <- length(levels(combined.factor))
      else
            nlev <- length(unique(combined.factor))
      kombined.factor <- c(1:nlev)[combined.factor]
      for (i in counter)
      { klev <- length(factor.names[[i]])
      if (is.numeric(factor.names[[i]]))
      { if (klev != 1)
      { lev <- factor.names[[i]]
      val <- (kombined.factor - 1) %% klev + 1
      factor.names[[i]] <- factor(lev[val])
      }
            else
            { klev <- factor.names[[i]]
            factor.names[[i]] <- factor((kombined.factor - 1) %% klev + 1)
            }
      }
      else
      { factor.names[[i]] <- factor((kombined.factor - 1) %% klev + 1, 
                                    labels = factor.names[[i]])
      }
      kombined.factor <- (kombined.factor - 1) %/% klev + 1
      }
      new.factors <- data.frame(factor.names)
}

"fac.nested" <- function(nesting.fac, levels=NA, labels=NA, ...)
{
      n <- length(nesting.fac)
      reps <- table(nesting.fac)
      levs <- levels(nesting.fac)
      no.lev.between <- length(levs)
      no.lev.within <- max(table(nesting.fac))
      nested.fac <- c(rep(1, n))
      nested.fac[is.na(nesting.fac)] <-NA  #Line added
      #Set levels if nested factor for ach levels of nested factor, avoinding missing values
      for(i in 1:no.lev.between)
            nested.fac[!is.na(nesting.fac)][nesting.fac[!is.na(nesting.fac)] == levs[i]] <- 1:reps[i]
      if (length(levels) == 1 && is.na(levels)) levels <- 1:no.lev.within
      if (length(labels) == 1 && is.na(labels)) labels <- as.character(levels)
      nested.fac <- factor(nested.fac, levels=levels, labels=labels, ...)
      return(nested.fac)
}