library(ggplot2)
library(plotly)

check <- function(gg, expected, name=NULL){
  if(is.null(name)){
    m <- match.call()
    name <- as.character(m$gg)
  }
  list(ggplot=gg, expected=expected, name=name)
}
check.named <- function(expected, generated, trace){
  for(L in list(expected, generated)){
    stopifnot(is.list(L))
    stopifnot(!is.null(names(L)))
    if(any(names(L) == "")){
      print(names(L))
      stop("un-named elements")
    }
  }
  for(name in names(expected)){
    this.trace <- c(trace, name)
    e <- expected[[name]]
    g <- generated[[name]]
    bad <- function(msg="did not generate what we expected"){
      if(missing(msg)){
        print(list(expected=e, generated=g))
      }
      print(this.trace)
      stop(msg)
    }
    if(is.list(e)){
      if(!is.list(g)){
        bad()
      }
      check.named(e, g, this.trace)
    }else if(is.atomic(e)){
      if(!is.atomic(g) || length(g) != length(e)) {
        bad()
      }
      if(is.numeric(e)){
        if(!is.numeric(g)){
          bad()
        }
        char.if.different <- all.equal(e, g)
        if(is.character(char.if.different)){
          print(rbind(expected=e, generated=g))
          print(char.if.different)
          bad("not numerically equal")
        }
      }else if(is.character(e) || is.factor(e)){
        if(any(e != g)){
          bad()
        }
      }else{
        print(e)
        stop("do not know what to do with this expectation")
      }
    }else{
      print(e)
      stop("do not know what to do with this expectation")
    }
  }
}

## Generate lineplot data.
set.seed(1)
n.groups <- 20
Groups <- data.frame(x=rep(1:10, times=n.groups),
                     group = rep(1:n.groups, each=10))
Groups$lt <- c("even", "odd")[(Groups$group%%2+1)] # linetype
Groups$group <- as.factor(Groups$group)
Groups$y <- rnorm(length(Groups$x), Groups$x, .5) +
  rep(rnorm(n.groups, 0, 2), each=10)
## Simple black lineplot.
AllBlack <- ggplot(Groups) +
  geom_line(aes(x=x, y=y, group=group)) + 
  ggtitle("geom_line")
group.list <- split(Groups, Groups$group)
AllBlack.expected <- list()
for(group.i in seq_along(group.list)){
  g <- group.list[[group.i]]
  AllBlack.expected[[group.i]] <-
    list(x=g$x, y=g$y, type="scatter", mode="lines",
         line=list(color=toRGB("black")))
}
## A ggplot with 6 different automatic types should be converted to
## plotly's 6 types.
Types <- ggplot(subset(Groups, as.integer(group)<=6)) +
  geom_line(aes(x=x, y=y, group=group, linetype=group))+
  ggtitle("geom_line + scale_linetype automatic")
Types.expected <- AllBlack.expected[1:6]
dash <-
  c("solid",
    "dash",
    "dot",
    "dashdot",
    "longdash",
    "longdashdot")
for(trace.i in seq_along(Types.expected)){
  Types.expected[[trace.i]]$line$dash <- dash[[trace.i]]
}


## Canada city population map.
library(maps)
data(canada.cities)
DefaultCities <- ggplot(canada.cities, aes(long, lat))+
  borders(regions="canada", name="borders")+
  coord_equal()+
  geom_point(aes(text=name, size=pop), colour="red",
             alpha=1/2, name="cities")
b <- borders(regions="canada")$data
group.list <- split(b, b$group)
line.df <- data.frame()
for(group.i in seq_along(group.list)){
  g <- group.list[[group.i]]
  line.df <- rbind(line.df, g, NA)
}
normalize <- function(x, m, M){
  x <- na.omit(x)
  zero.one <- (x-min(x))/(max(x)-min(x))
  stopifnot(range(zero.one) == c(0,1))
  m.M <- zero.one*(M-m) + m
  stopifnot(range(m.M) == c(m, M))
  m.M
}
DefaultCities.expected <-
  list(list(x=line.df$long, y=line.df$lat,
            type="scatter", mode="lines", name="borders",
            line=list(dash="solid", color=toRGB("grey50"))),
       with(canada.cities,{
         list(x=long, y=lat, text=name, type="scatter", mode="markers",
              name="cities",
              marker=list(opacity=1/2, color=toRGB("red"), size=pop))
       }))
## different ways to define the iris scatterplot, these should all
## give the same result.
iris.plots <-
  list(global=ggplot(iris,aes(Petal.Width, Sepal.Width, color=Species))+
       geom_point(),
       point=ggplot(iris)+
       geom_point(aes(Petal.Width, Sepal.Width, color=Species)),
       qplot=qplot(Petal.Width, Sepal.Width, color=Species, data=iris))
## different ways to define color, these should all give the same result.
color.synonyms <-
  list(color=qplot(Petal.Width, Sepal.Width, color=Species, data=iris),
       colour=qplot(Petal.Width, Sepal.Width, colour=Species, data=iris),
       col=qplot(Petal.Width, Sepal.Width, col=Species, data=iris))
igroups <- split(iris, iris$Sp)

## TODO: revise this test so that we only send 1 color!
## iris.expected <- list()
## colors3 <- c("rgb(0,186,56)","rgb(248,118,109)","rgb(97,156,255)")
## inames <- c("versicolor", "setosa", "virginica")
## for(species.i in seq_along(inames)){
##   iname <- inames[[species.i]]
##   sp <- igroups[[iname]]
##   iris.expected[[species.i]] <-
##     list(x=sp$Petal.Width, y=sp$Sepal.Width, name=as.character(sp$Sp[1]),
##          type="scatter", mode="markers",
##          marker=list(color=rep(colors3[[species.i]], nrow(sp))))
## }

## Checklist.
to.check <-
  list(check(AllBlack, AllBlack.expected),
       check(DefaultCities, DefaultCities.expected),
       check(Types, Types.expected))

## TODO: check.unordered function!
## for(name in names(iris.plots)){
##   full.name <- sprintf("iris.%s", name)
##   to.check[[length(to.check)+1]] <-
##     check(iris.plots[[name]], iris.expected, full.name)
## }

for(L in to.check){
  generated <- gg2list(L$gg)
  for(trace.i in seq_along(L$expected)){
    e <- L$exp[[trace.i]]
    g <- generated[[trace.i]]
    check.named(e, g, c(L$name, trace.i))
  }
}


