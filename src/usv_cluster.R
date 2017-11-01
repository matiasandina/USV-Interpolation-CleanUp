mbt.interp <- read.csv(file.choose(),header=TRUE)

library('FactoMineR')
library('dplyr')




# Take the difference in rows (within call), and transpose to have the same orientation as big data frame
# Names are spurious now
# freq.X <- t(apply(mbt.interp[,3:12],1,diff))


# In Grimsley2013 they use "start" "middle" and "end" of the frequencies
# Hence  complete here new variables

# step.count <- freq.X>10000

# count by rows the true values
# step <- rowSums(step.count)


# paper.matrix <- data.frame(label=mbt.interp$label,
#                           duration=mbt.interp$duration,
#                           f1=mbt.interp$f1,
#                           f5=mbt.interp$f5,
#                           f10=mbt.interp$f10,
#                           bandwidth=mbt.interp$bandwidth,
#                           step=step)

# paper.clust <- HCPC(select(paper.matrix,-label), nb.clust=-1, consol=TRUE, iter.max=10, min=3, 
#                max=NULL, metric="euclidean", method="ward", order=TRUE,
#                graph.scale="inertia", nb.par=5, graph=TRUE, proba=0.05, 
#                cluster.CA="rows",kk=Inf,description=TRUE)


# Who is within each cluster

# res.clust <- data.frame(new.data, clust=diff.clust$data.clust$clust)


### Dynamic Time Warping #####

library('dtw') ; library('dtwclust')

# dc is the distance we allow for cut offs
# k is the number of clusters we want
# window.size is the points that we will compare with x position (x-window.size and x+window.size)
# cannot be greater than the whole time series
# https://rdrr.io/cran/dtwclust/man/TADPole.html

# ## dtwclust PACKAGE
# https://github.com/asardaes/dtwclust

pc.tadp <- tsclust(mysub, type = "tadpole", k = 10,
                   trace = TRUE,
                   control = tadpole_control(dc = 100000, window.size = 2L))
plot(pc.tadp)



# Reproducible example

set.seed(980)

# Non addaptors data
# mysub<-mbt.interp[sample(x = nrow(mbt.interp),size = 2000),3:12]

mysub <- Adapted_50 

pc.tadp <- tsclust(mysub, type = "tadpole", k = 20,
                   trace = TRUE,
                   control = tadpole_control(dc = 1000, window.size = 2L))
plot(pc.tadp)


## What happens if we get rid of all the flats?
not.flat <-mbt.interp[!grepl("flat", mbt.interp$label),]

not.flat.freq<-not.flat[,3:12]

not.flat.pc.tadp <- tsclust(not.flat[,c(2,3:12,15)], type = "tadpole", k = 20,
                   trace = TRUE,
                   control = tadpole_control(dc = 10000, window.size = 2L))
plot(not.flat.pc.tadp)

## What happens if we add duration and bandwidth as final points of the "time series"

library(dtwclust)

all.values <- mbt.interp[sample(x = nrow(mbt.interp),size = 2000),
                           c(3:12,2,15)]

all.values <- mbt.interp[sample(x = nrow(mbt.interp),size = 2000),
                           c(3:12,2)]


all.pc.tadp <- tsclust(all.values, type = "tadpole", k = 20,
                            trace = TRUE, 
                            control = tadpole_control(dc = 10000,
                                                      window.size = 1L))
plot(all.pc.tadp)

## What happens if we take the diff approach on the frequencies


freq.X.pc.tadp <- tsclust(freq.X[sample(x = nrow(mbt.interp),size = 2000),], type = "tadpole", k = 20,
                       trace = TRUE,
                       control = tadpole_control(dc = 1000, window.size = 2L))
plot(freq.X.pc.tadp)


### Distance Matrix using dtw distance,
## These helper functions comes really handy (maybe)


dtwpairdist <- function(...) {
  dtw(distance.only=TRUE,...)$distance;
}

dtwDist <- function(mx,my=mx,...) {
  mye<-function(y,x,FUN,...) {
    apply(x,1,FUN,y,...);
  }
  
  apply(my,1,mye,mx,dtwpairdist,...);
}

################################


lala <- dtwDist(mysub)



### Cluster Distance First ######

pru <-hclust(dist(data.frame(mbt.interp$duration,mbt.interp$bandwidth)))
dend <- as.dendrogram(pru)
dend <- dend %>%
  color_branches(k = 5) %>%
  set("branches_lwd", c(2,1,2)) %>%
  set("branches_lty", c(1,2,1))
dend <- color_labels(dend, k = 5)
plot(dend)


ggplot(mbt.interp,
       aes(duration/1000, bandwidth/10,
           color=label))+
  geom_point(alpha=.5)+
  facet_wrap(~label) + 
  xlab('Duration(ms)')+
  ylab('Banwidth(Hz)')+
  xlim(1,500)+theme_bw()


dur.cluster<-kmeans(data.frame(mbt.interp$duration),centers = 5)

ban.dur.cluster<-kmeans(scale(data.frame(mbt.interp$duration,
                                         mbt.interp$bandwidth)),centers = 5)

mbt.interp$cluster<-dur.cluster$cluster

mbt.interp$ban.dur.cluster<-factor(ban.dur.cluster$cluster)

ggplot(mbt.interp,
       aes(duration/1000, bandwidth/10,
           color=file))+
  geom_point(alpha=.5)+
  facet_wrap(~label) + 
  xlab('Duration(ms)')+
  ylab('Banwidth(Hz)')+
  xlim(1,500)+theme_bw()
+
  scale_color_manual(values=brewer.pal(n=15, "Set1"))


######### Add adaptors ##############



########### ADDING SPACERS  ###########
# This seems to be working
lb_improved(c(0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0), c(0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0), window.size = 2,norm = "L2")

