## This R Notebook shows code for Textile example given 

#Textile application

library(EBImage)
library(keras)
require(spc4sts)
require(tiff)

## Fit the model for IC image
require(textile) # This is rda file of image data
data('trainImg')
set.seed(1)
model_textile0<-surfacemodel(trainImg, nb=15, trim.vars=FALSE, stationary.test = TRUE, verbose = TRUE)

## Check for correct nb size.
showNb(model_textile0, 'importance')
model_textile0

## Since we find nb=12 is better choice of nb, refit the model
## We will use this model for further investigation.
set.seed(2)
model_textile<-surfacemodel(trainImg, nb=12, trim.vars=TRUE, verbose = TRUE)

model_textile

## Construct CLs for both local and Global change monitoring.
data('icImgs')
cl3<-climit(imgs=icImgs, fa.rate = 0.05, 
            model=model_textile, type=1:2, stat='ad', 
            w=5, no_cores = 6, nD=5, verbose = TRUE)

#Show Control Limit value
cl3
plot(cl3)

#Phase2 monitoring
## We use 6 images with local defects.
## 47 images with only global changes obtained via randomly stretching
## the set of 94 Phase I images 15% in vertical direction.

data('ocImgs') # load six images containing local defects
ls3 <-list()
stat_ld <- stat_gc <-vector('numeric', 53)
for (j in 1:6) { ls3[[j]] <- monitoringStat(img=ocImgs[ , , j],
                                            model=model_textile, 
                                            cl=cl3) 
stat_ld[j] <-ls3[[j]]$localStat
stat_gc[j] <-ls3[[j]]$globalStat }

ocImgs2 <-icImgs[ , , id]
set.seed(9)
id <-sample(1:94, 47)
for (j in 1:dim(ocImgs2)[3]) {
  ocImgs2[ , , j]<-EBImage::resize(ocImgs2[ , , j], 288, 250)[1:250, ] #stretch images
  tmp <-monitoringStat(img=ocImgs2[ , ,j], model=model_textile, cl=cl3)
  stat_ld[j+6] <-tmp$localStat
  stat_gc[j+6] <-tmp$globalStat
}


## This plot shows the following:
## Chart shows existence of local defects in first set of 
## Phase II images (which is correspond to points 
## b/n 2 vert lines).
par(mfrow=c(2,1), mar=c(2.5,2.5,1,1), mgp=c(1.5,0.5,0))
plotcc(log(stat_ld),
       log(cl3$localStat$control.limit), 
       log(cl3$localStat$PIstats[id]))

abline(v=53.5, lty=2)

## This chart shows global change corresponding to points after 
## vertical dashed line.
plotcc(stat_gc, 
       cl3$globalStat$control.limit.trans_chi2, 
       cl3$globalStat$PIstats[id])
abline(v=53.5, lty=2)

## Monitoring Stat
tmp<- monitoringStat(img=ocImgs[ , , 1], model=model_textile, 
                     type=1:2, stat='ad', w=5, verbose= TRUE)

## Cluster Analysis
c(rep(1,47), rep(2,47))

plot(model_textile, type=1)

# Cluster Analsysis using KL and AKL dissimilarity matrices.
imgs3 <-array(0, dim= c(250, 250, 188))
imgs3 [ , ,1:94] <- icImgs
imgs3 [ , ,95:188]<- ocImgs2

D <-disMat(imgs3, nb=12, cp=0.001, 
           subsample= c(0.5, 0.25), verbose=TRUE)
clust_kl <-hclust(as.dist(D$KL))

clust_akl <-hclust(as.dist(D$AKL))

cutree(clust_kl, 2)
cutree(clust_akl,2)

##Comment: This cluster analysis divides 47 IC images and 47 
## other Global changes by (1,2)

#----- End ---- # 