library("MVA")
data=heptathlon 
#########################
#########Normality###########
#########################
layout(matrix(1:4,1,4))
qqnorm(data[,"hurdles"], main = "hurdles"); qqline(hep[,"hurdles"])
qqnorm(data[,"highjump"], main = "highjump"); qqline(hep[,"highjump"])
qqnorm(data[,"shot"], main = "shot"); qqline(hep[,"shot"])
qqnorm(data[,"run200m"], main = "run200m"); qqline(hep[,"run200m"])
qqnorm(data[,"longjump"], main = "longjump"); qqline(hep[,"longjump"])
qqnorm(data[,"javelin" ], main = "javelin" ); qqline(hep[,"javelin" ])
qqnorm(data[,"run800m"], main =  "run800m"); qqline(hep[, "run800m"])
qqnorm(data[,"score"], main = "score" ); qqline(hep[,"score" ])
shapiro.test(data$hurdles)
shapiro.test(data$highjump)
shapiro.test(data$shot)
shapiro.test(data$run200m)
shapiro.test(data$longjump)
shapiro.test(data$javelin)
shapiro.test(data$run800m)
shapiro.test(data$ "score")

d = apply(x, 1, function(x) t(x - cm) %*% solve(S) %*% (x - cm))
plot(qc <- qchisq((1:nrow(x) - 1/2) / nrow(x), df = 8),
     sd <- sort(d),xlab = expression(paste(chi[8]^2, " Quantile")),
     abline(a = 0, b = 1))
###############var and cor 
S= cov(x)
c=cor(data)
library("corrplot")
corrplot(c,method="color",cl.length=5)
#########################
#########Scatter plot###########
#########################
################## scatter plot 
pairs(data)
highjum =data$highjump
hurdle=data$hurdles
plot(highjum ~ hurdle, data = data, xlab = "highjum", ylab = "hurdle")
########## scatter plot by function
fun1 <- function(){
  x= readline(prompt="Enter number: ")
  y  = readline(prompt="Enter number: ")
  
  x <- as.numeric(unlist(strsplit(x, ",")))
  y <- as.numeric(unlist(strsplit(y, ",")))
  data=heptathlon 
  b=names(data)
  if (x<9 & y<9 & x!=y){
    
    out1 <- plot(data[,x]~data[,y],data = data, xlab = b[x], ylab = b[y])
    
    return(out1)
  }
  else{
    print("error")
  }
}
fun1() 
2#########scatterplot by for 
library("MVA")
> data=heptathlon 
> b=names(data)
> for (i in 1:8){
  +     for(j in 1:8){
    +         if(i != j){
      +             plot(data[,i]~data[,j],data = data, xlab = b[i], ylab = b[j])
      +         }
    +     }
  + }
> 
  
  #######rug
  plot(highjum ~ hurdle, data = data, xlab = "highjum", ylab = "hurdle")

rug(data$highjump, side = 2)
rug(data$hurdles ,side = 1)

##########scotterplot by hist and boxplot 

layout(matrix(c(2, 0, 1, 3), nrow = 2, byrow = TRUE),
       widths = c(2, 1), heights = c(1, 2), respect = TRUE)
xlim <- with(data, range(highjum)) * 1.1
ylim <- with(data, range(hurdle)) * 1.1

plot(highjum ~ hurdle, data =data, 
     xlab ="highjum", ylab = "hurdle")
with(data, text(highjum, hurdle, 
                labels = abbreviate(row.names(data))))
with(data, hist(highjum, main = "", xlim = xlim))
with(data, boxplot(highjum))
########convex hall of bivariate data 
(hull <- with(data, chull(hurdles, highjump)))
with(data,plot(hurdles, highjump, pch = 1, xlab ="hurdles", ylab =" highjump"))
with(data, polygon(hurdles[hull],  highjump[hull], density = 15, angle = 30))
cor(data$hurdles,data$highjump)
with(data, cor(hurdles[-hull],highjump[-hull]))
#######chi-plot
with(data,plot(hurdles, highjump, xlab ="hurdles", ylab =" highjump", cex.lab = 0.9))
with(data, chiplot(hurdles, highjump))
####################Bubble plot 
data2=(data[,1:2])
ylim <- with(data, range(hurdles)) * c(0.95, 1)
plot(hurdles ~ highjump, data = data, xlab ="highjump" , ylab ="hurdles" , pch = 10, ylim = ylim)
with(data, symbols(highjump,hurdles, circles = run200m, inches = 0.5, add = TRUE))
######STATR
stars(data)
######CHERNOF
library("aplpack")
faces(data,cex = 0.9)
##########PAIRS
pairs(data,
      panel = function (x, y, ...) {
        points(x, y, ...)
        abline(lm(y ~ x), col = "grey")
      }, pch = ".", cex = 1.5)

###################################################
plot of bivariate density 
###################################################
persp(x = HPS$x1, y = HPS$x2, z =HPS$fhat,
      xlab = "hurdles",
      ylab = "highjump",
      zlab = "density")

#######################################three-dimensional scatterplot 
library("scatterplot3d")
with(data, scatterplot3d(hurdles, highjump, shot, type = "h", angle = 55))

################################### Scatterplot by divide shot variable in 2 part 
library("lattice")
plot(xyplot(hurdles ~ highjump| cut(shot, 2), data = data))
#########################
#########PCA###########
#########################
v=cov(data[,-8])
c=cor(data[,-8])
###############pca by cov
pca=princomp((covmat=v))
summary(pca,loading=TRUE)
######################pca by cor
pca2=princomp((cormat=c))
summary(pca2,loading=TRUE)
################ scree daig pca by cov
plot(pca$sdev^2, xlab = "Component number",
     ylab = "Component variance", type = "l", main = "Scree diagram")
###################### scree diag pca by cor
plot(pca2$sdev^2, xlab = "Component number",
     ylab = "Component variance", type = "l", main = "Scree diagram")
##########################pca by omit outlier
heptathlon <- heptathlon[-grep("PNG", rownames(heptathlon)),]
score <- which(colnames(heptathlon) == "score")
heptathlon_pca <- prcomp(heptathlon[, -score], scale = TRUE)
print(heptathlon_pca)
summary(heptathlon_pca)
plot(heptathlon_pca)
a1 <- heptathlon_pca$rotation[,1]
a1
center <- heptathlon_pca$center
scale <- heptathlon_pca$scale

hm <- as.matrix(heptathlon[,-score])
drop(scale(hm, center = center, scale = scale) %*%
       + heptathlon_pca$rotation[,1])
#########################
#########Factor Anlaysis###########
#########################

data=data[ ,-8]
javab1=factanal(data, factors = 1, method ="mle")
javab1
javab2=factanal(data, factors = 2, method ="mle")
javab2
javab3=factanal(data, factors = 3, method ="mle")
javab3
scores=factanal(data, factors = 3, method ="mle",scores="regression")
sc=scores$scores
plot(sc[,1], sc[,2], type = "n", xlab = "Factor 1", ylab = "Factor 2")
text(sc[,1], sc[,2], abbreviate(rownames(data), 5), cex = .5)
plot(sc[,2], sc[,3], type = "n", xlab = "Factor 2", ylab = "Factor 3")
text(sc[,2], sc[,3], abbreviate(rownames(data), 5), cex = .5)
###########ratate
factanal(data, factors = 3, method ="mle",rotation="varimax")
factanal(data, factors = 3, method ="mle",rotation="promax")

###################pca 
m=cor(data)

pca2=princomp((cormat=m))
summary(pca2,loading=TRUE)

plot(pca2$sdev^2, xlab = "Component number",
     ylab = "Component variance", type = "l", main = "Scree diagram")

#########################
#########CFA###########
#########################
#######################CFA
power_model <- specify.model(file.choose())
power_sem <- sem(power_model,cov(data), 25)
summary( power_sem)
library(DiagrammeR)
pathDiagram(power_sem , file = "power_model", 
            ignore.double = FALSE, edge.labels = "both")
round(power_sem$S - power_sem$C, 3)
##############################
#########################
#########Clustering ###########
#########################
data=data[,-8]
data2=scale(data,center=FALSE,scale=TRUE)
dm=dist(data2)
###########################agglu 
layout(matrix(1:3,1,3))
plot(hclust(dm,method="single"))
plot(hclust(dm,method="complete"))
plot(hclust(dm,method="average"))
data2
###############remove louna
data3 <-data2[-grep("PNG", rownames(data2)),]
dm1=dist(data3)
layout(matrix(1:3,1,3))
plot(cs<-hclust(dm1,method="single"))
plot(cc<-hclust(dm1,method="complete"))
plot(ca<-hclust(dm1,method="average"))
#####################plot by pca
data_pc <- princomp(dm1, cor = TRUE)
xlim <- range(data_pc$scores[,1])
layout(matrix(1:4, nr = 2), height = c(2, 1))
plot(cc <- hclust(dm1, method = "complete"), main = "Complete")
abline(h = .37, col = "lightgrey")
plot(data_pc$scores[,1:2], type = "n", xlim = xlim, ylim = xlim,
     xlab = "PC1", ylab = "PC2")
lab <- cutree(cc, h = .37)  
text(data_pc$scores[,1:2], labels = lab, cex=0.6)     
plot(ca <- hclust(dm1, method = "average"), main = "Average")
abline(h = .2, col = "lightgrey")
plot(data_pc$scores[,1:2], type = "n", xlim = xlim, ylim = xlim,
     xlab = "PC1", ylab = "PC2")
lab <- cutree(ca, h = .2)                             
text(data_pc$scores[,1:2], labels = lab, cex=0.6) 

##################K-means
n <- nrow(data2)
wss <- rep(0, 6)
wss[1] <- (n - 1) * sum(sapply(data2, var))
for (i in 2:6)
  wss[i] <- sum(kmeans(data2,
                       centers = i)$withinss)
plot(1:6, wss, type = "b", xlab = "Number of groups",
     ylab = "Within groups sum of squares")
kmclus=kmeans(data3, centers = 3)
kmclus$cluster
library("lattice")
levelplot(as.matrix(dm1), xlab = "hepth Number",
          ylab = "hepth Number")
data_pca<- prcomp(data3)
#################
library("mclust")
mc <- Mclust(data3)
mc$classification
mc$z
mc$data
plot(mc,data3, what = "BIC", col = "black")
#############vis   
library("flexclust")
k <- cclust(data3, k = 3, save.data = TRUE)
plot(k, project = prcomp(data), hull = FALSE, col = rep("black", 3),
     xlab = "PC1", ylab = "PC2")  
set.seed(15)
c5 <- cclust(data3, k = 3, save.data = TRUE)
stripes(c5, type = "second", col = "black")


