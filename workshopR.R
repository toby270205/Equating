rm(list=ls()) #remove all current variables 
library(TAM)
setwd("C:\\G_MWU\\R Projects\\Equating")
resp1 <- read.csv("test1.csv")
resp2 <- read.csv("test2.csv")
mod1 <- tam(resp1)
mod1$xsi
plot(mod1,fix.devices=TRUE,overlay = TRUE,observed = FALSE)

mod2 <- tam(resp2)
iname <- colnames(resp1) %in% colnames(resp2)
link1 <- mod1$xsi[iname,]
link2 <- mod2$xsi[row.names(link1),]
xsi2.adj <- link2$xsi - mean(link2$xsi) + mean(link1$xsi)
p1 <- (link1$xsi+xsi2.adj)/2-sqrt(link1$se.xsi^2+link2$se.xsi^2)
p2 <- (link1$xsi+xsi2.adj)/2+sqrt(link1$se.xsi^2+link2$se.xsi^2)
plot(link1$xsi,xsi2.adj)
lines(p1,p2)
lines(p2,p1)
text(link1$xsi,xsi2.adj,labels=rownames(link1),cex=0.7,pos=3)
diff <- link1$xsi-xsi2.adj
diff

linked <- data.frame(link1,link2,xsi2.adj,diff,p1,p2)
#remove some items
rem <- c("S16","S19","S24","S35")
link1 <- link1[!rownames(link1) %in% rem,]
link2 <- link2[!rownames(link2) %in% rem,]

xsi2.adj <- link2$xsi - mean(link2$xsi) + mean(link1$xsi)
p1 <- (link1$xsi+xsi2.adj)/2-sqrt(link1$se.xsi^2+link2$se.xsi^2)
p2 <- (link1$xsi+xsi2.adj)/2+sqrt(link1$se.xsi^2+link2$se.xsi^2)
plot(link1$xsi,xsi2.adj)
lines(p1,p2)
lines(p2,p1)
text(link1$xsi,xsi2.adj,labels=rownames(link1),cex=0.7,pos=3)
diff <- link1$xsi-xsi2.adj

shift <- - mean(link2$xsi) + mean(link1$xsi) 
linked <- data.frame(link1,link2,xsi2.adj,diff,p1,p2)
rownames(linked)

x <- which(rownames(mod1$xsi) %in% rownames(linked))
y <- which(rownames(mod2$xsi) %in% rownames(linked))
xsi.fixed <- cbind(y,mod1$xsi$xsi[x])
mod3 <- tam(resp2,xsi.fixed=xsi.fixed) #anchor parameters
shift

Abil2 <- tam.wle(mod2)
PersonAbility <- Abil2$theta
mean(PersonAbility)
var(PersonAbility)
mod2$beta
mod2$variance

library(WrightMap)
itemdiff <- mod2$xsi$xsi
wrightMap(PersonAbility,itemdiff,item.side=itemClassic)
ctt <- tam.ctt(resp2,PersonAbility)
write.csv(ctt,"tamctt.csv")

colnames(resp1)[c(16,19,24,35)]<-c("S16a","S19a","S24a","S35a")
library(plyr)
resp4 <- rbind.fill(resp1,resp2)
testname <- c(rep(1,nrow(resp1)),rep(2,nrow(resp2)))
mod4 <- tam(resp4,Y=testname)
mod5 <- tam(resp4)
