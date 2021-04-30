pdf("mygraph.pdf")
attach(mtcars)
plot(wt,mpg)
abline(lm(mpg~wt))
title("regression of mpg on weight")
detach(mtcars)
dev.off()
dev.new()


dose<-c(20,30,40,45,60)
drugA<-c(16,20,27,40,60)
drugB<-c(15,18,25,31,40)
plot(dose,drugA,type = "b")

opar<-par(no.readonly = TRUE)
par(lty=2,pch=17)
plot(dose,drugA,type="b")
par(opar)

#符号和线条
plot(dose,drugA,type = "b",lty=3,lwd=3,pch=15,cex=2)

##颜色
n<-10
mycolor<-rainbow(n)
pie(rep(1,n),lablels=mycolor,col = mycolor)
mygrays<-gray(0:n/n)
pie(rep(1,n),labels = mycolor,col = mygrays)


library(RColorBrewer)
n<-7
mycolors<-brewer.pal(n,"set1")
barplot(rep(1,n),col=mycolor)

dose<-c(20,30,40,45,60)
drugA<-c(16,20,27,40,60)
drugB<-c(15,18,25,31,40)
par(pin=c(2,3))
par(lwd=2,cex=1.5)
par(cex.axis=.75,font.axis=3)
plot(dose,drugA,type = "b",pch=19,lty=2,col="red")
plot(dose,drugB,type = "b",pch=23,lty=6,col="blue",bg="green")

plot(dose,drugA,type = "b",
     col="red",lty=2,pch=2,lwd=2,
     main = "this is a hypothetical data",
     xlab = "dosage",ylab = "drug response",
     xlim = c(0,60),ylim=c(0,70)
     )

##自定义坐标轴
x<-c(1:10)
y<-x
z<-10/x
par(mar=c(5,4,4,8)+0.1)
plot(x,y,type = "b",
     pch=21,col="red",
     yaxt="n",lty=3,ann=FALSE
     )
lines(x,z,type="b",pch=22,col="blue",lty=2)
axis(2,at=x,labels = x,col.axis="red",las=2)
axis(4,at=z,labels = round(z,digits=2),
     col.axis="blue",las=2,cex.axis=.7)
mtext("y=1/x",side=4,line = 3,cex.lab=1,las=2,col = "blue")
title("an example of greative axes",
      xlab="x values",
      ylab = "y=x")
par(opar)
  
#参考线
abline(h=c(1,5,7))
abline(v=seq(1,10,2),lty=2,col="blue")

#图例
dose<-c(20,30,40,45,60)
drugA<-c(16,20,27,40,60)
drugB<-c(15,18,25,31,40)
opar<-par(no.readonly = TRUE)
par(lwd=2,cex=1.5,font.lab=2)
#绘制图形
plot(dose,drugA,type = "b",
     pch=15,lty=1,col="red",ylim=c(0,60),
     main = "drug a vs. drug b",
     xlab = "drug dosage",ylab="drug response")

lines(dose,drugB,type = "b",
      pch=17,lty=2,col="blue")
abline(h=c(30),lwd=1.5,lty=2,col="gray")

#添加次刻度线
install.packages("Hmisc")
library(Hmisc)
minor.tick(nx=3,ny=3,tick.ratio = 0.5)

#添加图例
legend("topleft",inset = .05,title = "drug type",c("a","b"),
       lty = c(1,2),pch = c(15,17),col=c("red","blue"))
par(opar)

#文本标注
attach(mtcars)
plot(wt,mpg,
     main = "mileage vs. car weight",
     xlab = "weight",ylab = "mileage",
     pch=18,col="blue")
text(wt,mpg,
     row.names(mtcars),
     cex = 0.6,pos=4,col="red")
detach(mtcars)

opar<-par(no.readonly = TRUE)
par(cex=1.5)
plot(1:7,1:7,type="n")
text(3,3,"example of mono-spaced text")
text(4,4,"example of default text")
text(5,5,"example of serif text")
par(opar)

#数字标注
#图形的组合
attach(mtcars)
opar<-par(no.readonly = TRUE)
par(mfrow=c(2,2))#两行两列排列图形
plot(wt,mpg,main="scatterplot of wt vs. mpg")
plot(wt,disp,main="scatterplot of wt vs.disp")
hist(wt,main = "histogram of wt")
boxplot(wt,main="boxplot of wt")

#三行一列排列图形
attach(mtcars)
opar<-par(no.readonly = TRUE)
par(mfrow=c(3,1))
hist(wt)
hist(mpg)
hist(disp)
par(opar)
detach(mtcars)

#1123排列图形
attach(mtcars)
layout(matrix(c(1,1,2,3),2,2,byrow=TRUE))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)
#图形宽度，高度
attach(mtcars)
layout(matrix(c(1,1,2,3),2,2,byrow=TRUE),
       widths=c(3,1),heights = c(1,2))
hist(wt)
hist(mpg)
hist(disp)

#图形布局的精细布局
#设置散点图
opar<-par(no.readonly = TRUE)
par(fig=c(0,0.8,0,0.8))
plot(mtcars$wt,mtcars$mpg,
     xlab="miles per gallon",
     ylab = "car weight")
#在上方添加箱线图
par(fig=c(0,0.8,0.55,1),new=TRUE)
boxplot(mtcars$wt,horizontal = TRUE,axes=FALSE)
#在右侧添加箱线图
par(fig=c(0.65,1,0,0.8),new=TRUE)
boxplot(mtcars$mpg,axes=FALSE)
mtext("enhanced scatterplot",side=3,outer=TRUE,line =-3)
