################################################################################################
#########################################First class ###########################################
################################################################################################

#################

a = 2 
a
a <- 2
a
2 -> a

#################

2+2 
2-2
2*2
2/2

#################

2^2
sqrt(2)
2**0.5
exp(4)
log(4)
mean(c(2,3))
min(2,3)
max(2,3)
abs(-4)
sign(-10)      
round(4.56789, 2)
ceiling(4.56789)
as.integer(4.56789)

################

rep(1, 10)
seq(from = 0, to = 20, by =1)
seq(from = 0, to = 20, by =2)
seq(from = 1, to = 21, by =2)
1980:2023

###############
c(1,3,2,0,4,5)
c(1,3,2,0,4,5)[2]
sd(c(1,3,2,0,4,5))
sum(c(1,3,2,0,4,5))
median(c(1,3,2,0,4,5))
sort(c(1,3,2,0,4,5))
rank(c(1,3,2,0,4,5))
c("abcd","a", "ab", "abc")
rank(c("abcd","a", "ab", "abc"))
sort(c("abcd","a", "ab", "abc"))


##########################
matrix(1:6, nrow = 2)
matrix(1:6, ncol = 3)
matrix(1:6, nrow = 2,byrow = T) 
nrow(matrix(1:6, nrow = 2))
ncol(matrix(1:6, nrow = 2))
matrix(1:6, nrow = 2)[1,3]
matrix(1:6, nrow = 2)[,1]
matrix(1:6, nrow = 2)[2,]
matrix(1:6, nrow = 2)[1:2, c(1,3)] 
matrix(1:6, nrow = 2)[1:2, c(1,3)]

########################

t(matrix(1:6, nrow = 2))
dim(matrix(1:6, nrow = 2))
matrix(1:6, nrow = 2)%*%matrix(1:6, nrow = 3)
det(matrix(1:6, nrow = 2)%*%matrix(1:6, nrow = 3))
eigen(matrix(1:6, nrow = 2)%*%matrix(1:6, nrow = 3))
solve(matrix(1:6, nrow = 2)%*%matrix(1:6, nrow = 3))

#########################

A <- matrix(c(0.5,2),nrow=2)
A

B <- matrix(c(1,1,0,1),nrow=2)
B

crossprod(A,B)
t(A)%*%B

kronecker(A,B)



########################

c(1,3,2,0,4,5)
matrix(c(1,3,2,0,4,5), nrow = 2)
matrix(c(1,3,2,0,4,5), nrow = 2, byrow=F)
matrix(c(1,3,2,0,4,5), nrow = 2,byrow=T)

########
diag(3)


########

c(1,3,2)
c(0,4,5)
rbind(c(1,3,2),c(0,4,5))
######

cbind(c(1,3,2),c(0,4,5))

#######

X <- cbind(1,c(1,4,2,5))
X
B <- solve(crossprod(X,X))
B

Bautre <- solve(t(X)%*%X)
Bautre

dim(B)
diag(B)

Y <- matrix(c(3,6,4,7),nrow=4)
Y

OLS <- B%*%crossprod(X,Y)
OLS

OLSautre <- B%*%t(X)%*%Y
OLSautre

###############

setwd("C:/Users/Yao Thibaut Kpegli/Desktop/ENS Paris Saclay/R2023_2024")
getwd()

###############

example <- data.frame(one = 1:10, two = 11 :20, three = 21:30)
View(example)

exampleA <- as.data.frame(matrix(1:30, ncol=3))
names(exampleA)

names(exampleA)<- c("one", "two", "three")
View(exampleA)



##############

mean(example$one)
summary(example$one)

ratio <- example$one/example$three
example$ratio <- example$one/example$three

View(example)
names(example)[names(example) == "ratio"] <- "new"
example$one[example$one==3] <- 5

##########
attach(example) 

mean(one)
min(two)
sd(two)

detach(example)

with(example, mean(two))

###########

example$ratio <- (example$one)/(example$two)
example$dumy_ch <- ifelse(example$one <= 5, "Low", "High")
example$dumy_num <- ifelse(example$one <= 5, 1, 0)

View(example)

###########

set_obs<-subset(example, two<=16)
View(set_obs)
set_var<-subset(example, select = -two)
set_varA<-subset(example, select = c("one","three"))
set_obs_var<-subset(example, two<=16, select = -two)

###########

write.table(example, file="example.txt", col.names=TRUE)

write.table(example, file="exampleB.txt", col.names=F)

###########

install.packages("writexl")
library("writexl")
write_xlsx(example, "example.xlsx")


##########

write.csv(example, "example.csv")

##########

text_file <- read.table("example.txt", header=TRUE)
View(text_file)
##########

install.packages("xlsx")
library(xlsx)
Excel_file <- read.xlsx("example.xlsx", sheetIndex=2, sheet="Sheet1")


##########
Excel_csv <- read.csv("example.csv")




###########

data("Journals", package = "AER") # aller dans le package AER et installer la base de donnée journals#
names(Journals) # permet d'afficher les noms de la base de données journal#
Journals$citeprice <- Journals$price/Journals$citations # créer la variable citerprice dans la bd journals
names(Journals) # permet d'afficher les noms des variables dela base de données journal
attach(Journals)


X<-cbind(1,log(citeprice))
Y<-log(subs)
beta<-solve(crossprod(X,X))%*%t(X)%*%Y
beta

e<-Y-X%*%beta
e
N<-nrow(Journals)
N

vcm<- (kronecker(crossprod(e,e)/(N-2),solve(crossprod(X,X))))
vcm
sd <- sqrt(diag(vcm))
sd 

tstat<-beta/sd  # t-stat egal ?  beta/ecarttype

tstat


##### check result with lm command
logsubs<-log(subs)
logpc<-log(citeprice)
lms= lm(logsubs~logpc, data = Journals)
summary(lms)


######## Join 


# Vertical join
v1=data.frame( Num  =c(1,-1,0,3,0,2), Str=c("L1","L2","L3","M1","M2","D1"))

v2=data.frame(Num=c(10,20,30,40), Str=c("L1","L2","M1","D3"))

v <- rbind(v1,v2)


# Horizontal join

h1=data.frame(id =c(1,2,3,4,5,6), Num  =c(1,-1,0,3,0,2), Str=c("L1","L2","L3","M1","M2","D1"))

h2=data.frame(id =c(1,2,3,4,5,7), Name=c("Rac","Elo","Fra","Hon","Hor","Ben"))

merge_inner <- merge(x=h1,y=h2,by="id")

merge_left <- merge(x=h1,y=h2,by="id", all.x=T)

merge_right <- merge(x=h1,y=h2,by="id", all.y=T)
merge_outer <- merge(x=h1,y=h2,by="id", all=T)



################################################################################################
#########################################Second class ##########################################
################################################################################################

######## plot

install.packages("ggplot2")
library("ggplot2")

ggplot(Journals, aes(log(citeprice),log(subs))) +
  geom_point()

ggplot(Journals, aes(log(citeprice),log(subs))) +
  geom_point(aes(color = pages ))

ggplot(Journals, aes(log(citeprice),log(subs))) +
  geom_point(aes(color = pages )) +
  geom_smooth(method = "lm", se=F)

ggplot(Journals, aes(log(citeprice),log(subs))) +
  geom_point(aes(color = pages )) +
  geom_smooth(method = "lm", se=T)

##

ggplot(Journals) +
  geom_bar(aes(society))

##
ggplot(Journals) +
  geom_histogram(aes(log(subs))) 

## 

ggplot(Journals) +
  stat_ecdf(aes(log(subs)))

##

ggplot(Journals) +
geom_density(aes(log(subs)))

##

ggplot(Journals) +
geom_boxplot(aes(subs))


## Illustration of the monotonicty problem of random utility models (Apesteguia and Ballester, 2018)

f <- function(x){
  return(1/(1+ exp(5^x - 1^x - (60^x - 1^x)*0.05)))
}

ggplot(data.frame(x=c(0,2)), aes(x=x)) +
  stat_function(fun=f) +
  labs(title="", x =expression(paste(alpha)), y = expression(paste(P(A))))



##########################

f <- function(alpha){
  y <- alpha^2
  return(y)
}

f(-2:2)

f(seq(from=-2, to=2, by=0.1))

ggplot(data.frame(x=c(-2,2)), aes(x=x)) +
  stat_function(fun=f) +
  labs(title="", x =expression(paste(alpha)), y = expression(paste(P(alpha))))


f <- function(x,y){
  
  z<- x^2+y
  
  return(z)
}

f(0,-1)
f(0:2,-1:1)


f <- function(x){
  
  z <- x[1]^2+x[2]
  
  return(z)
}

f(c(0,-1))
f(c(1,1))


######


v1<- c(-1:1)
v2 <- c(0:2)
expand.grid(v1,v2)

####


x<- seq(from=-1, to=1,by=0.01)
y<- seq(from=-1, to=1,by=0.01)
data <- data.frame(expand.grid(x,y))
names(data) <- c("x","y")
data$z <- (data$x)^2 + (data$y)^2


ggplot(data, aes(x,y)) +
  geom_point(aes(color = log(z) ))+
  scale_color_gradientn(colours = rainbow(4))


#install.packages("plot3D")
library("plotly")
plot_ly(x=data$x, y=data$y, z=data$z, type="scatter3d", color=data$z, mode="markers") 



####### optimization

# 1

f <- function(x){
  
  y= x**2
  
  return(y)
}

initial_value <- 5
optim(initial_value, f, method="BFGS")


# 
f <- function(x){

y= x - 0.5*x**2

return(-y)
}

initial_value <- 5
optim(initial_value, f, method="BFGS")


#2

f <- function(x){
  
  y = x[1]**2+x[2]**2
  
  return(y)
  
}

initial_value <- c(10, 10)
  
optim(initial_value, f, method="BFGS")




# OLS (similar idea to write MLE code)

Y <- log(Journals$subs)
X  <- log(Journals$price/Journals$citations)

ols <- function(z){
sse = sum((Y - z[1]*X-z[2])^2)
return(sse)
  
}

initial_value <- c(1, 1)

optim(initial_value, ols, method="BFGS")





####### loop

n <- nrow(Journals)
count <- 0
for (i in 1:n) {
  if(Journals$society[i] == "yes"){count = count+1}
}
print(count)


i <- 1
while (Journals$society[i] == "no") {
  i = i+1
  print(i)
}
print(i-1)


### Bias

rnorm(10,0,1)
runif(10,0,1)
v <- 1:10
v
sample(v)

set.seed(123456789)
rep <- 1000
n=1000
coef <-c(2,1)
stock <- matrix(rep(NA,2*rep), ncol=2)
stock
for (i in 1:rep){
sim <- data.frame(x=runif(n,-5,5),er=rnorm(n,0,2))
sim$y = coef[1]*sim$x + coef[2] + sim$er
lms= lm(y~x, data = sim)
stock[i,] <- lms$coefficients
}

stock

Mean <- c(mean(stock[,1]),mean(stock[,2])) 
Mean

### 

x <- runif(100000000,-10,10)
f <- (1/(sqrt(2*pi)))*exp(-0.5*x^2)
f
f_bar <-mean(f)
f_bar
approx <- f_bar*20
approx
pi


####
pid <- data.frame(x=runif(1000000,-1,1),y=runif(1000000,-1,1))

pid$area <- (pid$x)^2+(pid$y)^2

pid$dum <- ifelse(pid$area <= 1, 1, 0)
pid$Area <- ifelse(pid$area <= 1, "Circle", "Square")

pi_approx <- 4*mean(pid$dum)

pi_approx
pi


ggplot(pid, aes(x,y)) +
  geom_point(aes(color = Area ))


