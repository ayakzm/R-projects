#1st lab in ML.
#BASIC COMMANDS
x <-c(1, 3, 2, 5)
x

y=c(1, 6, 2, 3)
y

#length() function shows the lenght of vectors
length(x)
length(y)
z=x+y
z

#The ls() function allows us to look at a list of all of the objects, such as data and functions, that we have saved so far. 
ls()

#The rm() function can be used to delete any that we don’t want.
rm(x, y)

#It is also possible to remove all objects at once
rm(list=ls())

#Creating a simple matrix
?matrix
x=matrix(data=c(1, 2, 3, 4), nrow=2, ncol=2)
x

#We can omit typing data, nrow and ncol
y=matrix(c(1, 2, 3, 4), 2, 2)
y

#byrow=TRUE matrix entries are filled by row, byrow=FALSE entries are filled by column
matrix ( c (1 ,2 ,3 ,4) ,2 ,2 , byrow = TRUE )

sqrt(y)
x^2

#rnorm() creates standard normal random variables with a mean of 0 and a standard deviation of 1
x=rnorm(50)

#mean and sd arguments can alter mean, standard deviation
y = x + rnorm (50 , mean =50 , sd =.1)
y

#Correation between variables
cor(x,y)

#Used to reproduce the exact same set of numbers. It takes an integer argument
set.seed(100)

mean()
var()
sd()

#GRAPHICS
#plot() is the main way of plotting data in R
x = rnorm (100)
y = rnorm (100)
plot (x , y ) #here is plot scatterplot x vs y
plot (x ,y , xlab =" this is the x - axis " , ylab =" this main isthe y - axis", main = "Plot of X vs Y")

#pdf() and jpeg() are functions used to save graphics in the format we need
pdf (" Figure . pdf ")
plot (x ,y , col =" green ")
dev.off() #indicates that we are done creating the chart
null device

# seq(a,b) makes a vector of integers between a and b
#The following command gives the sequence of 10 numbers equally spaced between 0 and 1
seq(0,1,length=10)
#a:b is a shorthand for seq(a,b)

#contour() function produces a contour plot in order to represent three-dimensional data
#It is like a topographical map. It takes three arguments:
# 1. A vector of the x values (the first dimension)
# 2. A vector of the y values (the second dimension), and
# 3. A matrix whose elements correspond to the z value (the third dimension) for each pair of (x,y) coordinates
y=x
f = outer (x ,y , function (x , y ) cos ( y ) /(1+ x ^2) )
contour (x ,y , f )
contour (x ,y ,f , nlevels =45 , add = T )
fa =( f - t ( f ) ) /2
contour (x ,y , fa , nlevels =15)


#image() works the same way as contour(), except that it produces a color-coded plot whose colors depend on the z value.
#this is known as a heatmap, sometimes used to plot temperature in weather forecasts

#persp() can be used to produce 3 dimensional plot. The arguments theta and phi control the angles at which the plot is viewed.
image (x ,y , fa )
persp (x ,y , fa )
persp (x ,y , fa , theta =30)
persp (x ,y , fa , theta =30 , phi =20)
persp (x ,y , fa , theta =30 , phi =70)
persp (x ,y , fa , theta =30 , phi =40)



#INDEXING DATA
A <- matrix(1:16, 4, 4)
A
A[2, 3]
dim(A)
A[c(1,3), c(2,4)]
A [1:3 ,2:4]
A [1:2 ,]
A [ ,1:2]
A [1 ,] #keep only the first row
#The use of a negative sign in the index tells R to keep all rows or columns except those indicated in the index
A[-c(1,3),]
A[-c(1,3), -c(1,3,4)]

#LOADING DATA
read.table() #importing a data set into R
write.table() #exporting data

install.packages("ISLR")
library(ISLR)
Auto=read.table("Auto.data") #Loads Auto file in a format of data frame
fix(Auto)
#Header=TRUE tells R that header contains variable names. Na.strings tells that cells with ? are missing values
Auto=read.table("Auto.data",header=T, na.strings="?")
fix(Auto)
dim(Auto)
na.omit(Auto) #removes rows with missing values
dim(Auto)
names(Auto) #displays names of the variables


#ADDITIONAL GRAPHICAL AND NUMERICAL SUMMARIES
plot(Auto$cylinders, Auto$mpg) # $ sign tells are to look for variables in Auto data set
attach(Auto) #alternative to the previous method
plot(cylinders, mpg)
cylinder=as.factor(cylinders) #transforms cylinders into qualitative data

plot(Auto$cylinder, Auto$mpg)
plot(Auto$cylinder, Auto$mpg, col="red")
plot(Auto$cylinder, Auto$mpg, col="aquamarine4", varwidth=T)
plot(Auto$cylinder, Auto$mpg, col="aquamarine4", varwidth=T, horizontal=T)
plot(Auto$cylinder, Auto$mpg, col="aquamarine4", xlab = "cylinder", ylab="MPG")


hist(mpg)
hist(mpg, col="aquamarine4")
hist(mpg, col="aquamarine4", breaks=30)


pairs(Auto, col="aquamarine4") #creates a scatterplot matrix, all variables
pairs(~mpg+displacement+acceleration+horsepower+weight, Auto, col="aquamarine4") #scatterplot matrix for subset of variables

plot(horsepower, mpg)
identify(horsepower, mpg, name)
?identify()

summary(Auto)
summary(mpg)

q() #for quitting R
savehistory() #save recent commands typed in the last session
loadhistory() #load commands typed in the last session
