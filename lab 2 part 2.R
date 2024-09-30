#Reading in data set
abalone <- read.csv("C:/Users/Tat/Desktop/Data Analytics 2024/Lab 2 part 2/abalone.data")
abalone.names <- read.csv("C:/Users/Tat/Desktop/Data Analytics 2024/Lab 2 part 2/abalone.names")

#Calling naive bayes package
install.packages("e1071")
library(e1071)

#inserting the column names
colnames(abalone) <-c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght',
                      'viscera_wieght', 'shell_weight', 'rings' ) 
#age group
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels=c("young", "adult", "old"))





#EXAMPLE 1

#naive bayes
#using the columns 2-4 (inclusive) and comparing to the sex 
classifier.abalone <- naiveBayes(abalone[,2:4], abalone[,1])
table(predict(classifier.abalone, abalone[,-1]),abalone[,1],dnn=list('predicted', 'actual'))
classifier.abalone$apriori

#going to plot length first
classifier.abalone$tables$length
plot(function(x) dnorm(x, 0.579, 0.086), -1,2, col="pink", main= "Length Distribution for 3 Diff Sexes")
curve(dnorm(x,, 0.428, 0.109), add= TRUE, col="cyan")
curve(dnorm(x, 0.561, 0.103), add= TRUE, col= "purple")

#now plotting diameter
classifier.abalone$tables$diameter
plot(function(x) dnorm(x, 0.455, 0.071),-0.5,1, col="orange", main ="Diameter Distribution for 3 Diff Sexes")
curve(dnorm(x,0.326, 0.088), add= TRUE, col="red")
curve(dnorm(x, 0.439, 0.084),add= TRUE, col="green")

# plotting height
classifier.abalone$tables$height
plot(function(x) dnorm(x, 0.158, 0.040), -0.2,0.5, col="darkseagreen4", main = "Height Distribution for 3 Diff Sexes", ylim=c(0,15))
curve(dnorm(x, 0.108, 0.032), add= TRUE,  col ="darkorchid")
curve(dnorm(x, 0.151, 0.0348), add= TRUE, col= "coral3")



# EXAMPLE 2 KNN WITH IRIS
iris <- iris
normalize <- function(x){return ((x- min(x)) / (max(x) - min(x)))}
iris[1:4] <- as.data.frame(lapply(iris[1:4],normalize))
#prof said to ignore normalizing
150*0.7
#105 is 70%
s_iris <- sample(150, 105)
#creating train and test sets
iris.train <- iris[s_iris, ]
iris.test <- iris[-s_iris, ]
sqrt(105)
#kNN analysis
knn(train = iris.train[1:7], test = iris.test[1:7], cl = iris$Sepal.Length, k = 11)
help("knn")
# it keeps telling me it cant find the knn function even tho the package is loaded