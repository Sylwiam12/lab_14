library(cluster) 
library(magrittr)
# install.packages("GGally")
library("GGally")
#exercise 1

lst <- 1:10
lst %<>% log2() %<>% sin() %<>% sum() %<>% sqrt()
print(lst)

data(iris)
print(head(iris,4))

iris_mean <- iris %>% 
    aggregate(. ~ Species, ., mean)
print(iris_mean)

#exercise 2
library("ggplot2")
plt <- ggplot(data=iris, aes(x = Petal.Width)) + geom_histogram(aes(fill=Species, color=Species)) + geom_vline(data=iris_mean, aes(xintercept=Petal.Width, color=Species)) + labs(x='x', y='y', title='histogram dla petal.width')
ggsave("/home/iris1.jpg", plot = plt)


plt2 <- ggpairs(data=iris,aes(color=Species))
ggsave("/home/iris2.jpg", plot = plt2 )

#exercise 3

x <- iris[,1:4]
y <- iris[,5]

sqrt_s <- c()

for (k in 1:10){
    result <- kmeans(x, k) 
    sqrt_s <- append(sqrt_s, result$tot.withinss) 
}

plt3 <- ggplot(data.frame(iteration = 1:length(sqrt_s), value = sqrt_s), aes(x = iteration, y = sqrt_s)) + geom_line()
ggsave("/home/iris3.jpg", plot = plt3)

result <- kmeans(x, 3) 
plt4 <- ggplot(iris, aes(x = Sepal.Width, y = Petal.Width, color = result$cluster)) + geom_point()
ggsave("/home/iris4.jpg", plot = plt4)

plt5 <- ggplot(iris, aes(x = Sepal.Width, y = Petal.Width, color = Species)) + geom_point()
ggsave("/home/iris5.jpg", plot = plt5)
