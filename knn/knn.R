startN <- 1 #Какие признаки будем учитывать? Со startN по endN.
endN <- 4
dist = function(x, y) {
  return(sqrt(sum((x-y)^2)))
}
s_dst = function(iris, point){
  a = c()
  for (i in (1:length(iris[,1]))){
    a[i] = dist(point, iris[i,startN:endN])
    
  }
  return(iris[order(a), ])
}
knn = function(point, dataset, k=5){
  sortedDataset = s_dst(dataset, point[startN:endN])
  filtereddataset = sortedDataset[1:k, 5]
  return (names(which.max(table(filtereddataset))))
  
}

testPoint = c(0, 0, 2, 2.5)
class<-knn(testPoint, iris, 5)
colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)
points(testPoint[3], testPoint[4], pch = 21, bg = colors[class])
