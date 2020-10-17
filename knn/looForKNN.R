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
knn = function(point, dataset, k=5, errors=0){
  sortedDataset = s_dst(dataset, point[startN:endN])
  filtereddataset = c()
  for(i in 1:k){
    filtereddataset = sortedDataset[1:i, 5]
    result = names(which.max(table(filtereddataset)))
    if(point[5] != result){
      errors[i] = errors[i] + 1;
    }
  }
  
  return (errors)
  
}

loo = function(k, dataset=iris){
  errors = vector(mode = "double", length = 150)
  len = length(dataset[,1])
  for(i in 1:len){
    xk = dataset[i,];
    if(i!=1 && i!=len){
      xl = dataset[c(1:(i-1), (i+1):len),]
    }else if(i == 1){
      xl=dataset[2:len,]
    }else{
      xl=dataset[1:(len-1),]
    }
    errors <- knn(xk, xl, k, errors)
  }
  
  for(i in 1:k){
    errors[i] = errors[i]/len;
  }
  return (errors)
}
errors<-loo(150, iris)
print(errors);
plot(1:150,errors[1:150])
lines(1:150,errors[1:150])

