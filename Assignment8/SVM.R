install.packages(data.table)
library(data.table) # allows us to use function fread


mySVM=function(X,Y,Xtest,Ytest,kernel='linear',method='gradient',num_iterations=75,learning_rate=1e-1,lamda=0.01)
{ X = X[1:2000,]
  Y = Y[1:2000]
  Xtest = Xtest[1:500,]
  Ytest = Ytest[1:500]
  training_acc = rep(0,num_iterations)

  n=dim(X)[1]
  p=dim(X)[2]+1
  X1 = cbind(rep(1, n), X)#intercept is included 

  Y=2*Y-1# y=1--->1 y=0--->-1

  ntest = dim(Xtest)[1]
  Xtest1 = cbind(rep(1, ntest), Xtest)
  Ytest=2*Ytest-1
  score_test = matrix(rep(0,ntest),nrow=ntest)

  K = matrix(0,n,n)
  alpha = matrix(rep(0,n),nrow=n)
 for (i in 1:n) {
  for (j in 1:(n)) {
    if (kernel == 'polynomial'){
      K[i,j] = (X1[i,]%*%c(X1[j,]))**2
    }
    else if (kernel == 'sigmoid'){
      K[i,j] = tanh(0.0000001*(X1[i,]%*%c(X1[j,])))
    }
    else if(kernel == 'radial'){
      K[i,j] = exp(-0.0000005*(X1[i,]-X1[j,])%*%c(X1[i,]-X1[j,]))
    }
    else {
      K[i,j] = (X1[i,]%*%c(X1[j,]))
    }
  }
}

  K2 = matrix(0,n,ntest)
  for (i in 1:n) {
    for (j in 1:ntest) {
      if (kernel == 'polynomial'){
        K2[i,j] = (X1[i,]%*%c(Xtest1[j,]))**2
      }
      else if (kernel == 'sigmoid'){
        K2[i,j] = tanh(0.0000001*(X1[i,]%*%c(Xtest1[j,])))
      }
      else if(kernel == 'radial'){
        K2[i,j] = exp(-0.0000005*(X1[i,]-Xtest1[j,])%*%c(X1[i,]-Xtest1[j,]))
      }
      else {
        K2[i,j] = (X1[i,]%*%c(Xtest1[j,]))
      }
    }
  }

  if (method=='gradient'){

    for(it in 1:num_iterations){
      score = matrix(rep(0,n),nrow=n)
      for (i in 1:n){
        for (j in 1:n){
          score[j] = score[j] + alpha[i]*Y[i]*K[i,j]
        }
      }

    dalpha = matrix(rep(0,n),nrow=n)

    for (i in 1:n){
      for (j in 1:n){
        if (score[j]*Y[j]<1){
          dalpha[i] = dalpha[i] + Y[j]*Y[i]*K[i,j]
        }
      }
    }

    dalpha = dalpha/n

    alpha=alpha-lamda*alpha
    alpha=alpha+learning_rate*dalpha
    
    for (i in 1:n){
        for (j in 1:n){
          score[j] = score[j] + alpha[i]*Y[i]*K[i,j]
        }
    }

    predict_trainning_temp=sign(score)
    train_acc_temp=100*mean(predict_trainning_temp*Y>0)
    training_acc[it] = train_acc_temp
    #cat(it,"traning accuracy: ",train_acc_temp,"%\n") 
  }

  for (j in 1:ntest){
      for (i in 1:n){
        score_test[j] = score_test[j] + alpha[i]*Y[i]*K2[i,j]
      }
  }
}
                                                                                                                                            
  if (method=='dual'){
    for (it in 1:num_iterations){
      score = matrix(rep(0,n),nrow=n)
      is = sample(1:n, 3, replace=TRUE)
      for (i in is){
        sum = 0
        for (j in 1:n){
          if (i!=j) {
            sum = sum + alpha[j]*Y[i]*Y[j]*K[i,j]
          }
        }
        alpha[i] = max((1-sum)/(Y[i]*Y[i]*K[i,i]),0)        
      }
      for (i in 1:n){
        for (j in 1:n){
          score[j] = score[j] + alpha[i]*Y[i]*K[i,j]
        }
      }

    predict_trainning_temp=sign(score)
    train_acc_temp=100*mean(predict_trainning_temp*Y>0)
    training_acc[it] = train_acc_temp
    #cat(it,"traning accuracy: ",train_acc_temp,"%\n")
    }
    for (j in 1:ntest){
        for (i in 1:n){
          score_test[j] = score_test[j] + alpha[i]*Y[i]*K2[i,j]
        }
    }
  }


  predict_trainning_final=sign(score_test)
  test_acc=100*mean(predict_trainning_final*Ytest>0)
  #plot (training_acc,type="l",main=paste("training accuracy",kernel,method),xlabel="iteration",ylabel="accuracy")
  #return(beta)
  cat("Final traning accuracy: ",train_acc_temp,"%","\nFinal testing accuracy: ",test_acc,"%")
  return (list(training_acc, test_acc)) 
}





# load data
prepare_data <- function() {
  
  #load train data
  df = fread("/Users/apple/Desktop/202A/Assignment8/mnist-in-csv/mnist_train.csv") 
  df = as.matrix(df)
  
  ## only keep the numbers we want.
  l_col <- df[,1]
  index = NULL
  
  subset = c(0,1)

  for (i in 1:length(subset)){    
    number = subset[i]
    index = c(index,which(l_col == number))
  }

  index = sort(index)
  df = df[index,]
   
  # convert to numpy arrays.
  digits = df[,2:dim(df)[2]]
  labels = df[,1]

  #load test data
  df2 = fread("/Users/apple/Desktop/202A/Assignment8/mnist-in-csv/mnist_test.csv") 
  df2 = as.matrix(df2)
  
  l_col <- df2[,1]
  index = NULL
  
  for (i in 1:length(subset)){    
    number = subset[i]
    index = c(index,which(l_col == number))
  }

  index = sort(index)
  df2 = df2[index,]
  
  
  # convert to numpy arrays.s
  digits2 = df2[,2:dim(df2)[2]]
  labels2 = df2[,1] 

  return(list(digits, labels, digits2, labels2))
  
}

test <- function(){
  result = prepare_data()
  train_data = result[[1]]
  train_label = result[[2]]
  test_data = result[[3]]
  test_label = result[[4]]


  linear_gradient = mySVM(train_data,train_label,test_data,test_label,kernel='linear',method='gradient')
  train_acc_linear_gradient = linear_gradient[[1]]
  test_acc_linear_gradient = linear_gradient[[2]]
  print (paste("test_acc_linear_gradient", test_acc_linear_gradient))
  linear_dual =  mySVM(train_data,train_label,test_data,test_label,kernel='linear',method='dual')
  train_acc_linear_dual = linear_dual[[1]]
  test_acc_linear_dual = linear_dual[[2]]
  print (paste("test_acc_linear_dual", test_acc_linear_dual))
  poly_gradient = mySVM(train_data,train_label,test_data,test_label,kernel='polynomial',method='gradient')
  train_acc_poly_gradient = poly_gradient[[1]]
  test_acc_poly_gradient = poly_gradient[[2]]
  print (paste("test_acc_poly_gradient", test_acc_poly_gradient))
  poly_dual = mySVM(train_data,train_label,test_data,test_label,kernel='polynomial',method='dual')
  train_acc_poly_dual = poly_dual[[1]]
  test_acc_poly_dual = poly_dual[[2]]
  print (paste("test_acc_poly_dual", test_acc_poly_dual))
  radial_gradient = mySVM(train_data,train_label,test_data,test_label,kernel='radial',method='gradient')
  train_acc_radial_gradient = radial_gradient[[1]]
  test_acc_radial_gradient = radial_gradient[[2]]
  print (paste("test_acc_radial_gradient", test_acc_radial_gradient))
  radial_dual = mySVM(train_data,train_label,test_data,test_label,kernel='radial',method='dual')
  train_acc_radial_dual = radial_dual[[1]]
  test_acc_radial_dual = radial_dual[[2]]
  print (paste("test_acc_radial_dual", test_acc_radial_dual))
  sigmoid_gradient = mySVM(train_data,train_label,test_data,test_label,kernel='sigmoid',method='gradient')
  train_acc_sigmoid_gradient = sigmoid_gradient[[1]]
  test_acc_sigmoid_gradient = sigmoid_gradient[[2]]
  print (paste("test_acc_sigmoid_gradient", test_acc_sigmoid_gradient))
  sigmoid_dual = mySVM(train_data,train_label,test_data,test_label,kernel='sigmoid',method='dual')
  train_acc_sigmoid_dual = sigmoid_dual[[1]]
  test_acc_sigmoid_dual = sigmoid_dual[[2]]
  print (paste("test_acc_sigmoid_dual", test_acc_sigmoid_dual))


  cl = rainbow(8)
  plot(train_acc_linear_gradient,type = "l",col = cl[1], 
       xlab = "iteration",ylab = "accuracy",main="training accuracy of different settings")
  lines(train_acc_linear_dual,type = "l",col = cl[2])
  lines(train_acc_poly_gradient,type = "l",col = cl[3])
  lines(train_acc_poly_dual,type = "l",col = cl[4])
  lines(train_acc_radial_gradient,type = "l",col = cl[5])
  lines(train_acc_radial_dual,type = "l",col = cl[6])
  lines(train_acc_sigmoid_gradient,type = "l",col = cl[7])
  lines(train_acc_sigmoid_dual,type = "l",col = cl[8])
  legend(50, 70, legend=c("inear_gradient", "linear_dual","poly_gradient","poly_dual","radial_gradient","radial_dual","sigmoid_gradient","sigmoid_dual"),
       col=cl, lty=1:2, cex=0.8)

}

test()

