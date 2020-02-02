
library(data.table) # allows us to use function fread


mySVM=function(X,Y,Xtest,Ytest,num_iterations=40,learning_rate=1e-1,lamda=0.01)
{
  n=dim(X)[1]
  p=dim(X)[2]+1
  X1 = cbind(rep(1, n), X)#intercept is included 
  Y=2*Y-1# y=1--->1 y=0--->-1
  
  ntest = dim(Xtest)[1]
  Xtest1 = cbind(rep(1, ntest), Xtest)
  Ytest=2*Ytest-1
  
  beta=matrix(rep(0,p),nrow=p)
  for(it in 1:num_iterations){
    score=X1%*%beta
    db=score*Y<1
    dbeta=matrix(rep(1:n),nrow=1)%*%(matrix(db*Y,n,p)*X1)/n
    beta=beta+learning_rate*t(dbeta)
    beta[2:p]=beta[2:p]-lamda*beta[2:p]
    
    predict_trainning_temp=sign(X1%*%beta)
    train_acc_temp=100*mean(predict_trainning_temp*Y)
    
    if (it %% 1 == 0){
      cat(it,"traning accuracy: ",train_acc_temp,"%\n")
    }
  
  }
  predict_trainning_final=sign(Xtest1%*%beta)
  test_acc=100*mean(predict_trainning_final*Ytest)
  #return(beta)
  cat("Final traning accuracy: ",train_acc_temp,"%","\nFinal testing accuracy: ",test_acc,"%")
  #acc=mean(sign(s)*Y) 
}





# load data
load_digits <- function(normalize=TRUE) {

  df = fread("/Users/apple/Desktop/202A/Assignment8/mnist-in-csv/mnist_train.csv") 
  df = as.matrix(df)
  
  ## only keep the numbers we want.
  subset = c(0,1)

  c <- dim(df)[2]
  l_col <- df[,c]
  index = NULL
  
  for (i in 1:length(subset)){    
    number = subset[i]
    index = c(index,which(l_col == number))
  }

  sort(index)
  df = df[index,]
  
  
  # convert to numpy arrays.
  digits = df[,-1]
  labels = df[,c]
  
  # Normalize digit values to 0 and 1.
  # if (normalize == TRUE) {
  #   digits = digits - min(digits)
  #   digits = digits/max(digits)}
  print (digits)
  print (labels)
  
  return(list(digits, labels))
  
}

split_samples <- function(digits,labels) {
  
  # Split the data into a training set (70%) and a testing set (30%).
  
  num_samples <- dim(digits)[1]
  num_training <- round(num_samples*0.7)
  indices = sample(1:num_samples, size = num_samples)
  training_idx <- indices[1:num_training]
  testing_idx <- indices[-(1:num_training)]
  
  return (list(digits[training_idx,], labels[training_idx],
               digits[testing_idx,], labels[testing_idx]))
}

result = load_digits(normalize=TRUE)
# digits = result[[1]]
# labels = result[[2]]

# result = split_samples(digits,labels)
# training_digits = result[[1]]
# training_labels = result[[2]]
# testing_digits = result[[3]]
# testing_labels = result[[4]]

# # print dimensions
# length(training_digits)
# length(testing_digits)

# X=training_digits 
# Y=training_labels
# final_beta=mySVM(training_digits,training_labels,testing_digits,testing_labels )
