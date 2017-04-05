  # The codes below are for automated occupation coding in R.
  # For implementing the codes, the following conditions must be met.
  # Both training and test data are in either data.frame or matrix forms.
  # The first column of a data set must contain numerical occupation codes.
  
  # Currently, the codes use the svm function from the e1071 package 
  # for a statistical learning method.
  # This requires the installation of the e1071 package.
  install.packages(e1071) 
  library(e1071)  

  f_combined_svm <- function(train_data,test_data)
  {
    # 4 digit svm 
    tr <- cbind(y=as.factor(train_data[,1]),train_data[,-1])
    ts <- cbind(y=as.factor(test_data[,1]),test_data[,-1])
    m <- svm(y~.,data=tr,probability=TRUE,kernel="linear")
    pred <- predict(m,ts,probability=TRUE)
    pred3 <- as.data.frame(attr(pred,"probabilities"))
    pred11 <- as.matrix(pred3)
    tr_label <- names(table(train_data[,1]))
    label <- colnames(pred11)
  
    svm_M <- matrix(0,nrow=nrow(test_data),ncol=length(tr_label))
    for(k in 1:length(label))
    {
      ind <- which(tr_label[k]==label)
      svm_M[,k] <- pred11[,ind]
    } 
  
    # 3 digit svm
    tr10 <- floor(train_data[,1]/10)
    ts10 <- floor(test_data[,1]/10)
    train_data[,1] <- tr10
    test_data[,1] <- ts10
    tr <- cbind(y=as.factor(train_data[,1]),train_data[,-1])
    ts <- cbind(y=as.factor(test_data[,1]),test_data[,-1])
    m <- svm(y~.,data=tr,probability=TRUE,kernel="linear")
    pred <- predict(m,ts,probability=TRUE)
    pred3 <- as.data.frame(attr(pred,"probabilities"))
    pred22 <- as.matrix(pred3)
    tr_label10 <- names(table(tr10))
    label_10 <- colnames(pred22)

    svm_M10 <- matrix(0,nrow=nrow(test_data),ncol=length(tr_label10))
    for(k in 1:length(label_10))
    {
      ind <- which(tr_label10[k]==label_10)
      svm_M10[,k] <- pred22[,ind]
    } 
  
    #Combining svms
    aug_M <- matrix(0,nrow=nrow(test_data),ncol=length(tr_label))
    for(k in 1:length(tr_label10))
    {
      ind <- which(floor(as.numeric(tr_label)/10)==tr_label10[k])
      aug_M[,ind] <- svm_M10[,k]
    }
    svm_comb_M <- (svm_M+aug_M)/2
    colnames(svm_comb_M) <- tr_label
    return(svm_comb_M)
  }
  
