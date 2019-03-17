eval_classifier<-function(trained_model, x_test, y_test){
  
  #make predictions and probailities on the test set
  y_pred<-predict(trained_model,x_test,type = "raw")
  y_pred_prob<-predict(trained_model,x_test,type = "prob")
  
  #spit out the confusion matrix on the test set
  print(confusionMatrix(data = y_pred , y_test ))
  
  #make test predictions data frame 
  tdf<-tibble(y_pred, 
              B=y_pred_prob$B, 
              M=y_pred_prob$M, 
              y_test=y_test)
  
  # Select a parameter setting if random forest
  if (trained_model$method=="rf"){
    selectedIndices <- trained_model$pred$mtry == 2
    selected_pred <- trained_model$pred[selectedIndices, ]
  } else {
    selected_pred <- trained_model$pred
  }
  
  
  # Get the test set AUC:
  test_auc=auc(y_test, y_pred_prob$B)
  
  #plot train ROC in red
  p<-ggplot(selected_pred, 
            aes(m = M, d = obs)) + 
    geom_roc(hjust = -0.4, 
             vjust = 1.5,
             color = 'red') +
    #add test ROC in blue
    geom_roc(hjust = -0.4,
             vjust = 1.5, 
             color = 'blue', 
             data = tdf, 
             mapping = aes(m = M, d = y_test)) +
    #make it look prettier
    theme_classic()+
    coord_equal()+
    scale_x_continuous(expand=c(0.01,0.01))+
    scale_y_continuous(expand=c(0.01,0.01))+
    labs(title = paste0("Test AUC = ", format(round(test_auc,3), nsmall = 3),", train = red, test = blue"))
  
  print(p)
  
  return(invisible(tdf))
}
