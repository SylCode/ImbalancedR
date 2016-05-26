checkNumOfObjs <- function(testResult, safe, borderline, rare, outlier) {
  
  numOfObjs <- 0
  
  if(safe) {
    numOfObjs <- numOfObjs + testResult$score$minority.safe
  }
  
  if(borderline) {
    numOfObjs <- numOfObjs + testResult$score$minority.borderline
  }
  
  if(rare) {
    numOfObjs <- numOfObjs + testResult$score$minority.rare
  }
  
  if(outlier) {
    numOfObjs <- numOfObjs + testResult$score$minority.outlier
  }
  
  return(numOfObjs)
}