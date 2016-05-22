checkResult <- function (output, trueOutput)
{
  n = length(output)
  
  tp = 0
  fp = 0
  fn = 0
  
  for (i in 1:n)
  {
    if (output[i]==0 && trueOutput[i] == 0 && output[i] == trueOutput[i])
    {
      tp = tp + 1
    }
    else if (output[i]==0 && trueOutput[i]==1)
    {
      fp = fp + 1
    }
    else if (output[i]==1 && trueOutput[i]==0)
    {
      fn = fn + 1
    }
  }
  precision = tp / (tp + fp)
  #print ("Tp")
  print(tp)
  recll = tp / (tp + fn)
  #print ("Fp")
  #print(fn)
  gMean = sqrt(precision*recll)
  
  return (gMean)
  
  
}