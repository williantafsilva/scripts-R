#######################################################################################
#######################################################################################
#######################################################################################
#Author: WILLIAN T.A.F. SILVA
#E-mail: willian.silva@evobiolab.com
#######################################################################################
#######################################################################################
#######################################################################################

#Find the element of a vector with the closest value.
closest_index<-function(myvalue,myvector){
  index<-which(abs(myvalue-myvector)==min(abs(myvalue-myvector)))
  return(index)
}  