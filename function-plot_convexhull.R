#######################################################################################
#######################################################################################
#######################################################################################
#Author: WILLIAN T.A.F. SILVA
#E-mail: willian.silva@evobiolab.com
#######################################################################################
#######################################################################################
#######################################################################################
 
#Convex hull around data points in a particular color (specified by linecolor).
plot_convexhull<-function(xcoord,ycoord,linecolor="black",linetype=1,linewidth=1,fillcolor="blue"){
  hpts<-chull(x=xcoord,y=ycoord)
  hpts<-c(hpts,hpts[1])
  #lines(xcoord[hpts],ycoord[hpts],col=linecolor,lty=linetype,lwd=linewidth)
  polygon(xcoord[hpts],ycoord[hpts],col=fillcolor,border=linecolor,lty=linetype,lwd=linewidth)
}  