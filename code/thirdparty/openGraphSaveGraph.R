# openGraphSaveGraph.R
# John K. Kruschke, January 29, 2013.
# http://www.indiana.edu/~kruschke/DoingBayesianDataAnalysis/Programs/?M=D

openGraph = function( width=7 , height=7 , mag=1.0 , ... ) {
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    X11( width=width*mag , height=height*mag , type="cairo" , ... )
  } else { # Windows OS
    windows( width=width*mag , height=height*mag , ... )
  }
}

saveGraph = function( filename="saveGraphOutput" , type="pdf" , ... ) {
  if(is.null(dev.list())) dev.new()
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    if ( any( type == c("png","jpeg","jpg","tiff","bmp")) ) {
      sptype = type
      if ( type == "jpg" ) { sptype = "jpeg" }
      savePlot( filename=paste(filename,".",type,sep="") , type=sptype , ... )     
    }
    if ( type == "pdf" ) {
      dev.copy2pdf(file=paste(filename,".",type,sep="") , ... )
    }
    if ( type == "eps" ) {
      dev.copy2eps(file=paste(filename,".",type,sep="") , ... )
    }
  } else { # Windows OS
    file=paste(filename,".",type,sep="") # force explicit extension
    savePlot( filename=filename , type=type , ... )
  }
}
