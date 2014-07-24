rst2html <- function(InFile, OutFile, PyScriptPath="c:/Python24/Scripts/rst2html.py"){
     Directive <- paste("python", PyScriptPath, InFile, OutFile, sep=" ")
     system(Directive)
     }
     
