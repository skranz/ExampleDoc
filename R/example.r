# f = function(x){
#   # Add one and square
#   x = x+1
#   x^2
# }
# 
# examples.f = function() {
#   # Try it out with a scalar
#   f(5)
#   
#   # Now with a vector
#   f(1:5)
#   
# }
# 
# examples.source = function() {
#   setwd("C:/libraries/ExampleDoc/ExampleDoc/R")
#   source("example.r", keep.source=TRUE)
#   deparse(f, control="useSource")  
# }