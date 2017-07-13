.onLoad = function(...)  {
  # If loaded as library overwrite restore.point to an empty function
  assign("restore.point", function(...){}, envir=parent.env(environment()))
}


#' Build package documentation with examples
#' @export
document.with.examples = function(package, package.path = path.package(package),clean=TRUE,roclets=c("namespace","rd"),example.path=NULL, ...) {
  library(devtools)
  #load_all(package.path)
  document(package.path, clean=clean,roclets=roclets,...)
  add.examples.to.package(package,package.path=package.path,example.path=example.path,...)
}


examples.document.with.examples = function() {
  library(restorepoint)
  
  document.with.examples("ExampleDoc", "C:/libraries/ExampleDoc/ExampleDoc")  
  document.with.examples("RMaxima", "C:/libraries/RMaxima/RMaxima")  
  document.with.examples("stringtools", "C:/libraries/stringtools/stringtools")  
  
  
  document.with.examples("restorepoint","C:/libraries/restorepoint/restorepoint", roclets=c("rd","namespace"), example.path = "C:/libraries/restorepoint/restorepoint/examples")
  
}
  
#' Add examples to Rd files of a package
#' @export
add.examples.to.package = function(package, package.path = path.package(package), example.path =NULL, example.files = NULL) {
  #library(package, character.only=TRUE)
  ex.li = get.examples(files=example.files, package.path=package.path, path=example.path )
  for (i in seq_along(ex.li)) {
    add.example.to.Rd(ex.li[[i]], package.path=package.path)
  }
}

add.tests.to.package = function(package, package.path = path.package(package), example.path =NULL, example.files = NULL, ex.li=NULL) {
  restore.point("add.tests.to.package")
  # Need to load package to run examples
  library(package, character.only=TRUE)
  if (is.null(ex.li))
    ex.li = get.examples(files=example.files, package.path=package.path, path=example.path )
  
  test.code = lapply(ex.li, example.to.test)
  test.code = paste0(test.code, collapse =
"\n#########################################################\n")
  test.code
}

examples.add.tests.to.package = function() {
  library(restorepoint)
  library(RMaxima)
  connect.maxima()
  
  add.tests.to.package("RMaxima", "C:/libraries/RMaxima/RMaxima")
}

#' Adds examples to an Rd file
#' 
#' Not very stable yet. Assume that the Rd file has no examples section yet. Simple add examples at the end
#' @param ex a list with elements fun.name and example.source
#' @export
add.example.to.Rd = function(ex,package,package.path=path.package(package),fun.name=ex$fun.name,example.source=ex$example.source, remove.top.lines = 1, remove.bottom.lines = 1, all.dont.run = TRUE) {  
  Rd.file = paste(package.path,"/man/",fun.name,".Rd",sep="")
  if (!file.exists(Rd.file)) {
    warning(paste("No Rd file", Rd.file, " found!"))
    return()
  }
  txt = readLines(Rd.file)
  ex.source = example.source[(remove.top.lines+1):(length(example.source)-remove.bottom.lines)]
  if (all.dont.run) {
    new.txt = c("\\examples{\\dontrun{",ex.source,"}}")
  } else {
    new.txt = c("\\examples{",ex.source,"}")    
  }
  txt = c(txt,new.txt)
  txt
  writeLines(txt,Rd.file)
  invisible(txt)
}

get.R.files = function(package.path=NULL,files=NULL, path=NULL) {
  restore.point("get.R.files")
  
  if (!is.null(files))
    return(files)
  if (!is.null(package.path))
    path = c(path,paste0(package.path,"/R"))
  
  files = NULL
  for (p in path) {
    files = c(files, paste0(p,"/",list.files(p, pattern="^.*\\.[Rr]$")))
  }
  
  return(files)
}

#' Extract all example functions from a set of files
#' 
#' @param files a list of R files
#' @param path a path from which all R files are taken
#' @param package take all R files from the source directory in the package
#' @export
get.examples = function(package.path=NULL,files=NULL, path=NULL) {
  restore.point("get.examples")
  
  files = get.R.files(package.path,files,path)
  get.examples.from.files(files)
}

examples.get.examples = function() {
  path = "C:/libraries/ExampleDoc/ExampleDoc/R"
  get.examples(path=path)
  
}

get.examples.from.files = function(files, prefix="examples.", postfix="") {
  env = new.env(parent=.GlobalEnv)
  for (file in files)
    source(file,local=env, keep.source=TRUE)
  objs = ls(env)
  ex.li = lapply(objs, function(fun.name) {
    example.fun.name = paste0(prefix,fun.name,postfix)
    if (!exists(example.fun.name,env))
      return(NULL)
    message(paste0("Example for: ",fun.name))
    example.fun = get(example.fun.name,env)
    list(fun.name=fun.name,
         example.fun=example.fun,
         example.source=deparse(example.fun, control="useSource")
    )
  })
  empty = sapply(ex.li,is.null)
  ex.li[!empty]
}
  
# Checks whether any pattern matches in x
any.grepl = function(patterns,x,...) {
  ret = lapply(patterns,grepl,x=x,...)
  ret = do.call(rbind,ret)
  colSums(ret)>0
}

examples.any.grepl = function() {
  any.grepl(c("hallo","hi"), c("hi wie gehts","Na du","hallo"))
}

example.to.test = function(ex,fun.name=ex$fun.name, example.fun=ex$example.fun, max.nchar = 500, verbose = TRUE, ignore.calls = c("benchmark"),seed=1234567) {
  restore.point("example.to.test")
  
  ex = body(example.fun)
  com.str = as.character(ex)[-1]
  com.str = c(paste0("set.seed(",seed,")"),com.str)

  # Identify lines of code in which the to be tested function is called
  fun.regexp = paste("(\\Q", fun.name, "\\E)([[:space:]]*)\\(",sep="")
  calls.fun = any.grepl(fun.regexp,com.str)
  
  ignore.calls = paste("(\\Q", ignore.calls, "\\E)([[:space:]]*)\\(",sep="")
  ignore.lines = any.grepl(ignore.calls, com.str)
  
  calls.fun = calls.fun & (!ignore.lines)
  
  #com.str[calls.fun]
  
  
  last.row = max(which(calls.fun))
  if (length(last.row)>0) {
    com.str = com.str[1:last.row]
  }
  test.str = rep("", length(com.str))
  
  work.env = emptyenv()
  for (i in seq_along(com.str)) {
    err = try(val <- eval(parse(text=com.str[i]),envir=work.env), silent=TRUE)
    if (is(err,"try-error")) {
      if (calls.fun[i])
        test.str[i] = paste("\n  expect_error(eval(expression(", com.str[i], ")))")
      com.str[i] = paste("#", com.str[i])      
    } else {
      
      if (calls.fun[i]) {
        val.str = paste(deparse(val, width.cutoff=500L),collapse="\n")
        if (nchar(val.str)<max.nchar) {
          test.str[i] = paste("\n  expect_equivalent(\n  eval(expression(", com.str[i], ")),\n  ", val.str,"\n)\n")
        } else {
          require(digest)
          test.str[i] = paste("\n  expect_equivalent(\n  digest(eval(expression(", com.str[i], "))),\n  ",'"', digest(val),'"',"\n)\n",sep="")
          #test.str[i] ="# Test ommited result too long"
        }
        com.str[i] = paste("#", com.str[i])
      }
    }
    if (verbose)
     message(paste("\n  ",com.str[i],test.str[i],sep=""))
  }
  code = paste("\n  ",com.str, test.str, collapse="",sep="")
  
  code = paste('test_that("test.',example.fun,'",{\n',code,'\n})',sep="")
  #capture.output(cat(code))
  code
}


