CGIparse <- function(string){
    the.split.string <- lapply(strsplit(string,"&"), 
                             function(string)strsplit(string,"="))[[1]]
    arglist <- lapply(the.split.string, function(pair)pair[2])
    names(arglist) <- sapply(the.split.string, function(pair)pair[1])
    lapply(arglist,hexDecode)}
       
hexDecode <- function(string){
    string <- gsub("\\+"," ", string)
    string <- gsub("%09","\t", string)
    string <- gsub("%0D%0A","\n", string)
    string <- gsub("%0D|%0A","\n", string)
    pieces <- strsplit(string, "%")[[1]]
    dehex <- function(string){
              hex <- substr(string, 1, 2)
              paste(ascii[hex], substring(string, 3), sep="")
              }
    pieces <- c(pieces[1], sapply(pieces[-1], dehex))
    paste(pieces, collapse = "")
    }     

HTMLheader <- function(){
    cat("<!doctype html public \"-//W3C/DTD HTML 4.0/EN\">\n")
    lf(2)
    }
         
br <- function(n = 1){
    cat(paste(rep("<BR>", n), collapse=""), "\n")
    }

tag <- function(tagname,...){
    if (exists("useUnquotedTagnames") && useUnquotedTagnames) 
        tagname <- as.character(substitute(tagname))
    dots <- as.list(substitute(list(...)))[-1]
    if (length(list(...))>0) {
       dots <- gsub(" = ", "=", gsub("[,)]", "",
                 sub("[^,]*,", "", deparse(match.call()))))}
    cat("<", tagname,dots, ">", sep="")}
  
untag <- function(tagname){
    if (useUnquotedTagnames) 
        tagname <- as.character(substitute(tagname))
    cat("</", tagname, ">", sep="")}
  
lf <- function(n = 1) cat(paste(rep("\n", n), sep = ""))

comments <- function(text) cat("<!--", text, "-->")

mailto <- function(text, address){
    cat("<a href=\"mailto:", address, "\">", text, "</a>", sep="")}
    
linkto <- function(text, URL){
    cat("<A href=\"", URL, "\">", text, "</A>", sep = "")}
    
webPNG <- function(file, ...){
    if (!exists("graphDir")) return(cat(
         "Error in webPNG(): graphDir not set\n\n"))
    n <- nchar(graphDir)
    separator <- if (substr(graphDir, n, n) == "/") "" else "/"
    bitmap(file=paste(graphDir, file, sep = separator), ...)
    invisible(NULL)}
    
img <- function(src, ...){
    if (exists("graphURLroot")) src <- paste(graphURLroot, src, sep="")
    dots <- as.list(substitute(list(...)))[-1]
    if (length(list(...)) > 0) {
       dots <- gsub(" = ", "=", gsub("[,)]", "", sub("[^,]*,", "",
                      deparse(match.call()))))}
    cat("<IMG SRC=\"", src, "\"", dots, ">", sep = "")
    invisible("image")}

"ascii" <-
  structure(c(
    "\t", "\n", "\r",  "!", "\"", 
     "#",  "$",  "%",  "&",  "'", 
     "(",  ")",  "+",  ",",  "/",  ":",  ";",  "<", 
     "=",  ">",  "?",  "[", "\\",  "]",  "^",  "`",  "{",  "|", 
     "}",  "~",   "",  "¢",  "£", "±"), 
  .Names = c( 
    "09", "0A", "0D", "21", "22", 
    "23", "24", "25", "26", "27", 
    "28", "29", "2B", "2C", "2F", "3A", "3B", "3C", 
    "3D", "3E", "3F", "5B", "5C", "5D", "5E", "60", "7B", "7C", 
    "7D", "7E", "7F", "A2", "A3", "B1"))

scanText <- function(string, ...){
    tc <- textConnection(string)
    scan(tc, what = character(0), quiet = TRUE, ...)}
    
indentPrint <- function(object, indent=4, ...){
    sink(textConnection("zz", "w"))
    print(object, ...)
    sink()
    indent <- paste(rep(" ", indent), sep="", collapse="")
    cat(paste(indent, zz, sep=""), sep="\n")}
    
.First.lib <- function(lib, pkg){
    useUnquotedTagnames <<- TRUE
    formData <<- Sys.getenv("FORM_DATA")
    if (formData=="") {  ## probably uncgi has been used
        Env <- Sys.getenv()
        Names <- names(Env)
        formData <<- as.list(Env[grep("^WWW\_", Names)])
        names(formData) <<- sapply(names(formData), function(name){
                                    gsub("^WWW\_", "", name)})
    }   
    else formData <<- CGIparse(formData)}
