#generic cleanup method
cleanup <- function (x)  {
  if(is.null(attr(x, "class"))){
    stop("Not a class")
  }
  else  UseMethod("cleanup", x)
}


run_pipeline <- function (x)  {
  if(is.null(attr(x, "class"))){
    stop("Not a class")
  }
  else  UseMethod("run_pipeline", x)
}

flatten <- function (x){
  if(is.null(attr(x, "class"))){
    stop("Not a class")
  }
  else  UseMethod("flatten", x)
}

model <- function(x){
  if(is.null(attr(x, "class"))){
    stop("Not a class")
  }
  else  UseMethod("model", x)
}


write_to_file <- function(x){
  if(is.null(attr(x, "class"))){
    stop("Not a class")
  }
  else  UseMethod("write_to_file", x)
}


score_model <- function(x){
  if(is.null(attr(x, "class"))){
    stop("Not a class")
  }
  else  UseMethod("score_model", x)
}

get_random_seed <- function(x, index){
  if(is.null(attr(x, "class"))){
    stop("Not a class")
  }
  else  UseMethod("get_random_seed")
}

cleanup.default <- cleanup
run_pipeline.default <- run_pipeline
flatten.default <- flatten
model.default <- model
write_to_file.default <- write_to_file
score_model.default <- score_model
get_random_seed.default <- get_random_seed