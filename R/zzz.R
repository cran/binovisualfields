
.ddivfEnv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname){
  .ddivfEnv$pd_gender <- c(62, 64)
}
