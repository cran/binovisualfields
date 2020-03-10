## ----setup, include = FALSE----------------------------------------------
library(binovisualfields)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------
#  atbino.df <- read.csv(system.file("extdata", "atbinocular.csv",
#                                     package = "binovisualfields"))
#  #yourdf <- read.csv("yourdata.csv"")  specify path and data file name to load your own data
#  at_id <- unique(atbino.df$id)
#  at_gender <- atbino.df$gender[1:length(at_id)]

## ---- eval=FALSE---------------------------------------------------------
#  at_left_visual_fields <- atbino.df[atbino.df$seye=="OS", 4:ncol(atbino.df)]
#  at_rght_visual_fields <- atbino.df[atbino.df$seye=="OD", 4:ncol(atbino.df)]

## ----eval=FALSE----------------------------------------------------------
#  #initialize vf array for all patients
#  vf_matrix <- matrix(NA, ncol=10, nrow = 8)
#  at_rght_vf_array <- replicate(nrow(at_rght_visual_fields), vf_matrix)
#  for (i in 1:nrow(at_rght_visual_fields)){
#    at_rght_vf_array[,,i] <- makevf(unlist(at_rght_visual_fields[i,], use.names = F), eye="right")
#  }
#  
#  at_left_vf_array <- replicate(nrow(at_left_visual_fields), vf_matrix)
#  for (i in 1:nrow(at_rght_visual_fields)){
#    at_left_vf_array[,,i] <- makevf(unlist(at_left_visual_fields[i,], use.names = F), eye="left")
#  }

## ----eval=FALSE----------------------------------------------------------
#  id <- 3
#  pindex  <- match(id, at_id)
#  left_vf <- at_left_vf_array[, , pindex]
#  rght_vf <- at_rght_vf_array[, , pindex]
#  gender  <- at_gender[pindex]

## ----eval=FALSE----------------------------------------------------------
#  fix_dist <- c(600, 0)
#  theta_left <- caltheta(fix_dist, gender = gender, eye = "left")
#  theta_rght <- caltheta(fix_dist, gender = gender, eye = "right")

## ----eval=FALSE----------------------------------------------------------
#  object_distances <- seq(300, 1000, 100)
#  c_vf  <- binovfcal(left_vf, rght_vf, theta_left, theta_rght, object_distances, gender=gender)

## ----eval=FALSE----------------------------------------------------------
#  c_vf <- c_vf[, , as.character(500)]

## ----eval=FALSE----------------------------------------------------------
#  filename <- paste0("archetypes", id, ".pdf")
#  pdf(filename, width=16)
#  options(error=dev.off)
#  
#  layout(matrix(c(1, 1, 1, 1, 1, 1, 2, 4, 5, 3, 4, 5), 3, 4),  heights = c(3, 3, 1))
#  m_xs <- seq(-27, 27, length.out = 10)
#  c_xs <- seq(-63, 63, 6)
#  for (i in object_distances) {
#    plotvfray (left_vf, rght_vf, theta_left, theta_rght, fix_dist, i)
#    plotvf(m_xs, left_vf, "Left Monocular")
#    plotvf(m_xs, rght_vf, "Right Monocular")
#    plotvf(c_xs, c_vf[, , as.character(i)],
#          paste0("DD-IVF Fixation Distance = ", fix_dist[1], "mm, Object Distance = ", i, "mm"))
#    colorkey()
#  }
#  dev.off()
#  options(error=NULL)

