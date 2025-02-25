library(markdown)
result0 <- 
  rpubsUpload("Red and Black 250225 : Search for the Best 1st 1 Million Trials", 
              "./R/Red_and_Black_prototype_250225_v5_0_1.html")
result0

## Mac 에서 오류 발생해도 URL 그대로 사용하면 됨.

library(rsconnect)
result0 <- 
  rpubsUpload("Red and Black 250225 : Search for the Best First 1 Million Trials", 
              "./R/Red_and_Black_prototype_250225_v5_0_1.html", 
              "./R/Red_and_Black_prototype_250225_v5_0_1.Rmd")

## Mac 에서도 잘 작동함. originalDoc 빠뜨리면 오류 발생함.