#* Our API:
library(plumber) 

# For our models:
#source("model.R")

#* A health check: is the API running okay?
#* @get /health-check
function() {
  list(
    status = "Everything's Good!",
    time = Sys.time()
  )
}

#* Predict kidney health status for patients
#* @post /predict
function(req, res) {
  nd <- as.data.frame(req$body) 
  nd$htn <- as.factor(nd$htn) ; nd$pe <- as.factor(nd$pe) ; nd$dm <- as.factor(nd$dm)
  predict(rfWithVip, newdata = nd)
}


#* @plumber 
function(pr) {
  pr %>% 
    pr_set_api_spec(yaml::read_yaml('apispec.yaml'))
}




