library(jtools)

#After accounting for hospital characteristics, which factors are associated 
  #with worse readmission performance?
  model1 <- glm(
    High_Readmission_Risk ~ 
      `Hospital Ownership` +
      `Hospital Type` +
      Emergency_Service +
      Year,
    family = binomial(link=logit),
    data = df
  )
  
  summ(model1, digits = 4, exp=TRUE)
