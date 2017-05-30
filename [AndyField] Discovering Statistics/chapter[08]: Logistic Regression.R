"
* Logistic regression: 
      * is multiple regression with categorical outcome variable and
      continuous + categorical predictor variables. 
      * binary log.reg: only two categorical outcomes. 
      * multinomial log.reg: more than two categorical outcomes. 
* Logistic reg. equation: 
      P(Y) = 1 / (1 + e^ (-(b0 + b1x1)))
* log-likelihood (equivalent of residual sum of squares):
      = sum(1->N) [Yi * ln(P(Yi)) + (1 - Yi) * ln(1 - P(Yi))]
      = unexplained variation left after model fitting
* deviance = -2LL
       = -2 * loglikelihood
* improvement of model statistic: X^2 (chi-squared)
      X^2 = -2LL(baseline) - (-2LL(new))
          = 2LL(new) - 2LL(baseline)
      df = k_new - k_baseline
* R-statistic: 
      = is the partial correlation between outcome variable
      and the predictor variables. 
      = sqrt((z^2 - 2df) / (-2LL(baseline)))
      = between -1 and 1
      = positive: increase in predictor variable amount is
      associated with increase in likelihood of event. 
      = negative: increase in predictor means decrease in 
      likelihood.
* R^2 equivalent: 
      = (2LL(new) - 2LL(baseline)) / (-2LL(baseline))
      = between 0 and 1
      = measure of how much the bad fit improves by including
      predictor variables. 
"