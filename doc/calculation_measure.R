Calculation_Measure <- function(s0, price, windowLen, horizon, 
                        method, measure, npaths, VaRp, ESp, data) {
  windowLenDays <- windowLen * 252
  horizon <- horizon / 252
  if (method == "Parametric Unweighted") {
    if (measure == "VaR") {
      return(gbmVaR(s0, p, horizon, data$WindowMean, data$WindowSD))
    }
    else {
      if (measure == "ES") {
        return(gbmES(s0, p, horizon, data$WindowMean, data$WindowSD))
      }
    }
  }
  else if (method == "Exponentially Weighted") {
    if (measure == "VaR") {
      return(gbmVaR(s0, p, horizon, data$ExponentialMean, data$ExponentialSD))
    }
    else {
      if (measure == "ES") {
        return(gbmES(s0, p, horizon, data$ExponentialMean, data$ExponentialSD))}
    }
  }
  else if (method == "Historical Simulation") {
    if (measure == "VaR") {
      return(historical_VaR(logreturn,s0,windowLenDays,p,horizon))
    }
    else {
      if (measure == "ES") {
        return(historical_ES(logreturn,s0,windowLenDays,p,horizon))
      }
    }
  }
  else if (method == "Monte Carlo Simulation") {
    if (measure == "VaR") {
      return(Monte_VaR(s0, data$WindowMean, data$WindowSD, p, horizon, npaths))
    }
    else {
      if (measure == "ES") {
        return(Monte_ES(s0, data$WindowMean, data$WindowSD, p, horizon, npaths))
      }
    }
  }
}