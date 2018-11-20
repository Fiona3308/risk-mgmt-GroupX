# risk-mgmt-GroupX
# Framework:

# 1. Individual stock:
###   1.0. GENERAL INPUT:
* General Input
**      - Ticker
**      - Position start date
**      - Position end date      
* Parameters Input
      - Initial Investment
      - Window
      - Horizon
      - Probability for VaR and ES calculation
      - Risk Method (Historical/Parametric/Monte Carlo)

      
###   1.1. Historical price plot: plot_price_stock()
###   1.2. Parameter estimates: plot_parameters_stock()
###   1.3. VaR/ES: plot_risk_stock()
      - EXTRA INPUT: see VaR/ES input below
# 2. Portfolio: 
###   1.0. GENERAL INPUT:
      - position_date
      - end_date 
      - tickers_string
      - weight_string
###   1.1. Historical price plot: plot_price_stock()
###   1.2. Parameter estimates: plot_parameters_stock()
###   1.3. VaR/ES: plot_risk_stock()
      - EXTRA INPUT: see VaR/ES input below
