
library(shiny)
library(fOptions)

shinyServer(function(input, output) {
  S0  = 100
  K   = 110
  r   = 0.05
  si  = 0.3
  tau = 0.02
  
  # computation of Brownian motion
  T   = 1000
  t   = (1:T)/T
  dt  = t[2] - t[1]
  Wt1 = rnorm(length(t), mean = 0, sd = 1)
  Wt  = cumsum(Wt1)  # cumulative sum
  St  = S0 * exp((r - 0.5 * si) * dt + si * sqrt(dt) * Wt)
  
  # Valuation of the call price for each value of the underlying
  Call    = GBSOption(TypeFlag = "c", S = St, X = K, Time = tau, r = r, b = 0, sigma = si)  
  Callp   = attr(Call, "price")
  
  output$plotBM <- renderPlot({ 
    split.screen(c(2, 1))
    screen(1)
    plot(t, St, col = "blue", type = "l", lwd = 2, xlab = "t", ylab = "S_t")
    screen(2)
    plot(t, Callp, col = "red", type = "l", lwd = 2, xlab = "t", ylab = "C(S,t)")
  })
  
})