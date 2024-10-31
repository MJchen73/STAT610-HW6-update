source("llr_functions.R")
# Try out functions to see if it works on some data:
library(reshape2) # package that contains "french_fries"
data(french_fries)
french_fries = french_fries[complete.cases(french_fries),]
z = seq(0, 15, length.out = 100)
fits = llr(z = z, x = french_fries$potato, y = french_fries$buttery, omega = 2)
plot(z, fits)

# Loop through different omega values
for (i in 1:10){
  fits = llr(z = z, x = french_fries$potato, y = french_fries$buttery, omega = i)
  plot(z, fits, main = paste("llr with ω =", i),  xlab= expression(z), ylab = expression(hat(f)(z)))
}

# We can see that the fit is getting more smoother when we increase the value of omega.
# In locally weighted regression, the hyperparameter omega controls how much of the 
# surrounding data influences the fit at any given point. Larger omega increases the size of 
# the window around the target point z, meaning more data points will contribute to 
# the local regression fit for each point, thus effectively averaging out local fluctuations 
# and reducing the impact of noise, leading to smoother curves.