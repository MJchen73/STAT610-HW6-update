# llr function that computes the fits at a point zi and apply it to each element of z:
llr = function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

# Compute f hat function:
compute_f_hat = function(z, x, y, omega) {
  Wz = make_weight_matrix(z, x, omega)
  X = make_predictor_matrix(x)
  f_hat = c(1, z) %*% solve(t(X) %*% Wz %*% X) %*% t(X) %*% Wz %*% y
  return(f_hat)
}

# Below is our task to write weight matrix function:
make_weight_matrix = function(z, x, omega) {
  # Define the weight function W(r)
  W = function(r) {
    ifelse(abs(r) < 1, (1 - abs(r)^3)^3, 0)
  }
  
  # Compute the weights for each point based on the distance to z
  distances = abs(x - z) / omega
  weights = W(distances)
  
  # Return a diagonal matrix with the weights
  Wz = diag(weights)
  return(Wz)
}

# Below is our task to write predictor matrix function:
make_predictor_matrix = function(x) {
  # Create the predictor matrix X with the first column as 1's and the second as x
  n = length(x)
  X = cbind(rep(1, n), x)
  return(X)
}