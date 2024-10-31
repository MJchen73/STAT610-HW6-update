# llr_functions.R (优化版本：speed-test-2)

# llr function that computes the fits at a point zi and applies it to each element of z:
llr = function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

# Compute f hat function (进一步优化):
compute_f_hat = function(z, x, y, omega) {
  weights = make_weight_vector(z, x, omega)  # 改为权重向量
  X = make_predictor_matrix(x)

  # 使用 sweep 函数来提高性能
  WX = sweep(X, 1, weights, `*`)  # 对 X 的每一行进行加权
  Wy = weights * y                # 对 y 进行加权

  # 计算拟合值
  f_hat = c(1, z) %*% solve(t(WX) %*% X) %*% t(WX) %*% Wy
  
  return(f_hat)
}

# 修改后的权重函数，生成权重向量而不是对角矩阵:
make_weight_vector = function(z, x, omega) {
  # 定义权重函数 W(r)
  W = function(r) {
    ifelse(abs(r) < 1, (1 - abs(r)^3)^3, 0)
  }

  # 计算每个点到 z 的距离，然后应用权重函数
  distances = abs(x - z) / omega
  weights = W(distances)

  # 返回权重向量
  return(weights)
}

# Predictor matrix function:
make_predictor_matrix = function(x) {
  # Create the predictor matrix X with the first column as 1's and the second as x
  n = length(x)
  X = cbind(rep(1, n), x)
  return(X)
}

