# llr_functions.R (优化 1：在 speed-test-1 分支中)

# llr function that computes the fits at a point zi and apply it to each element of z:
llr = function(x, y, z, omega) {
  # 使用 sapply 对每个 z 进行计算
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

# Compute f hat function (优化 1):
compute_f_hat = function(z, x, y, omega) {
  # 获取权重向量而不是对角矩阵
  weights = make_weight_vector(z, x, omega)
  
  # 创建预测矩阵 X
  X = make_predictor_matrix(x)
  
  # 使用权重向量与矩阵逐元素相乘，而不是使用矩阵乘法
  WX = X * weights  # 对每一列进行加权
  Wy = y * weights  # 对 y 进行加权
  
  # 计算拟合值
  f_hat = c(1, z) %*% solve(t(WX) %*% X) %*% t(WX) %*% y
  
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
