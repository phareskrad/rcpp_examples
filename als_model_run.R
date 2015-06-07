#alternate least square model for predicting recommendation scores
source(paste(UTILS_PATH, "updateVec.R", sep = ""))

updateUser = function(user_id, M, E, lambda, user_ls = USER_LS){
  data = user_ls[[user_id]]
  item_index = data$company_index
  rate = data$rate
  subM = as.matrix(M[,item_index])
  new_user = updateVec(subM, E, rate, lambda)
  return(new_user)
}

updateItem = function(item_id, U, E, lambda, company_ls = COMPANY_LS){
  data = company_ls[[item_id]]
  user_index = data$user_index
  rate = data$rate
  subU = as.matrix(U[,user_index])
  new_item = updateVec(subU, E, rate, lambda)
  return(new_item)
}

#loop function to loop through each item/user and update the score matrix
cppFunction('NumericMatrix processVector(NumericMatrix M, NumericMatrix E, List data_ls, double lambda, Function update)
{
  NumericMatrix U(M.nrow(),data_ls.size());
  for (int i = 0; i < data_ls.size(); i++) {
    NumericVector temp = update(i + 1, M, E, lambda);
    U(_, i) = temp;
  }
  return U;
}')

initMatrix = function(f, avg = AVG){
  m = matrix(runif(length(avg) * f), ncol = length(avg))
  m[1,] = avg
  return(m)
}

#loop function for checking the RMSE of the probe data
cppFunction('double checkRMSE(NumericMatrix u, NumericMatrix m, List probe_ls)
{
  NumericVector u_index = probe_ls["user_index"];
  NumericVector m_index = probe_ls["company_index"];
  NumericVector true_rate = probe_ls["total_rate"];
  int n = true_rate.size();
  NumericVector u_vector(u.nrow());
  NumericVector m_vector(m.nrow());
  NumericVector rate(n);
  for(int i = 0; i < n; i++){
    int ui = u_index[i] - 1;
    int mi = m_index[i] - 1;
    u_vector = u(_,ui);
    m_vector = m(_,mi);
    rate[i] = sum(u_vector * m_vector);
  }
  double rmse = sum(pow((true_rate - rate), 2)) / n;
  return rmse;
}')

runALSModel = function(lambda, f, iter_num, threshold = 0.001, result = T, user_ls = USER_LS, company_ls = COMPANY_LS, probe_ls = model_data){
  M = initMatrix(f)
  E = diag(rep(1, f))
  iter = 0;
  rmse_track = c();
  run_flag = TRUE;
  while(iter < iter_num && run_flag) {
    U = processVector(M, E, user_ls, lambda, updateUser)
    M = processVector(U, E, company_ls, lambda, updateItem)
    iter = iter + 1
    #print(iter)
    rmse = sqrt(checkRMSE(U, M, probe_ls))
    rmse_track[iter] = rmse;
    if (iter > 1 && rmse - rmse_track[iter - 1] <= threshold)
      run_flag = FALSE
  }
  if (result)
    return(list('item_feature' = t(M), 'user_feature' = t(U)))
  else
    return(rmse_track)
}

chooseLambda = function(f, lambda_set, iter_num) {
  min_rmse = foreach(i = 1 : length(lambda_set)) %do% {
    rmse = runALSModel(lambda_set[i], f, iter_num, FALSE)
    return(min(rmse))
  }
  min_rmse = unlist(min_rmse)
  print(min_rmse)
  return(lambda_set[min_rmse == min(min_rmse)])
}
