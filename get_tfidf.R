library(fastmatch)
library(Rcpp)

#get term idf values
getIDF = function(idf_counts, l){
  terms_idf = log10((l + 0.5) / (idf_counts + 0.5))
  return(terms_idf)
}

#get tf-idf value for one document
getTFIDF = function(freq, terms_idf)
{
  idf = terms_idf[fmatch(names(freq),names(terms_idf))]
  return(freq * idf)
}

#loop function for getting tf-idf values for all documents
cppFunction(
'List getTFIDF_ls(List freq_ls, NumericVector terms_idf, Function f)
{
  int n = freq_ls.size();
  List result(n);
  for(int i=0; i < n;i++)
  {
    result[i] = f(freq_ls[i], terms_idf);
  }
  return result;
}')
