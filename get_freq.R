library(inline)


#included self defined functions, tableC as table(), strsplitC as strsplit()
inc =
'std::map<String, int> tableC(CharacterVector x)
{
  std::map<String, int> counts;

  int n = x.size();
  for (int i = 0; i < n; i++) {
    counts[x[i]]++;
  }
  return counts;
}

CharacterVector strsplitC(std::string s, char split)
{
  int n = s.length();
  CharacterVector result;
  int start = 0;

  for(int i =0 ; i < n;i++)
  {
      if(s[i] == split)
      {
        result.push_back(s.substr(start,i - start));
        start = i + 1;
      }
      if(i == n - 1)
      {
        result.push_back(s.substr(start,i - start + 1));
      }
  }
  return result;
}'

#loop function body
bd =
'CharacterVector v = as<CharacterVector>(R_x);
 char split = as<char>(R_y);
  int n = v.size();
  std::map<String, int> idf_counts;
  List freq_ls(n);

  for(int i =0 ; i < n;i++)
  {
    std::string jobdesc = as<std::string>(v[i]);
    CharacterVector jobdesc_v = strsplitC(jobdesc, split);
    freq_ls[i] = tableC(jobdesc_v);

    CharacterVector idf_words = Rcpp::unique(jobdesc_v);
    int m = idf_words.size();
    for(int j =0 ; j < m;j++)
    {
      idf_counts[idf_words[j]]++;
    }
  }


  return List::create(_["FREQ_LS"] = freq_ls, _["idf_counts"] = idf_counts);'

#R wrap up
getFreqList = cxxfunction(signature(R_x = "character", R_y="character"),
                    body = bd,
                    includes=inc,plugin="Rcpp")
