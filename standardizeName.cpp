#include <Rcpp.h>
#include <string>
#include <cctype>

std::string standardize(std::string str) {
  int n = str.length();
  if (n <= 4) {
    for (int i = 0; i < n; i++) {
      str[i] = toupper(str[i]);
    }
  }
  else {
    str[0] = toupper(str[0]);
    for (int i = 0; i < n; i++) {
      if (str[i] == ' ')
        str[i + 1] = toupper(str[i + 1]);
    }
  }
  return str;
}


// [[Rcpp::export]]
std::vector<std::string> standardizeNames(Rcpp::CharacterVector names) {
  int n = names.size();
  std::vector<std::string> result(n);
  for (int i = 0; i < n; i++) {
    std::string s = std::string(names[i]);
    s = standardize(s);
    result[i] = s;
  }
  return result;
} 

