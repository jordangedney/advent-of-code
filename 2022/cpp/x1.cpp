#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;

vector<int> parse(){
  fstream file;
  vector<int> nums;

  int cur = 0;
  file.open("../inputs/1", ios::in);
  if(file.is_open()){
    string tp;
    while(getline(file, tp)){
      if(tp.compare("") == 0){
        nums.push_back(cur);
        cur = 0;
      } else {
        cur += stoi(tp);
      }
    }
    file.close();
  }
  return nums;
}

int main(){
  auto cals = parse();

  // part-one
  cout << *max_element(cals.begin(), cals.end()) << endl;

  // part-two
  sort(cals.begin(), cals.end());
  cout << cals[cals.size()-1] + cals[cals.size()-2] + cals[cals.size()-3] << endl;
}
