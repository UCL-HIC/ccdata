#include <Rcpp.h>
#include <iostream>
#include <ctime>
#include <algorithm>
#include <unordered_map>
#include <string>
using namespace Rcpp ;


// select the last value of the time period.
// [[Rcpp::export]]
DataFrame reallocateTime_(DataFrame d, const float t_discharge, const float frequency){
    // input data
    NumericVector tin = d["time"];
    CharacterVector valin = d["val"];
    // vectors for storing output data
    std::vector<float> tnew;
    std::vector<std::string> vnew;

    CharacterVector metain;
    std::vector<std::string> mnew;
    std::unordered_map<float, std::string> df_meta;


    int HAS_META = 0;
    // initialise meta data column
    if (d.containsElementNamed("meta")) {
        CharacterVector metain = d["meta"];
        for (int i = 0; i < tin.size(); ++i) 
            df_meta.insert(std::pair<float, std::string>(tin[i], as<std::string>(metain[i])));
        HAS_META = 1;
    }

    //create a map with tin and tvalin acting as a table.
    std::unordered_map<float, std::string> df_in;
    for (int i = 0; i < tin.size(); ++i) 
        df_in.insert(std::pair<float, std::string>(tin[i], as<std::string>(valin[i])));

    float tt = 0;
    while(tt <= t_discharge) {
        vnew.push_back("NA");
        tnew.push_back(tt);
        if (HAS_META) mnew.push_back("NA");
        tt += frequency;
    }

    tnew.push_back(1e127);
    std::sort(tin.begin(), tin.end());

    auto tnew_iter = tnew.begin();
    int ind;

    for ( auto t = tin.begin(); t != tin.end(); ++t ) {  
        while ( tnew_iter != tnew.end() ) {
            ind = tnew_iter - tnew.begin();
            if ( *t >= tnew[ind] && *t < tnew[ind + 1] ) {
                vnew[ind] = df_in[*t];
                if (HAS_META) mnew[ind] = df_meta[*t];
                break;
            }
            else 
                tnew_iter++;
        }
    }

    tnew.pop_back();
    if (HAS_META)
        return DataFrame::create(_["time"]=tnew, _["val"]=vnew, _["meta"]=mnew);

    return DataFrame::create(_["time"]=tnew, _["val"]=vnew);
}

// select the last value of the time period.
// [[Rcpp::export]]
DataFrame reallocateTime__(DataFrame d, const float t_admission, 
        const float t_discharge, const float frequency){
    // input data
    NumericVector tin = d["time"];
    CharacterVector valin = d["val"];
    // vectors for storing output data
    std::vector<float> tnew;
    std::vector<std::string> vnew;

    CharacterVector metain;
    std::vector<std::string> mnew;
    std::unordered_map<float, std::string> df_meta;


    int HAS_META = 0;
    // initialise meta data column
    if (d.containsElementNamed("meta")) {
        CharacterVector metain = d["meta"];
        for (int i = 0; i < tin.size(); ++i) 
            df_meta.insert(std::pair<float, std::string>(tin[i], as<std::string>(metain[i])));
        HAS_META = 1;
    }

    //create a map with tin and tvalin acting as a table.
    std::unordered_map<float, std::string> df_in;
    for (int i = 0; i < tin.size(); ++i) 
        df_in.insert(std::pair<float, std::string>(tin[i], as<std::string>(valin[i])));

    float tt = floor(t_admission);
    while(tt <= t_discharge) {
        vnew.push_back("NA");
        tnew.push_back(tt);
        if (HAS_META) mnew.push_back("NA");
        tt += frequency;
    }

    tnew.push_back(1e127);
    std::sort(tin.begin(), tin.end());

    auto tnew_iter = tnew.begin();
    int ind;

    for ( auto t = tin.begin(); t != tin.end(); ++t ) {  
        while ( tnew_iter != tnew.end() ) {
            ind = tnew_iter - tnew.begin();
            if ( *t >= tnew[ind] && *t < tnew[ind + 1] ) {
                vnew[ind] = df_in[*t];
                if (HAS_META) mnew[ind] = df_meta[*t];
                break;
            }
            else 
                tnew_iter++;
        }
    }

    tnew.pop_back();
    if (HAS_META)
        return DataFrame::create(_["time"]=tnew, _["val"]=vnew, _["meta"]=mnew, 
                _["stringsAsFactors"] = false);

    return DataFrame::create(_["time"]=tnew, _["val"]=vnew, 
            _["stringsAsFactors"] = false);
}
