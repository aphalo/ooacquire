// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// maya_tail_correction
List maya_tail_correction(NumericVector w_length, NumericVector cts_second);
RcppExport SEXP ooacquire_maya_tail_correction(SEXP w_lengthSEXP, SEXP cts_secondSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type w_length(w_lengthSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type cts_second(cts_secondSEXP);
    __result = Rcpp::wrap(maya_tail_correction(w_length, cts_second));
    return __result;
END_RCPP
}