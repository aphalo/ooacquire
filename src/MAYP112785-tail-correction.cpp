#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;
using namespace std;

//' Function to compute the tail correction
//'
//' @param w_length numeric vector of wavelengths in nm.
//' @param cts_second numeric vector of counts per second (stray-light corrected).
//'
//' @return tail returned as numeric vector within a list.
//'
//' Tail correction is a reimplementation of the calculations developed by Lasse
//' Ylianttila (STUK, Finland), originally in Excel.
//'
//' @family spectral data-processing functions
//'
//' @export
//'
// [[Rcpp::export]]
List MAYP112785_tail_correction(NumericVector w_length, NumericVector cts_second) {

    const int window_width = 300L;

    vector<double> wl = Rcpp::as< vector<double> >(w_length);
    vector<double> cs = Rcpp::as< vector<double> >(cts_second);
    vector<double> dwl (cs.size());
    vector<double> tail (cs.size());

    vector<double>::iterator inner_bg, inner_nd;

    for (vector<double>::iterator wl_itp = wl.begin(), wl_it = (wl.begin() + 1), dwl_it = dwl.begin(); wl_it < wl.end(); wl_itp++, wl_it++, dwl_it++) {
      (*dwl_it) = (*wl_it) - (*wl_itp);
    }
    *(dwl.begin()) = *(dwl.begin() + 1);

    const double t1 = -9.22574221, t2 = -0.035984385;
    for (vector<double>::iterator wl_o = wl.begin(), tail_o = tail.begin(); wl_o < wl.end(); wl_o++, tail_o++) {
      inner_bg = wl_o - window_width / 2L; if (inner_bg < wl.begin()) inner_bg = wl.begin();
      inner_nd = wl_o + window_width / 2L; if (inner_nd > wl.end()) inner_nd = wl.end();
      (*tail_o) = 0.0;
      for(vector<double>::iterator wl_i = inner_bg, dwl_i = dwl.begin(), cs_i = cs.begin(); wl_i < inner_nd; wl_i++, dwl_i++, cs_i++)
       {
         (*tail_o) = (*tail_o) +  (*dwl_i) * (*cs_i) * exp(t1 + t2 * abs((*wl_i) - (*wl_o)));
       }
    }
    return Rcpp::List::create(Rcpp::Named("tail") = tail);
}
