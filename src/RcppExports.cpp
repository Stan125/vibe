// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// perm_cpp
Rcpp::List perm_cpp(Rcpp::NumericMatrix perms, Rcpp::CharacterVector model_ids, Rcpp::NumericVector gofs_vector);
RcppExport SEXP _vibe_perm_cpp(SEXP permsSEXP, SEXP model_idsSEXP, SEXP gofs_vectorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type perms(permsSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type model_ids(model_idsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type gofs_vector(gofs_vectorSEXP);
    rcpp_result_gen = Rcpp::wrap(perm_cpp(perms, model_ids, gofs_vector));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_vibe_perm_cpp", (DL_FUNC) &_vibe_perm_cpp, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_vibe(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
