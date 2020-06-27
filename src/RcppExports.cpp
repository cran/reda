//
// R package reda by Wenjie Wang, Haoda Fu, and Jun Yan
// Copyright (C) 2015-2020
//
// This file is part of the R package reda.
//
// The R package reda is free software: You can redistribute it and/or
// modify it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or any later
// version (at your option). See the GNU General Public License at
// <https://www.gnu.org/licenses/> for details.
//
// The R package reda is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//

// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/reda.h"
#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// cpp_np_mcf
Rcpp::List cpp_np_mcf(const arma::vec& time1, const arma::vec& time2, const arma::uvec& id, const arma::vec& event, const unsigned int& point_method, const unsigned int& var_method, const unsigned int& ci_method, const double& ci_level, const unsigned int& var_bootstrap_method, const unsigned int& var_bootstrap_B);
RcppExport SEXP _reda_cpp_np_mcf(SEXP time1SEXP, SEXP time2SEXP, SEXP idSEXP, SEXP eventSEXP, SEXP point_methodSEXP, SEXP var_methodSEXP, SEXP ci_methodSEXP, SEXP ci_levelSEXP, SEXP var_bootstrap_methodSEXP, SEXP var_bootstrap_BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type time1(time1SEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type time2(time2SEXP);
    Rcpp::traits::input_parameter< const arma::uvec& >::type id(idSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type event(eventSEXP);
    Rcpp::traits::input_parameter< const unsigned int& >::type point_method(point_methodSEXP);
    Rcpp::traits::input_parameter< const unsigned int& >::type var_method(var_methodSEXP);
    Rcpp::traits::input_parameter< const unsigned int& >::type ci_method(ci_methodSEXP);
    Rcpp::traits::input_parameter< const double& >::type ci_level(ci_levelSEXP);
    Rcpp::traits::input_parameter< const unsigned int& >::type var_bootstrap_method(var_bootstrap_methodSEXP);
    Rcpp::traits::input_parameter< const unsigned int& >::type var_bootstrap_B(var_bootstrap_BSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_np_mcf(time1, time2, id, event, point_method, var_method, ci_method, ci_level, var_bootstrap_method, var_bootstrap_B));
    return rcpp_result_gen;
END_RCPP
}
// rrisk_exponential
Rcpp::NumericVector rrisk_exponential(arma::mat z, arma::mat zCoef);
RcppExport SEXP _reda_rrisk_exponential(SEXP zSEXP, SEXP zCoefSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type z(zSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type zCoef(zCoefSEXP);
    rcpp_result_gen = Rcpp::wrap(rrisk_exponential(z, zCoef));
    return rcpp_result_gen;
END_RCPP
}
// rrisk_linear
Rcpp::NumericVector rrisk_linear(arma::mat z, arma::mat zCoef);
RcppExport SEXP _reda_rrisk_linear(SEXP zSEXP, SEXP zCoefSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type z(zSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type zCoef(zCoefSEXP);
    rcpp_result_gen = Rcpp::wrap(rrisk_linear(z, zCoef));
    return rcpp_result_gen;
END_RCPP
}
// rrisk_excess
Rcpp::NumericVector rrisk_excess(arma::mat z, arma::mat zCoef);
RcppExport SEXP _reda_rrisk_excess(SEXP zSEXP, SEXP zCoefSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type z(zSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type zCoef(zCoefSEXP);
    rcpp_result_gen = Rcpp::wrap(rrisk_excess(z, zCoef));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_reda_cpp_np_mcf", (DL_FUNC) &_reda_cpp_np_mcf, 10},
    {"_reda_rrisk_exponential", (DL_FUNC) &_reda_rrisk_exponential, 2},
    {"_reda_rrisk_linear", (DL_FUNC) &_reda_rrisk_linear, 2},
    {"_reda_rrisk_excess", (DL_FUNC) &_reda_rrisk_excess, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_reda(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
