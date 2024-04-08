// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// min_key_vertex
int min_key_vertex(NumericVector key, LogicalVector mst_set);
RcppExport SEXP _mstPackage_min_key_vertex(SEXP keySEXP, SEXP mst_setSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type key(keySEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type mst_set(mst_setSEXP);
    rcpp_result_gen = Rcpp::wrap(min_key_vertex(key, mst_set));
    return rcpp_result_gen;
END_RCPP
}
// generate_random_adjacency_matrix
NumericMatrix generate_random_adjacency_matrix(int n, Nullable<int> num_zeros);
RcppExport SEXP _mstPackage_generate_random_adjacency_matrix(SEXP nSEXP, SEXP num_zerosSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< Nullable<int> >::type num_zeros(num_zerosSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_random_adjacency_matrix(n, num_zeros));
    return rcpp_result_gen;
END_RCPP
}
// prim_mst_rcpp
NumericMatrix prim_mst_rcpp(NumericMatrix adj_matrix);
RcppExport SEXP _mstPackage_prim_mst_rcpp(SEXP adj_matrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type adj_matrix(adj_matrixSEXP);
    rcpp_result_gen = Rcpp::wrap(prim_mst_rcpp(adj_matrix));
    return rcpp_result_gen;
END_RCPP
}
// kruskal_mst_rcpp
NumericMatrix kruskal_mst_rcpp(NumericMatrix adj_matrix);
RcppExport SEXP _mstPackage_kruskal_mst_rcpp(SEXP adj_matrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type adj_matrix(adj_matrixSEXP);
    rcpp_result_gen = Rcpp::wrap(kruskal_mst_rcpp(adj_matrix));
    return rcpp_result_gen;
END_RCPP
}
// adj_matrix_to_edges
NumericMatrix adj_matrix_to_edges(NumericMatrix adj_matrix);
RcppExport SEXP _mstPackage_adj_matrix_to_edges(SEXP adj_matrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type adj_matrix(adj_matrixSEXP);
    rcpp_result_gen = Rcpp::wrap(adj_matrix_to_edges(adj_matrix));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_mstPackage_min_key_vertex", (DL_FUNC) &_mstPackage_min_key_vertex, 2},
    {"_mstPackage_generate_random_adjacency_matrix", (DL_FUNC) &_mstPackage_generate_random_adjacency_matrix, 2},
    {"_mstPackage_prim_mst_rcpp", (DL_FUNC) &_mstPackage_prim_mst_rcpp, 1},
    {"_mstPackage_kruskal_mst_rcpp", (DL_FUNC) &_mstPackage_kruskal_mst_rcpp, 1},
    {"_mstPackage_adj_matrix_to_edges", (DL_FUNC) &_mstPackage_adj_matrix_to_edges, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_mstPackage(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}