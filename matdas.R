#' Exponential Function
#'
#' Computes the value of an exponential function y = a * exp(b * x).
#'
#' @param a Coefficient of the exponential function.
#' @param b Exponent constant.
#' @param x Input value.
#' @return The computed value of y.
#' @export
exponential_function <- function(a, b, x) {
  y <- a * exp(b * x)
  return(y)
}

#' Nth Root Function
#'
#' Computes the nth root of a number x, i.e., x^(1/n).
#' If x is negative and n is even, an error is raised.
#'
#' @param x The number to compute the nth root.
#' @param n The root degree.
#' @return The nth root of x.
#' @export
nth_root <- function(x, n) {
  if (x < 0 & n %% 2 == 0) {
    stop("Akar pangkat genap dari bilangan negatif tidak terdefinisi.")
  }
  result <- x^(1 / n)
  return(result)
}

#' Arithmetic Sum
#'
#' Computes the sum of an arithmetic series given the first term, the last term, and the number of terms.
#'
#' @param a The first term of the series.
#' @param l The last term of the series.
#' @param n The number of terms in the series.
#' @return The sum of the arithmetic series.
#' @export
arithmetic_sum <- function(a, l, n) {
  sum <- (n / 2) * (a + l)
  return(sum)
}

#' Geometric Sum
#'
#' Computes the sum of a geometric series given the first term, the common ratio, and the number of terms.
#' If the common ratio is 1, the sum is computed as a * n.
#'
#' @param a The first term of the series.
#' @param r The common ratio.
#' @param n The number of terms in the series.
#' @return The sum of the geometric series.
#' @export
geometric_sum <- function(a, r, n) {
  if (r == 1) {
    sum <- a * n
  } else {
    sum <- a * (1 - r^n) / (1 - r)
  }
  return(sum)
}

#' Logarithm Base 10
#'
#' Computes the logarithm of a number x to the base 10.
#' The input x must be positive.
#'
#' @param x The number for which the logarithm is computed.
#' @return The base-10 logarithm of x.
#' @export
log_base_10 <- function(x) {
  if (x <= 0) {
    stop("Logaritma hanya terdefinisi untuk bilangan positif.")
  }
  result <- log10(x)
  return(result)
}

#' Logarithm with Custom Base
#'
#' Computes the logarithm of a number x to the base b.
#' The input x must be positive and the base b must be positive and not equal to 1.
#'
#' @param x The number for which the logarithm is computed.
#' @param b The base of the logarithm.
#' @return The logarithm of x to the base b.
#' @export
log_base_b <- function(x, b) {
  if (x <= 0 || b <= 0 || b == 1) {
    stop("Basis dan input harus positif, dan basis tidak boleh 1.")
  }
  result <- log(x) / log(b)
  return(result)
}

#' Matrix Multiplication
#'
#' Multiplies two matrices A and B. The number of columns in matrix A must match the number of rows in matrix B.
#'
#' @param A The first matrix.
#' @param B The second matrix.
#' @return The result of the matrix multiplication of A and B.
#' @export
matrix_multiply <- function(A, B) {
  if (ncol(A) != nrow(B)) {
    stop("Jumlah kolom matriks A harus sama dengan jumlah baris matriks B.")
  }
  result <- A %*% B
  return(result)
}
