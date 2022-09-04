/*
 * include/inline.h
 *
 * This work was supported by the Director, Office of Science, Division
 * of Mathematical, Information, and Computational Sciences of the
 * U.S. Department of Energy under contract number DE-AC03-76SF00098.
 *
 * Copyright (c) 2000-2001
 *
 * This file contains the basic functions used both by double-double
 * and quad-double package.  These are declared as inline functions as
 * they are the smallest building blocks of the double-double and 
 * quad-double arithmetic.
 */
#ifndef _QD_INLINE_H
#define _QD_INLINE_H

#define _QD_SPLITTER 134217729.0               /* = 2^27 + 1 */

#include <cmath>

#if defined(_MSC_VER) && (_MSC_VER <= 1200)
/* For some reason VC++ 6.0 doesn't include these in std namespace */
#define _QD_STD_FLOOR(x)     floor(x)
#define _QD_STD_CEIL(x)      ceil(x)
#define _QD_STD_SINH(x)      sinh(x)
#define _QD_STD_COSH(x)      cosh(x)
#define _QD_STD_ABS(x)       fabs(x)
#define _QD_STD_LDEXP(x, y)  ldexp(x, y)
#define _QD_STD_SQRT(x)      sqrt(x)
#define _QD_STD_EXP(x)       exp(x)
#define _QD_STD_LOG(x)       log(x)
#define _QD_STD_LOG10(x)     log10(x)
#define _QD_STD_ATAN2(x, y)  atan2(x, y)
#define _QD_STD_POW(x, y)    pow(x, y)
#else
#define _QD_STD_FLOOR(x)     std::floor(x)
#define _QD_STD_CEIL(x)      std::ceil(x)
#define _QD_STD_SINH(x)      std::sinh(x)
#define _QD_STD_COSH(x)      std::cosh(x)
#define _QD_STD_ABS(x)       std::abs(x)
#define _QD_STD_LDEXP(x, y)  std::ldexp(x, y)
#define _QD_STD_SQRT(x)      std::sqrt(x)
#define _QD_STD_EXP(x)       std::exp(x)
#define _QD_STD_LOG(x)       std::log(x)
#define _QD_STD_LOG10(x)     std::log10(x)
#define _QD_STD_ATAN2(x, y)  std::atan2(x, y)
#define _QD_STD_POW(x, y)    std::pow(x, y)
#endif

namespace qd {

static const double _d_nan = 0.0 / 0.0;

/*********** Basic Functions ************/
/* Computes fl(a+b) and err(a+b).  Assumes |a| >= |b|. */
inline double quick_two_sum(double a, double b, double &err) {
  double s = a + b;
  err = b - (s - a);
  return s;
}

/* Computes fl(a-b) and err(a-b).  Assumes |a| >= |b| */
inline double quick_two_diff(double a, double b, double &err) {
  double s = a - b;
  err = (a - s) - b;
  return s;
}

/* Computes fl(a+b) and err(a+b).  */
inline double two_sum(double a, double b, double &err) {
  double s = a + b;
  double bb = s - a;
  err = (a - (s - bb)) + (b - bb);
  return s;
}

/* Computes fl(a-b) and err(a-b).  */
inline double two_diff(double a, double b, double &err) {
  double s = a - b;
  double bb = s - a;
  err = (a - (s - bb)) - (b + bb);
  return s;
}

#ifndef QD_HAS_FMA
/* Computes high word and lo word of a */
inline void split(double a, double &hi, double &lo) {
  double temp;
  temp = _QD_SPLITTER * a;
  hi = temp - (temp - a);
  lo = a - hi;
}
#endif

/* Computes fl(a*b) and err(a*b). */
inline double two_prod(double a, double b, double &err) {
#ifdef QD_HAS_FMA
  double p = -a * b;
  err = a * b + p;
  return -p;
#else
  double a_hi, a_lo, b_hi, b_lo;
  double p = a * b;
  split(a, a_hi, a_lo);
  split(b, b_hi, b_lo);
  err = ((a_hi * b_hi - p) + a_hi * b_lo + a_lo * b_hi) + a_lo * b_lo;
  return p;
#endif
}

/* Computes fl(a*a) and err(a*a).  Faster than the above method. */
inline double two_sqr(double a, double &err) {
#ifdef QD_HAS_FMA
  double p = -a * a;
  err = a * a + p;
  return -p;
#else
  double hi, lo;
  double q = a * a;
  split(a, hi, lo);
  err = ((hi * hi - q) + 2.0 * hi * lo) + lo * lo;
  return q;
#endif
}

/* Computes the nearest integer to d. */
inline double nint(double d) {
  if (d == _QD_STD_FLOOR(d))
    return d;
  return _QD_STD_FLOOR(d + 0.5);
}

/* Computes the truncated integer. */
inline double aint(double d) {
  return (d >= 0.0) ? _QD_STD_FLOOR(d) : _QD_STD_CEIL(d);
}

/* These are provided to give consistent 
   interface for double with double-double and quad-double. */
inline void sincosh(double t, double &sinh_t, double &cosh_t) {
  sinh_t = _QD_STD_SINH(t);
  cosh_t = _QD_STD_COSH(t);
}

inline double sqr(double t) {
  return t * t;
}

}

#endif /* _QD_INLINE_H */
