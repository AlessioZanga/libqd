/*
 * include/qd.h
 *
 * This work was supported by the Director, Office of Science, Division
 * of Mathematical, Information, and Computational Sciences of the
 * U.S. Department of Energy under contract number DE-AC03-76SF00098.
 *
 * Copyright (c) 2000-2001
 *
 * Quad-double precision (>= 212-bit significand) floating point arithmetic
 * package, written in ANSI C++, taking full advantage of operator overloading.
 * Uses similar techniques as that of David Bailey's double-double package
 * and that of Jonathan Shewchuk's adaptive precision floating point
 * arithmetic package.  See
 *
 *   http://www.nersc.gov/~dhbailey/mpdist/mpdist.html
 *   http://www.cs.cmu.edu/~quake/robust.html
 *
 * for more details.
 *
 * Yozo Hida
 */
#ifndef _QD_QD_H
#define _QD_QD_H

#include <iostream>
#include <string>
#include <qd/qd_config.h>
#include <qd/dd.h>

class qd_real {
protected:

  /* Eliminates any zeros in the middle component(s). */
  void zero_elim();
  void zero_elim(double &e);

  void renorm();
  void renorm(double &e);

  void quick_accum(double d, double &e);
  void quick_prod_accum(double a, double b, double &e);

  /* Computes  qd * d  where d is known to be a power of 2.
     This can be done component wise.                      */
  friend qd_real mul_pwr2(const qd_real &qd, double d);

  /* Sin / Cos Tables */
  static const qd_real _pi1024;  /* Used in sin / cos routine. */
  static const qd_real sin_table[];
  static const qd_real cos_table[];

public:
  double x[4];    /* The Components. */

  /* Protected Constructor. */
  qd_real(double x0, double x1, double x2, double x3);
  qd_real(const double *xx);


  /* Useful constants. */
  static const qd_real _2pi;
  static const qd_real _pi;
  static const qd_real _3pi4;
  static const qd_real _pi2;
  static const qd_real _pi4;
  static const qd_real _pi8;
  static const qd_real _e;
  static const qd_real _log2;
  static const qd_real _log10;
  static const qd_real _nan;

  static const double _eps;      /* = 2^-212. */
  static const int _ndigits;     /* = 64.     */

  /* Constructors */
  qd_real();
  qd_real(const char *s);
  qd_real(const dd_real &dd);
  qd_real(double d);
  qd_real(int i);
  qd_real(long int i);
  qd_real(unsigned int i);
  qd_real(unsigned long int i);

  /* Member Access */
  double operator[](int i) const;

  static void abort(const char *msg);

  /* Addition */
  friend qd_real operator+(const qd_real &a, const qd_real &b);
  friend qd_real operator+(const dd_real &a, const qd_real &b);
  friend qd_real operator+(const qd_real &a, const dd_real &b);
  friend qd_real operator+(const qd_real &a, double b);
  friend qd_real operator+(double a, const qd_real &b);

  /* Self-Addition */
  qd_real &operator+=(double a);
  qd_real &operator+=(const dd_real &a);
  qd_real &operator+=(const qd_real &a);

  /* Subtraction */
  friend qd_real operator-(const qd_real &a, const qd_real &b);
  friend qd_real operator-(const dd_real &a, const qd_real &b);
  friend qd_real operator-(const qd_real &a, const dd_real &b);
  friend qd_real operator-(const qd_real &a, double b);
  friend qd_real operator-(double a, const qd_real &b);

  /* Self-Subtraction */
  qd_real &operator-=(double a);
  qd_real &operator-=(const dd_real &a);
  qd_real &operator-=(const qd_real &a);

  /* Multiplication */
  friend qd_real operator*(const qd_real &a, const qd_real &b);
  friend qd_real operator*(const dd_real &a, const qd_real &b);
  friend qd_real operator*(const qd_real &a, const dd_real &b);

#define QD_REAL_OP_MUL_T_DECL( T )			\
  friend qd_real operator*(const qd_real &a, T b);	\
  friend qd_real operator*(T a, const qd_real &b);

  QD_REAL_OP_MUL_T_DECL( int );
  QD_REAL_OP_MUL_T_DECL( long int );
  QD_REAL_OP_MUL_T_DECL( unsigned int );
  QD_REAL_OP_MUL_T_DECL( unsigned long int );
  QD_REAL_OP_MUL_T_DECL( float );
  QD_REAL_OP_MUL_T_DECL( double );
  QD_REAL_OP_MUL_T_DECL( long double );

  /* Self-Multiplication */
  template<typename T>
  qd_real &operator*=(T a);
  qd_real &operator*=(const dd_real &a);
  qd_real &operator*=(const qd_real &a);

  /* Division */
  friend qd_real operator/(const qd_real &a, const qd_real &b);
  friend qd_real operator/(const dd_real &a, const qd_real &b);
  friend qd_real operator/(const qd_real &a, const dd_real &b);
  friend qd_real operator/(const qd_real &a, double b);
  friend qd_real operator/(double a, const qd_real &b);

  /* Self-Division */
  qd_real &operator/=(double a);
  qd_real &operator/=(const dd_real &a);
  qd_real &operator/=(const qd_real &a);

  /* Square, Square Root, Power, N-th Root. */
  friend qd_real sqr(const qd_real &a);
  friend qd_real sqrt(const qd_real &a);
  friend qd_real pow(const qd_real &a, int n);
  friend qd_real pow(const qd_real &a, const qd_real &b);
  friend qd_real npwr(const qd_real &a, int n);
  qd_real operator^(int n) const;
  friend qd_real nroot(const qd_real &a, int n);

  /* Unary Minus */
  qd_real operator-() const;

  /* Remainder */
  friend qd_real rem(const qd_real &a, const qd_real &b);
  friend qd_real drem(const qd_real &a, const qd_real &b);
  friend qd_real divrem(const qd_real &a, const qd_real &b, qd_real &r);

  /* Assignment */
#define QD_REAL_OP_EQUAL_T_DECL( T ) \
  qd_real &operator=(T a);

  QD_REAL_OP_EQUAL_T_DECL( int );
  QD_REAL_OP_EQUAL_T_DECL( long int );
  QD_REAL_OP_EQUAL_T_DECL( unsigned int );
  QD_REAL_OP_EQUAL_T_DECL( unsigned long int );
  QD_REAL_OP_EQUAL_T_DECL( float );
  QD_REAL_OP_EQUAL_T_DECL( double );
  QD_REAL_OP_EQUAL_T_DECL( long double );

  qd_real &operator=(const dd_real &a);
  qd_real &operator=(const char *s);

  /* Cast */
  operator dd_real() const;
  operator double() const;
  operator int() const;

  /* Equality Comparison */
  friend bool operator==(const qd_real &a, const qd_real &b);
  friend bool operator==(const qd_real &a, const dd_real &b);
  friend bool operator==(const dd_real &a, const qd_real &b);
  friend bool operator==(double a, const qd_real &b);
  friend bool operator==(const qd_real &a, double b);

  /* Less-Than Comparison */
  friend bool operator<(const qd_real &a, const qd_real &b);
  friend bool operator<(const qd_real &a, const dd_real &b);
  friend bool operator<(const dd_real &a, const qd_real &b);
  friend bool operator<(double a, const qd_real &b);
  friend bool operator<(const qd_real &a, double b);

  /* Greater-Than Comparison */
  friend bool operator>(const qd_real &a, const qd_real &b);
  friend bool operator>(const qd_real &a, const dd_real &b);
  friend bool operator>(const dd_real &a, const qd_real &b);
  friend bool operator>(double a, const qd_real &b);
  friend bool operator>(const qd_real &a, double b);

  /* Less-Than-Or-Equal-To Comparison */
  friend bool operator<=(const qd_real &a, const qd_real &b);
  friend bool operator<=(const qd_real &a, const dd_real &b);
  friend bool operator<=(const dd_real &a, const qd_real &b);
  friend bool operator<=(double a, const qd_real &b);
  friend bool operator<=(const qd_real &a, double b);

  /* Greater-Than-Or-Equal-To Comparison */
  friend bool operator>=(const qd_real &a, const qd_real &b);
  friend bool operator>=(const qd_real &a, const dd_real &b);
  friend bool operator>=(const dd_real &a, const qd_real &b);
  friend bool operator>=(double a, const qd_real &b);
  friend bool operator>=(const qd_real &a, double b);

  /* Not-Equal-To Comparison */
  friend bool operator!=(const qd_real &a, const qd_real &b);
  friend bool operator!=(const qd_real &a, const dd_real &b);
  friend bool operator!=(const dd_real &a, const qd_real &b);
  friend bool operator!=(double a, const qd_real &b);
  friend bool operator!=(const qd_real &a, double b);

  /* Other Comparisons.  These are faster than directly
     comparing to 0 or 1.                               */
  bool is_zero() const;
  bool is_one() const;
  bool is_positive() const;
  bool is_negative() const;

  /* Micellaneous algebraic operations */
  friend qd_real fabs(const qd_real &a);
  friend qd_real abs(const qd_real &a);    /* same as fabs */

  /* Computes  a * 2^n. */
  friend qd_real ldexp(const qd_real &a, int n);

  /* Rounding */
  friend qd_real nint(const qd_real &a);
  friend qd_real quick_nint(const qd_real &a);
  friend qd_real floor(const qd_real &a);
  friend qd_real ceil(const qd_real &a);
  friend qd_real aint(const qd_real &a);

  /* Trigonometric Functions */
  friend qd_real sin(const qd_real &a);
  friend qd_real cos(const qd_real &a);
  friend qd_real tan(const qd_real &a);
  friend void sincos(const qd_real &a, qd_real &s, qd_real &c);

  /* Inverse Trigonometric Functions */
  friend qd_real asin(const qd_real &a);
  friend qd_real acos(const qd_real &a);
  friend qd_real atan(const qd_real &a);
  friend qd_real atan2(const qd_real &y, const qd_real &x);

  /* Exponential / Logarithm */
  friend qd_real exp(const qd_real &a);
  friend qd_real log(const qd_real &a);
  friend qd_real log10(const qd_real &a);

  /* Hyperbolic Functions */
  friend qd_real sinh(const qd_real &a);
  friend qd_real cosh(const qd_real &a);
  friend qd_real tanh(const qd_real &a);
  friend void sincosh(const qd_real &a, qd_real &sin_qd, qd_real &cos_qd);

  /* Inverse Hyperbolic Functions */
  friend qd_real asinh(const qd_real &a);
  friend qd_real acosh(const qd_real &a);
  friend qd_real atanh(const qd_real &a);

  /* Random Number */
  static qd_real rand(void);
  friend qd_real qdrand(void);

  /* Min / Max */
  friend qd_real max(const qd_real &a, const qd_real &b);
  friend qd_real max(const qd_real &a, const qd_real &b, const qd_real &c);
  friend qd_real min(const qd_real &a, const qd_real &b);
  friend qd_real min(const qd_real &a, const qd_real &b, const qd_real &c);

  /* Polynomial Evaluator / Solver */
  friend qd_real polyroot(const qd_real *c, int n,
                          const qd_real &x0, double thresh);
  friend qd_real polyeval(const qd_real *c, int n, const qd_real &x);

  /* Input / Output */
  friend std::ostream &operator<<(std::ostream &s, const qd_real &a);
  friend std::istream &operator>>(std::istream &s, qd_real &a);

  void to_digits(char *s, int &expn, int precision = _ndigits) const;
  void write(char *s, int precision = _ndigits,
      bool showpos = false, bool uppercase = false) const;
      /* Note: s must hold d+8 chars. */
  std::string write(int precision = _ndigits, int width = 0,
      std::ios_base::fmtflags floatfield = (std::ios_base::fmtflags) 0,
      std::ios_base::fmtflags adjustfield = (std::ios_base::fmtflags) 0,
      bool showpos = false, bool uppercase = false, char fill = ' ') const;
  static int read(const char *s, qd_real &a);

#ifdef QD_DEBUG
  /* Debugging methods */
  void dump_components() const;
  void dump_bits() const;
  void dump() const;

  static qd_real debug_rand();
#endif

};

qd_real polyroot(const qd_real *c, int n,
                 const qd_real &x0, double thresh = 1.0e-62);

qd_real qdrand(void);
qd_real sqrt(const qd_real &a);

#ifdef QD_INLINE
#include <qd/qd_inline.h>
#endif

#endif /* _QD_QD_H */




