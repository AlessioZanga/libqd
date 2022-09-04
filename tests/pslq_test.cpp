/*
 * tests/pslq_test.cpp
 *
 * This work was supported by the Director, Office of Science, Division
 * of Mathematical, Information, and Computational Sciences of the
 * U.S. Department of Energy under contract number DE-AC03-76SF00098.
 *
 * Copyright (c) 2000-2001
 *
 * A driver for the pslq program which exercises the double-double and 
 * quad-double library.
 */

#include <cmath>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <ctime>
#include <qd/fpu.h>

#include "timer.h"
#include "pslq.h"

using std::cout;
using std::cerr;
using std::endl;
using std::abs;
using std::strcmp;
using std::rand;

#ifndef _MSC_VER
using std::time;
using std::atoi;
#endif

bool flag_random = false;
bool flag_verbose = false;
bool flag_double_pslq = false;
bool flag_dd_pslq = false;
bool flag_qd_pslq = false;

/* Computes the value of the given n-th degree polynomial at point x
   where the (n+1) coefficients of the polynomial is given in a. */
template <class T>
T polyeval(T *a, int n, T x) {
  /* Use Horner's evaluation scheme. */

  T t = a[n];
  for (int i = n-1; i >= 0; i--) {
    t *= x;
    t += a[i];
  }

  return t;
}

/* Computes a root near x0 of the given n-th degree polynomial
   where the (n+1) coefficients of the polynomial is given in a. */
template <class T>
T polyroot(T *a, int n, T x0, double eps) {
  /* Use Newton iteration. */

  T *da = new T[n];
  
  /* Compute the coefficients of the derivatives. */
  for (int i = 1; i <= n; i++)
    da[i-1] = ((double) i) * a[i];
  
  /* Perform Newton iteration. */
  T x = x0;
  T corr;
  static const double und = 2.22507385850721e-308;
  do {
    corr = polyeval<T>(a, n, x) / polyeval<T>(da, n-1, x);
    x -= corr;
  } while (abs(corr) > (eps * abs(x) + und));

  delete [] da;
  return x;
}

/* Creates a random polynomial (integer coefficients) of degree
   n-1, solves for one of its root, and tries to reconstruct the 
   original polynomial from the root by performing PSLQ on 
   1, r, r^2, ..., r^{n-1}.                                       

   Note that this test can report false failures, if the random
   polynomial has a non-trivial factor (with real coefficients).
*/
template <class T>
bool pslq_test(int n, double eps, int seed = 0, int max_itr = 1000) {
  T *x, *a, *b;
  T r, t;
  int err;
  TimeVal tv;
  double tm;

  a = new T[n];
  b = new T[n];
  x = new T[n];

  /* Randomize seed. */
  std::srand((unsigned int) seed);

  /* Construct a random polynomial, making sure the
     the first and the second coefficients are non-zero.  */
  if (flag_random) {
    a[0] = ((rand() % 2 == 0) ? 1 : -1) * (rand() % 9+1);
    a[n-1] = ((rand() % 2 == 0) ? 1 : -1) * (rand() % 9+1);
    for (int i = 1; i < n-1; i++) {
      a[i] = (T) (rand() % 19 - 9);
    }
  } else {
    for (int i = 0; i < n; i++)
      a[i] = (T) (((7*i+3) % 19) - 9);
  }

  if (flag_verbose) {
    cout << "Original polynomial:" << endl << "  ";
    for (int i = 0; i < n; i++)
      cout << (double) a[i] << " ";
    cout << endl;
  }

  /* Find a root */
  r = polyroot<T>(a, n-1, 0.0, eps);
  if (flag_verbose)
    cout << "Root: " << r << endl;

  /* Check the root. */
  t = polyeval<T>(a, n-1, r);
  if (flag_verbose)
    cout << " p(r) = " << t << endl;

  /* Fill in vector x with powers of r. */
  t = 1.0;
  for (int i = 0; i < n; i++, t *= r)
    x[i] = t;
  
  /* Reconstruct polynomial. */
  tic(&tv);
  err = pslq<T>(x, n, b, eps, max_itr);
  tm = toc(&tv);

  if (!err) {
    bool same = true;
    double sign = 0.0;

    if (a[0] == b[0])
      sign = 1.0;
    else if (a[0] == -b[0])
      sign = -1.0;
    else 
      same = false;

    if (flag_verbose) 
      cout << "Reconstructed polynomial:" << endl << "  ";
    for (int i = 0; i < n; i++) {
      if (a[i] != sign * b[i])
        same = false;
      if (flag_verbose)
        cout << (double) b[i] << " ";
    }
    cout << endl;

    if (!same)
      err = -1;
  }

  delete [] x;
  delete [] a;
  delete [] b;

  if (err)
    cout << "Test FAILED." << endl;
  else
    cout << "Test passed." << endl;
  cout << "Elapsed time: " << tm << " seconds." << endl;
  return !err;
}

void print_usage() {
  cout << "pslq_test [-h] [-n N] [-d] [-dd] [-qd] [-all] [-verbose]" << endl;
  cout << "  Performs the PSLQ algorithm on 1, r, r^2, ..., r^{n-1}" << endl;
  cout << "  where r is a root of a randomly constructed integer " << endl;
  cout << "  coefficient polynomial of degree n-1.  PSLQ algorithm" << endl;
  cout << "  should reconstruct the polynomial in most cases where" << endl;
  cout << "  the degree is not too high and the polynomial is" << endl;
  cout << "  irreducible over the rationals." << endl;
  cout << endl;
  cout << "  -h -help  Print this usage message and exit." << endl;
  cout << "  -n N      Use n reals in PSLQ algorithm (n-1 degree polynomial)." << endl;
  cout << "            Here n should be even to ensure the polynomial has" << endl;
  cout << "            at least one real root.  This flag will result" << endl;
  cout << "            in random n-1 degree polynomial to be chosen." << endl;
  cout << "  -d        Perform PSLQ with double precision (53 bit mantissa)." << endl;
  cout << "  -dd       Perform PSLQ with double-double precision." << endl;
  cout << "            (about 106 bits of significand)." << endl;
  cout << "  -qd       Perform PSLQ with quad-double precision." << endl;
  cout << "            (about 212 bits of significand).  This is the default." << endl;
  cout << "  -all      Perform PSLQ with all three precisions above." << endl;
  cout << "  -verbose" << endl;
  cout << "  -v        Output PSLQ iteration step information.  Mostly" << endl;
  cout << "            for debugging purposes." << endl;
}

int main(int argc, char **argv) {
  int n = 4;
  char *arg;
  int tmp;
  int seed;

  /* Parse the command-line arguments. */
  for (int i = 1; i < argc; i++) {
    arg = argv[i];
    if (strcmp(arg, "-h") == 0 || strcmp(arg, "-help") == 0) {
      print_usage();
      return 0;
    } else if (strcmp(arg, "-n") == 0) {
      if (++i < argc) {
        tmp = atoi(argv[i]);
        if (tmp <= 1 || tmp > 1024)
          cerr << "Invalid n." << endl;
        else {
          n = tmp;
          flag_random = true;
        }
      } else {
        cerr << "Number expected after `-n'." << endl;
      }
    } else if (strcmp(arg, "-d") == 0) {
      flag_double_pslq = true;
    } else if (strcmp(arg, "-dd") == 0) {
      flag_dd_pslq = true;
    } else if (strcmp(arg, "-qd") == 0) {
      flag_qd_pslq = true;
    } else if (strcmp(arg, "-all") == 0) {
      flag_double_pslq = flag_dd_pslq = flag_qd_pslq = true;
    } else if (strcmp(arg, "-v") == 0 || strcmp(arg, "-verbose") == 0) {
      flag_verbose = true;
    } else {
      cerr << "Unknown flag `" << arg << "'." << endl;
    }
  }

  if (!flag_double_pslq && !flag_dd_pslq && !flag_qd_pslq) {
    flag_dd_pslq = true;
    flag_qd_pslq = true;
  }

  /* Reset seed. */
  seed = (int) time(NULL);

  if (flag_random)
    cout << "Using N = " << n << endl << endl;

  unsigned int old_cw;
  fpu_fix_start(&old_cw);

  bool pass = true;
  if (flag_double_pslq) {
    cout << "Performing double-precision PSLQ." << endl;
    if (!flag_random)
      n = 8;
    pass &= pslq_test<double>(n, 1.0e-15, seed, 1000);
  }

  if (flag_dd_pslq) {
    cout << "Performing double-double precision PSLQ." << endl;
    if (!flag_random)
      n = 14;
    pass &= pslq_test<dd_real>(n, 1.0e-30 , seed, 1000);
  }

  if (flag_qd_pslq) {
    cout << "Performing quad-double precision PSLQ." << endl;
    if (!flag_random)
      n = 26;
    pass &= pslq_test<qd_real>(n, 1.0e-60, seed, 30000);
  }

  fpu_fix_end(&old_cw);
  return (pass ? 0 : 1);
}


