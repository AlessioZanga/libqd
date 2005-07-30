/*
 * tests/qd_timer.cpp
 *
 * This work was supported by the Director, Office of Science, Division
 * of Mathematical, Information, and Computational Sciences of the
 * U.S. Department of Energy under contract number DE-AC03-76SF00098.
 *
 * Copyright (c) 2000-2004
 *
 * Contains code to time basic operations.
 */

#include <cstring>
#include <cmath>
#include <iostream>
#include <qd/qd.h>
#include <qd/fpu.h>
#include "timer.h"

using std::cout;
using std::cerr;
using std::endl;
using std::sqrt;
using std::strcmp;

// Global flags passed to the main program.
static bool flag_test_dd = false;
static bool flag_test_qd = false;
bool flag_verbose = false;


template <class T>
class TestSuite {
private:
  static const int base_n;
public:
  void test1();
  void test2();
  void test3();
  void test4();
  void testall();
};

template <class T>
const int TestSuite<T>::base_n = 1000;

template <class T>
void TestSuite<T>::test1() {
  cout << endl;
  cout << "Timing addition / subtraction ..." << endl;

  int n = base_n;
  int i;
  TimeVal tv;
  double t;

  for (;;) {
    T a = T::_pi;
    T b = sqrt(a);
    T c = sqrt(b);
    T d = sqrt(c);

    tic(&tv);
    for (i = 0; i < n; i++) {
      a = b + c;
      b = a - d;
      a = b + c;
      b = a - d;
    }
    t = toc(&tv);
    if (t >= 1.0)
      break;
    if (flag_verbose) {
      cout << "n = " << n << "   t = " << t << endl;
      cout << "a = " << a << endl;
    }
    n *= 2;
  } 

  cout << "Iteration count = " << n *4 << endl;
  cout << "Total time = " << t << endl;
  t /= n * 4;
  cout << "Average time = " << t * 1e6 << " us." << endl;
}

template <class T>
void TestSuite<T>::test2() {
  cout << endl;
  cout << "Timing multiplication ..." << endl;

  int n = base_n;
  int i;
  TimeVal tv;
  double t;

  for (;;) {
    T a = 1.0 + T::_pi * 1.0e-7;
    T b = a + 1.0e-7;
    T c = b + 1.0e-8;
    T d = c + 1.0e-9;

    tic(&tv);
    for (i = 0; i < n; i++) {
      a = b * c;
      b = a * d;
      a = b * c;
      b = a * d;
    }
    t = toc(&tv);
    if (t >= 1.0)
      break;
    if (flag_verbose) {
      cout << "n = " << n << "   t = " << t << endl;
      cout << "a = " << a << endl;
    }
    n *= 2;
  }

  cout << "Iteration count = " << n *4 << endl;
  cout << "Total time = " << t << endl;
  t /= n * 4;
  cout << "Average time = " << t * 1e6 << " us." << endl;
}

template <class T>
void TestSuite<T>::test3() {
  cout << endl;
  cout << "Timing division ..." << endl;

  int n = base_n;
  int i;
  TimeVal tv;
  double t;

  for (;;) {
    T a = 1.0 + T::_pi;
    T b = 2.0 + T::_pi;
    T c = 1.0 + 1.0e-8;
    T d = 1.0 + 1.0e-9;

    tic(&tv);
    for (i = 0; i < n; i++) {
      a = b / c;
      b = a / d;
      a = b / c;
      b = a / d;
    }
    t = toc(&tv);
    if (t >= 1.0)
      break;
    if (flag_verbose) {
      cout << "n = " << n << "   t = " << t << endl;
      cout << "a = " << a << endl;
    }
    n *= 2;
  }

  cout << "Iteration count = " << n *4 << endl;
  cout << "Total time = " << t << endl;
  t /= n * 4;
  cout << "Average time = " << t * 1e6 << " us." << endl;
}

template <class T>
void TestSuite<T>::test4() {
  cout << endl;
  cout << "Timing square root ..." << endl;

  int n = base_n;
  int i;
  TimeVal tv;
  double t;

  for (;;) {
    T a;
    T b = 2.0 + T::_pi;

    tic(&tv);
    for (i = 0; i < n; i++) {
      a = sqrt(b);
    }
    t = toc(&tv);
    if (t >= 1.0)
      break;
    if (flag_verbose) {
      cout << "n = " << n << "   t = " << t << endl;
      cout << "a = " << a << endl;
    }
    n *= 2;
  }

  cout << "Iteration count = " << n << endl;
  cout << "Total time = " << t << endl;
  t /= n;
  cout << "Average time = " << t * 1e6 << " us." << endl;
}

template <class T>
void TestSuite<T>::testall() {
  test1();
  test2();
  test3();
  test4();
}

void print_usage() {
  cout << "qd_test [-h] [-dd] [-qd] [-all]" << endl;
  cout << "  Performs miscellaneous tests of the quad-double library," << endl;
  cout << "  such as polynomial root finding, computation of pi, etc." << endl;
  cout << endl;
  cout << "  -h -help  Prints this usage message." << endl;
  cout << "  -dd       Perform tests with double-double types." << endl;
  cout << "  -qd       Perform tests with quad-double types." << endl;
  cout << "            This is the default." << endl;
  cout << "  -all      Perform both double-double and quad-double tests." << endl;
  cout << "  -v        Verbose output." << endl;
}

int main(int argc, char *argv[]) {
  unsigned int old_cw;
  fpu_fix_start(&old_cw);

  /* Parse the arguments. */
  char *arg;
  for (int i = 1; i < argc; i++) {
    arg = argv[i];
    if (strcmp(arg, "-h") == 0 || strcmp(arg, "-help") == 0) {
      print_usage();
      std::exit(0);
    } else if (strcmp(arg, "-dd") == 0) {
      flag_test_dd = true;
    } else if (strcmp(arg, "-qd") == 0) {
      flag_test_qd = true;
    } else if (strcmp(arg, "-all") == 0) {
      flag_test_dd = flag_test_qd = true;
    } else if (strcmp(arg, "-v") == 0) {
      flag_verbose = true;
    } else {
      cerr << "Unknown flag `" << arg << "'." << endl;
    }
  }

  /* If no flag, test both double-double and quad-double. */
  if (!flag_test_dd && !flag_test_qd) {
    flag_test_dd = true;
    flag_test_qd = true;
  }

  if (flag_test_dd) {
    TestSuite<dd_real> dd_test;

    cout << endl;
    cout << "Timing dd_real" << endl;
    cout << "--------------" << endl;
    dd_test.testall();
  }

  if (flag_test_qd) {
    TestSuite<qd_real> qd_test;

    cout << endl;
    cout << "Timing qd_real" << endl;
    cout << "--------------" << endl;
    qd_test.testall();
  }
  
  fpu_fix_end(&old_cw);
  return 0;
}





