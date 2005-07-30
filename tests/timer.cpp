/*
 * tests/timer.cpp
 *
 * This work was supported by the Director, Office of Science, Division
 * of Mathematical, Information, and Computational Sciences of the
 * U.S. Department of Energy under contract number DE-AC03-76SF00098.
 *
 * Copyright (c) 2000-2001
 *
 * Contains function used for timing.
 */
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <ctime>
#endif

#include "timer.h"

void tic(TimeVal *tv) {
#ifdef HAVE_SYS_TIME_H
  gettimeofday(tv, 0L);
#else
  time(tv);
#endif
}

double toc(TimeVal *tv) {
  TimeVal tv2;
  
#ifdef HAVE_SYS_TIME_H
  gettimeofday(&tv2, 0L);

  double  sec = (double) (tv2.tv_sec - tv->tv_sec);
  double usec = (double) (tv2.tv_usec - tv->tv_usec);
  
  return (sec + 1.0e-6 * usec);
#else
  time(&tv2);
  return difftime(tv2, *tv);
#endif
}  

