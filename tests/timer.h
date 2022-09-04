/*
 * tests/timer.h
 *
 * This work was supported by the Director, Office of Science, Division
 * of Mathematical, Information, and Computational Sciences of the
 * U.S. Department of Energy under contract number DE-AC03-76SF00098.
 *
 * Copyright (c) 2000-2001
 *
 * Contains function used for timing.
 */

#ifndef _TIMER_H_
#define _TIMER_H_

#include "config.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
typedef struct timeval TimeVal;
#else
#include <ctime>
typedef time_t TimeVal;
#endif

void   tic(TimeVal *tv);   /* start timing. */
double toc(TimeVal *tv);   /* stop  timing. */

#endif  /* _TIMER_H_ */

