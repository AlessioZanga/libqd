module quadglobal
use qdmodule
implicit none
integer ndebug, ndigits, nerror, nquadl
end module

! program tquadtsq2d
subroutine f_main

!   David H. Bailey      2004-12-16
!   This is the Quad-Double Fortran-90 version.

!   This work was supported by the Director, Office of Science, Division
!   of Mathematical, Information, and Computational Sciences of the
!   U.S. Department of Energy under contract number DE-AC03-76SF00098.

!   This program demonstrates the 2D quadrature routine 'quadtsq2d', which employs
!   the error function.  The function quadtsq2d is suitable to integrate
!   a function that is continuous, infinitely differentiable and integrable on a
!   2D finite open interval.  It can also be used for certain integrals on
!   infinite intervals, by making a suitable change of variable -- see below.

!   The function(s) to be integrated is(are) defined in external function
!   subprogram(s) -- see the sample function subprograms below.  The name(s) of
!   the function subprogram(s) must be included in appropriate type and external
!   statements in the main program.

!   Inputs set in parameter statement below:
!   kdebug Debug level setting.  Default = 2.
!   ndp    Digits of precision.  May not exceed mpipl in file mpmod90.f.
!            In some cases, ndp must be significantly greater than the desired
!            tolerance in the result-- see the examples below.
!   neps   Log10 of the desired tolerance in the result (negative integer).
!   nq1    Max number of phases in quadrature routine; adding 1 increases
!            (possibly doubles) the number of accurate digits in the result,
!            but also roughly quadruples the run time.  nq1 > 2.
!   nq2    Space parameter for wk and xk arrays in the calling program.  By
!            default it is set to 8 * 2^nq1.  Increase nq2 if directed by a 
!            message produced in initqts.  Note that the dimension of the
!            wk and xk arrays starts with -1, so the length of these arrays is
!            (nq2+2) * 4 eight-byte words.

use qdmodule
use quadglobal
implicit none
integer i, kdebug, ndp, neps, nq1, nq2, n, n1
parameter (kdebug = 2, ndp = 64, neps = -64, nq1 = 8, nq2 = 8 * 2 ** nq1)
double precision dplog10q, d1, d2, second, tm0, tm1
type (qd_real) cat, catalan, err, quadtsq2d, fun01, fun02, fun03, fun04, &
  t1, t2, t3, t4, wk(-1:nq2), xk(-1:nq2), x1, x2, y1, y2
external quadtsq2d, catalan, fun01, fun02, fun03, fun04, second
integer*4 old_cw

call f_fpu_fix_start (old_cw)

ndebug = kdebug
ndigits = ndp
nerror = 0
nquadl = nq1
write (6, 1) ndigits, neps, nquadl
1 format ('Quadtsq2d test'/'Digits =',i6,'  Epsilon =',i6,'   Quadlevel =',i6)

!   Initialize quadrature tables wk and xk (weights and abscissas).

tm0 = second ()
call initqts (nq1, nq2, wk, xk)
tm1 = second ()
if (nerror > 0) stop
write (6, 2) tm1 - tm0
2 format ('Quadrature initialization completed: cpu time =',f12.6)
cat = catalan ()

!   Begin quadrature tests.

write (6, 11)
11 format (/ &
  'Problem 1: Int_-1^1 Int_-1^1 1/(1+x^2+y^2) dx dy = 4*log(2+sqrt(3))-2*pi/3')
x1 = -1.d0
x2 = 1.d0
y1 = -1.d0
y2 = 1.d0
tm0 = second ()
t1 = quadtsq2d (fun01, x1, x2, y1, y2, nq1, nq2, wk, xk)
tm1 = second ()
write (6, 3) tm1 - tm0
3 format ('Quadrature completed: CPU time =',f12.6/'Result =')
call qdwrite (6, t1)
t2 = 4.d0 * log (2.d0 + sqrt (qdreal (3.d0))) - 2.d0 * qdpi () / 3.d0
call decmdq (t2 - t1, d1, n1)
write (6, 4) d1, n1
4 format ('Actual error =',f10.6,'x10^',i5)

write (6, 12)
12 format (/&
  'Problem 2: Int_0^pi Int_0^pi log (2-cos(s)-cos(t)) = 4*pi*cat- pi^2*log(2)')
x1 = 0.d0
x2 = qdpi()
y1 = 0.d0
y2 = qdpi()
tm0 = second ()
t1 = quadtsq2d (fun02, x1, x2, y1, y2, nq1, nq2, wk, xk)
tm1 = second ()
t2 = 4.d0 * qdpi() * cat - qdpi()**2 * log (qdreal (2.d0))
write (6, 3) tm1 - tm0
call qdwrite (6, t1)
call decmdq (t2 - t1, d1, n1)
write (6, 4) d1, n1

write (6, 13)
13 format (/&
  'Problem 3: Int_0^inf Int_0^inf sqrt(x^2+xy+y^2) * exp(-x-y) = 1 + 3/4*log(3)')
x1 = 0.d0
x2 = 1.d0
y1 = 0.d0
y2 = 1.d0
tm0 = second ()
t1 = quadtsq2d (fun03, x1, x2, y1, y2, nq1, nq2, wk, xk)
tm1 = second ()
t2 = 1.d0 + 0.75d0 * log (qdreal (3.d0))
write (6, 3) tm1 - tm0
call qdwrite (6, t1)
call decmdq (t2 - t1, d1, n1)
write (6, 4) d1, n1

write (6, 14)
14 format (/&
  'Problem 4: Int_0^1 Int_0^1 1/(sqrt((1-x)*(1-y))*(x+y)) dx dy = 4*cat')
x1 = 0.d0
x2 = 1.d0
y1 = 0.d0
y2 = 1.d0
tm0 = second ()
t1 = quadtsq2d (fun04, x1, x2, y1, y2, nq1, nq2, wk, xk)
tm1 = second ()
t2 = 4.d0 * cat
write (6, 3) tm1 - tm0
call qdwrite (6, t1)
call decmdq (t2 - t1, d1, n1)
write (6, 4) d1, n1

call f_fpu_fix_end (old_cw)
stop
end

function fun01 (s, t)

!   fun01 (s,t) = 1/sqrt[1+s^2+t^2]

use qdmodule
implicit none
type (qd_real) fun01, s, t

fun01 = 1.d0 / sqrt (1.d0 + s**2 + t**2)
return
end

function fun02 (s, t)

!   fun02 (s,t) = log (2 - cos(s) - cos(t))

use qdmodule
implicit none
type (qd_real) fun02, s, t

fun02 = log (2.d0 - cos (s) - cos (t))
return
end

function fun03 (s, t)

!   fun03 (s,t) = ((1/s-1)^2 + (1/s-1)*(1/t-1) + (1/t-1)^2) 
!                / (s^2 * t^2 * exp(1/s + 1/t - 2)

use qdmodule
implicit none
type (qd_real) fun03, s, t, s1, t1, sq
external dplog10q

if (s > 3.d-3 .and. t > 3.d-3) then
  s1 = 1.d0 / s - 1.d0
  t1 = 1.d0 / t - 1.d0
  sq = sqrt (s1**2 + s1 * t1 + t1**2)
  fun03 = sq / (s**2 * t**2) * exp (-s1 - t1)
else
  fun03 = 0.d0
endif

return
end

function fun04 (s, t)

!   fun04 (s,t) = 1/(sqrt((1-s)*(1-t)) * (s+t))

use qdmodule
implicit none
type (qd_real) fun04, s, t

fun04 = 1.d0 / (sqrt ((1.d0 - s) * (1.d0 - t)) * (s + t))
return
end

subroutine initqts (nq1, nq2, wk, xk)

!   This subroutine initializes the quadrature arays xk and wk using the
!   function x(t) = tanh (pi/2*sinh(t)).  The argument nq2 is the space
!   allocated for wk and xk in the calling program.  By default it is set to 
!   12 * 2^nq1.  Increase nq2 if directed by a message produced below.
!   Upon completion, wk(-1) = nq1, and xk(-1) = n, the maximum space parameter
!   for these arrays.  In other words, the arrays occupy (wk(i), i = -1 to n)
!   and (xk(i), i = -1 to n), where n = xk(-1).   The array x_k contains 
!   1 minus the abscissas; the wk array contains the weights at these abscissas.

!   David H Bailey    2004-07-28

use qdmodule
use quadglobal
implicit none
integer i, ierror, iprint, j, k, k1, nq1, nq2, ntab, ntabx
real*8 h
parameter (iprint = 1000, ntabx = 1000)
type (qd_real) eps2, erfc, etab(ntabx), p2, spi, t1, t2, t3, t4, t5, u1, u2, &
  wk(-1:nq2), xk(-1:nq2)
external erfc

if (ndebug >= 1) then
  write (6, 1)
1 format ('initqts: Error function quadrature initialization')
endif

eps2 = 1.d-64
p2 = 0.5d0 * qdpi()
h = 0.5d0 ** nq1
wk(-1) = dble (nq1)

do k = 0, nq2
  if (ndebug >= 2 .and. mod (k, iprint) == 0) write (6, *) k, nq2
  t1 = dble (k) * h

!   xk(k) = 1 - tanh (u1) = 1 /(e^u1 * cosh (u1))
!   wk(k) = u2 / cosh (u1)^2
!   where u1 = pi/2 * cosh (t1), u2 = pi/2 * sinh (t1)

  t2 = exp (t1)
  u1 = 0.5d0 * p2 * (t2 + 1.d0 / t2)
  u2 = 0.5d0 * p2 * (t2 - 1.d0 / t2)
  t3 = exp (u2)
  t4 = 0.5d0 * (t3 + 1.d0 / t3)
  xk(k) = 1.d0 / (t3 * t4)
  wk(k) = u1 / t4 ** 2
  if (wk(k) < eps2) goto 100
enddo

write (6, 2) nq2
2 format ('initqts: Table space parameter is too small; value =',i8)
nerror = 91
goto 130

100 continue

xk(-1) = dble (k)
if (ndebug >= 2) then
  write (6, 3) k
3 format ('initqts: Table spaced used =',i8)
endif
goto 130

120 continue

nerror = ierror + 100
write (6, 4) nerror
4 format ('initqts: Error in quadrature initialization; code =',i5)

130  continue

return
end

function quadtsq2d (fun, x1, x2, y1, y2, nq1, nq2, wk, xk)

!   This routine computes the integral of the function in fun on the interval
!   [x1, x2], with up to nq1 iterations, with a target tolerance of 10^nepsilon1.
!   wk and xk are precomputed tables of abscissas and weights.  The function
!   fun is not evaluated at x = x1 or x2.  The array x_k contains 1 minus
!   the abscissas; the wk array contains the weights at these abscissas.

!   David H. Bailey     2004-07-28

use qdmodule
use quadglobal
implicit none
integer i, ierror, ip(0:100), izx, iz1, iz2, iz3, iz4, j, k, k1, k2, n, &
  nds, nq1, nq2, nqq1
parameter (izx = 4)
logical logx1, logx2, logy1, logy2
real*8 d1, d2, d3, d4, dplog10q, h
type (qd_real) ax, bx, ay, by, c10, quadtsq2d, epsilon1, epsilon2, eps1, eps2, &
  err, fun, tsum, s1, s2, s3, t1, t2, t3, t4, tw1, tw2, tw3, tw4, twmx, &
  wij, wk(-1:nq2), xk(-1:nq2), x1, x2, xki, xx1, xx2, y1, y2, ykj, yy1, yy2
external fun, dplog10q
integer jz1(0:nq2), jz2(0:nq2), jz3(0:nq2), jz4(0:nq2)
type (qd_real) twimx, twi1, twi2, twi3, twi4, twjmx, &
  twj1(0:nq2), twj2(0:nq2), twj3(0:nq2), twj4(0:nq2)

ax = 0.5d0 * (x2 - x1)
bx = 0.5d0 * (x2 + x1)
ay = 0.5d0 * (y2 - y1)
by = 0.5d0 * (y2 + y1)
tsum = 0.d0
s1 = 0.d0
s2 = 0.d0
h = 1.d0
c10 = 10.d0
epsilon1 = 1.d-64
epsilon2 = 1.d-64

if (wk(-1) < dble (nq1)) then
  write (6, 1) nq1
1 format ('quadtsq2d: quadrature arrays have not been initialized; nq1 =',i6)
  nerror = 70
  goto 140
endif
nqq1 = dble (wk(-1))
n = dble (xk(-1))

do k = 0, nqq1
  ip(k) = 2 ** k
enddo

do k = 1, nq1
  h = 0.5d0 * h
  s3 = s2
  s2 = s1
  k1 = ip(nqq1-k)
  k2 = ip(nqq1-k+1)
  twmx = 0.d0
  twimx = 0.d0
  twjmx = 0.d0

  do j = 0, n
    jz1(j) = 0
    jz2(j) = 0
    jz3(j) = 0
    jz4(j) = 0
  enddo

!   Evaluate function at level k in x and y, avoiding unnecessary computation.

  do j = 0, n, k1
    ykj = 1.d0 - xk(j)
    yy1 = - ay * ykj + by
    yy2 = ay * ykj + by
    logy1 = yy1 > y1
    logy2 = yy2 < y2
    iz1 = 0
    iz2 = 0
    iz3 = 0
    iz4 = 0

    do i = 0, n, k1
      if (mod (i, k2) /= 0 .or. mod (j, k2) /= 0 .or. k == 1) then
        xki = 1.d0 - xk(i)
        xx1 = - ax * xki + bx
        xx2 = ax * xki + bx
        logx1 = xx1 > x1
        logx2 = xx2 < x2
        wij = wk(i) * wk(j)

        if (logx1 .and. logy1 .and. iz1 < izx .and. jz1(j) < izx) then
          t1 = fun (xx1, yy1)
          tw1 = t1 * wij
          twi1 = abs (tw1)
          twj1(j) = twi1
          if (twi1 < epsilon1) then
            iz1 = iz1 + 1
            jz1(j) = jz1(j) + 1
          else
            iz1 = 0
            jz1(j) = 0
          endif
        else
          t1 = 0.d0
          tw1 = 0.d0
        endif

        if (i > 0 .and. logx2 .and. logy1 .and. iz2 < izx .and. jz2(j) < izx) &
          then
          t2 = fun (xx2, yy1)
          tw2 = t2 * wij
          twi2 = abs (tw2)
          twj2(j) = twi2
          if (twi2 < epsilon1) then
            iz2 = iz2 + 1
            jz1(j) = jz1(j) + 1
          else
            iz2 = 0
            jz2(j) = 0
          endif
        else
          t2 = 0.d0
          tw2 = 0.d0
        endif

        if (j > 0 .and. logx1 .and. logy2 .and. iz3 < izx .and. jz3(j) < izx) &
          then
          t3 = fun (xx1, yy2)
          tw3 = t3 * wij
          twi3 = abs (tw3)
          twj3(j) = twi3
          if (twi3 < epsilon1) then
            iz3 = iz3 + 1
            jz3(j) = jz3(j) + 1
          else
            iz3 = 0
            jz3(j) = 0
          endif
        else
          t3 = 0.d0
          tw3 = 0.d0
        endif

        if (i > 0 .and. j > 0 .and. logx2 .and. logy2 .and. iz4 < izx &
          .and. jz4(j) < izx) then
          t4 = fun (xx2, yy2)
          tw4 = t4 * wij
          twi4 = abs (tw4)
          twj4(j) = twi4
          if (twi4 < epsilon1) then
            iz4 = iz4 + 1
            jz4(j) = jz4(j) + 1
          else
            iz4 = 0
            jz4(j) = 0
          endif
        else
          t4 = 0.d0
          tw4 = 0.d0
        endif

        tsum = tsum + tw1 + tw2 + tw3 + tw4
        twmx = max (twmx, abs (tw1), abs (tw2))
        twmx = max (twmx, abs (tw3), abs (tw4))
      endif
    enddo

    twimx = max (twimx, twi1, twi2)
    twimx = max (twimx, twi3, twi4)
  enddo

  do j = 0, n
    twjmx = max (twjmx, twj1(j), twj2(j))
    twjmx = max (twjmx, twj3(j), twj4(j))
  enddo

!   Compute s1 = current integral approximation and err = error estimate.

  s1 =  ax * ay * h**2 * tsum
  eps1 = twmx * epsilon1
  eps2 = max (twimx, twjmx)
  d1 = dplog10q (abs (s1 - s2))
  d2 = dplog10q (abs (s1 - s3))
  d3 = dplog10q (eps1) - 1.d0
  d4 = dplog10q (eps2) - 1.d0

  if (k <= 2) then
    err = 1.d0
  elseif (d1 .eq. -9999.d0) then
    err = 0.d0
  else
    err = c10 ** nint (min (0.d0, max (d1 ** 2 / d2, 2.d0 * d1, d3, d4)))
  endif

!   Output current integral approximation and error estimate, to 56 dp.

  if (ndebug >= 2) then
    write (6, 2) k, nq1, nint (dplog10q (abs (err)))
2   format ('quadtsq2d: Iteration',i3,' of',i3,'; est error = 10^',i5, &
      '; approx value =')
    call qdwrite (6, s1)
  endif
  if (k >= 3 .and. err < eps1) goto 140
!  if (k >= 3 .and. err < eps2) goto 120
enddo

write (6, 3) nint (dplog10q (abs (err))), nquadl
3 format ('quadtsq2d: Estimated error = 10^',i5/&
  'Increase Quadlevel for greater accuracy. Current Quadlevel =',i4)
goto 140

120 continue

write (6, 4) nint (dplog10q (abs (err))), ndigits
4 format ('quadtsq2d: Estimated error = 10^',i5/&
  'Increase working prec (Digits) for greater accuracy. Current Digits =',i4)
goto 140

130 continue

if (ierror > 0) nerror = ierror + 100
write (6, 5) nerror
5 format ('quadtsq2d: Error in quadrature calculation; code =',i5)
s1 = 0.d0

140 continue

quadtsq2d = s1
return
end

function catalan ()
use qdmodule
implicit none
integer k
real*8 dk, eps
type (qd_real) catalan, c1, c2, c4, c8, r16, t1, t2, t3
type (qd_real) x1, x2, x3, x4, x5, x6

c1 = 1.d0
c2 = 2.d0
c4 = 4.d0
c8 = 8.d0
r16 = 1.d0 / 16.d0
t1 = 0.d0
t2 = 1.d0
eps = 1.d-64

do k = 0, 10000000
  dk = k
  t3 = t2 * (c8 / (8.d0 * dk + 1.d0) ** 2 + c8 / (8.d0 * dk + 2.d0) ** 2 &
       + c4 / (8.d0 * dk + 3.d0) ** 2 - c2 / (8.d0 * dk + 5.d0) ** 2 &
       - c2 / (8.d0 * dk + 6.d0) ** 2 - c1 / (8.d0 * dk + 7.d0) ** 2)
  t1 = t1 + t3
  t2 = r16 * t2
  if (t3 < 1.d-5 * eps) goto 100
enddo

write (6, *) 'catalan: error - contact author'

100 continue

catalan = 1.d0 / 8.d0 * qdpi() * log (c2) + 1.d0 / 16.d0 * t1
return
end

function dplog10q (a)

!   For input MP value a, this routine returns a DP approximation to log10 (a).

use qdmodule
implicit none
integer ia
double precision da, dplog10q, t1
type (qd_real) a

! call mpmdc (a%mpr, da, ia)
da = a
ia = 0
if (da .eq. 0.d0) then
  dplog10q = -9999.d0
else
  dplog10q = log10 (abs (da)) + ia * log10 (2.d0)
endif

100 continue
return
end

subroutine decmdq (a, b, ib)

!   For input MP value a, this routine returns DP b and integer ib such that 
!   a = b * 10^ib, with 1 <= abs (b) < 10 for nonzero a.

use qdmodule
implicit none
integer ia, ib
double precision da, b, t1, xlt
parameter (xlt = 0.3010299956639812d0)
type (qd_real) a

! call mpmdc (a%mpr, da, ia)
da = a
ia = 0
if (da .ne. 0.d0) then
  t1 = xlt * ia + log10 (abs (da))
  ib = t1
  if (t1 .lt. 0.d0) ib = ib - 1
  b = sign (10.d0 ** (t1 - ib), da)
else
  b = 0.d0
  ib = 0
endif

return
end
