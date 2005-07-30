!  ddmod.f
!  
!  This work was supported by the Director, Office of Science, Division
!  of Mathematical, Information, and Computational Sciences of the
!  U.S. Department of Energy under contract number DE-AC03-76SF00098.
!
!  Copyright (c) 2000-2005
!
!  Fortran-90 module file to use with double-double numbers.
!
!  Yozo Hida
!  David H Bailey    2005-01-25

MODULE ddmodule
  IMPLICIT NONE
  
  TYPE dd_real
     SEQUENCE
     REAL*8 :: re(2)
  END TYPE dd_real

  real*8 ddeps
  parameter (ddeps = 1.23259516440783d-32)

  INTERFACE ASSIGNMENT (=)
     MODULE PROCEDURE assign_dd_str
     MODULE PROCEDURE assign_dd_d
     MODULE PROCEDURE assign_d_dd
  END INTERFACE

  INTERFACE OPERATOR (+)
     MODULE PROCEDURE add_dd
     MODULE PROCEDURE add_dd_d
     MODULE PROCEDURE add_d_dd
  END INTERFACE

  INTERFACE OPERATOR (-)
     MODULE PROCEDURE sub_dd
     MODULE PROCEDURE sub_dd_d
     MODULE PROCEDURE sub_d_dd
     MODULE PROCEDURE neg_dd
  END INTERFACE

  INTERFACE OPERATOR (*)
     MODULE PROCEDURE mul_dd
     MODULE PROCEDURE mul_dd_d
     MODULE PROCEDURE mul_d_dd
  END INTERFACE

  INTERFACE OPERATOR (/)
     MODULE PROCEDURE div_dd
     MODULE PROCEDURE div_dd_d
     MODULE PROCEDURE div_d_dd
  END INTERFACE

  INTERFACE OPERATOR (**)
     MODULE PROCEDURE pwr_dd
     MODULE PROCEDURE pwr_dd_i
     MODULE PROCEDURE pwr_d_dd
  END INTERFACE

  INTERFACE ddreal
     MODULE PROCEDURE to_dd_d
     MODULE PROCEDURE to_dd_str
  END INTERFACE

  INTERFACE REAL
     MODULE PROCEDURE to_real_dd
  END INTERFACE

  INTERFACE SIN
     MODULE PROCEDURE ddsin
  END INTERFACE
  INTERFACE COS
     MODULE PROCEDURE ddcos
  END INTERFACE
  INTERFACE TAN
     MODULE PROCEDURE ddtan
  END INTERFACE
  INTERFACE SINCOS
     MODULE PROCEDURE ddsincos
  END INTERFACE
  INTERFACE ddcssnf
     MODULE PROCEDURE ddcossin
  END INTERFACE

  INTERFACE ASIN
     MODULE PROCEDURE ddasin
  END INTERFACE
  INTERFACE ACOS
     MODULE PROCEDURE ddacos
  END INTERFACE
  INTERFACE ATAN
     MODULE PROCEDURE ddatan
  END INTERFACE
  INTERFACE ATAN2
     MODULE PROCEDURE ddatan2
  END INTERFACE

  INTERFACE EXP
     MODULE PROCEDURE ddexp
  END INTERFACE
  INTERFACE LOG
     MODULE PROCEDURE ddlog
  END INTERFACE

  INTERFACE SQRT
     MODULE PROCEDURE ddsqrt
  END INTERFACE
  INTERFACE SQR
     MODULE PROCEDURE ddsqr
  END INTERFACE
  INTERFACE NROOT
     MODULE PROCEDURE ddnroot
  END INTERFACE

  INTERFACE SINH
     MODULE PROCEDURE ddsinh
  END INTERFACE
  INTERFACE COSH
     MODULE PROCEDURE ddcosh
  END INTERFACE
  INTERFACE TANH
     MODULE PROCEDURE ddtanh
  END INTERFACE
  INTERFACE SINCOSH
     MODULE PROCEDURE ddsincosh
  END INTERFACE
  INTERFACE ddcsshf
     MODULE PROCEDURE ddcossinh
  END INTERFACE

  INTERFACE ASINH
     MODULE PROCEDURE ddasinh
  END INTERFACE
  INTERFACE ACOSH
     MODULE PROCEDURE ddacosh
  END INTERFACE
  INTERFACE ATANH
     MODULE PROCEDURE ddatanh
  END INTERFACE

  INTERFACE AINT
     MODULE PROCEDURE ddaint
  END INTERFACE

  INTERFACE ANINT
     MODULE PROCEDURE ddanint
  END INTERFACE

  INTERFACE ABS
     MODULE PROCEDURE ddabs
  END INTERFACE

  INTERFACE DDRAND
     MODULE PROCEDURE ddrand
  END INTERFACE

  INTERFACE OPERATOR (==)
     MODULE PROCEDURE eq_dd
     MODULE PROCEDURE eq_dd_d
     MODULE PROCEDURE eq_d_dd
  END INTERFACE

  INTERFACE OPERATOR (/=)
     MODULE PROCEDURE ne_dd
     MODULE PROCEDURE ne_dd_d
     MODULE PROCEDURE ne_d_dd
  END INTERFACE

  INTERFACE OPERATOR (>)
     MODULE PROCEDURE gt_dd
     MODULE PROCEDURE gt_dd_d
     MODULE PROCEDURE gt_d_dd
  END INTERFACE

  INTERFACE OPERATOR (<)
     MODULE PROCEDURE lt_dd
     MODULE PROCEDURE lt_dd_d
     MODULE PROCEDURE lt_d_dd
  END INTERFACE

  INTERFACE OPERATOR (>=)
     MODULE PROCEDURE ge_dd
     MODULE PROCEDURE ge_dd_d
     MODULE PROCEDURE ge_d_dd
  END INTERFACE

  INTERFACE OPERATOR (<=)
     MODULE PROCEDURE le_dd
     MODULE PROCEDURE le_dd_d
     MODULE PROCEDURE le_d_dd
  END INTERFACE

  INTERFACE ddread
     MODULE PROCEDURE ddinpq
  END INTERFACE

  INTERFACE ddwrite
     MODULE PROCEDURE ddoutq
  END INTERFACE

  INTERFACE dble
     MODULE PROCEDURE dd_to_d
  END INTERFACE

  INTERFACE min
     MODULE PROCEDURE ddmin
     MODULE PROCEDURE ddmin3
  END INTERFACE
  INTERFACE max
     MODULE PROCEDURE ddmax
     MODULE PROCEDURE ddmax3
  END INTERFACE

  INTERFACE ddpi
     MODULE PROCEDURE dd_pi
  END INTERFACE

CONTAINS

! Assignments
  SUBROUTINE assign_dd_str(a, s)
    TYPE (dd_real), INTENT(INOUT) :: a
    CHARACTER (LEN=*), INTENT(IN) :: s
    character*80 t
    t = s
    call ddinpc (t, a%re(1))
  END SUBROUTINE assign_dd_str

  SUBROUTINE assign_dd_d(a, d)
    TYPE (dd_real), INTENT(INOUT) :: a
    REAL*8, INTENT(IN) :: d
    a%re(1) = d
    a%re(2) = 0.0d0
  END SUBROUTINE assign_dd_d

  SUBROUTINE assign_d_dd(d, a)
    REAL*8, INTENT(INOUT) :: d
    TYPE (dd_real), INTENT(IN) :: a
    d = a%re(1)
  END SUBROUTINE assign_d_dd

! Conversions
  TYPE (dd_real) FUNCTION to_dd_d(a)
    REAL*8, INTENT(IN) :: a
    to_dd_d%re(1) = a
    to_dd_d%re(2) = 0.0d0
  END FUNCTION to_dd_d

  REAL*8 FUNCTION to_real_dd(a) 
    TYPE (dd_real), INTENT(IN) :: a
    to_real_dd = a%re(1)
  END FUNCTION to_real_dd

  TYPE (dd_real) FUNCTION to_dd_str(s)
    CHARACTER (LEN=*), INTENT(IN) :: s
    character*80 t
    t = s
    call ddinpc (t, to_dd_str%re(1))
  END FUNCTION to_dd_str

! Additions
  TYPE (dd_real) FUNCTION add_dd(a, b)
    TYPE (dd_real), INTENT(IN) :: a, b
    CALL f_dd_add(a, b, add_dd)
  END FUNCTION add_dd

  TYPE (dd_real) FUNCTION add_dd_d(a, b)
    TYPE (dd_real), INTENT(IN) :: a
    REAL*8, INTENT(IN) :: b
    CALL f_dd_add_dd_d(a, b, add_dd_d)
  END FUNCTION add_dd_d

  TYPE (dd_real) FUNCTION add_d_dd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (dd_real), INTENT(IN) :: b
    CALL f_dd_add_d_dd(a, b, add_d_dd)
  END FUNCTION add_d_dd

! Subtractions
  TYPE (dd_real) FUNCTION sub_dd(a, b)
    TYPE (dd_real), INTENT(IN) :: a, b
    CALL f_dd_sub(a, b, sub_dd)
  END FUNCTION sub_dd

  TYPE (dd_real) FUNCTION sub_dd_d(a, b)
    TYPE (dd_real), INTENT(IN) :: a
    REAL*8, INTENT(IN) :: b
    CALL f_dd_sub_dd_d(a, b, sub_dd_d)
  END FUNCTION sub_dd_d

  TYPE (dd_real) FUNCTION sub_d_dd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (dd_real), INTENT(IN) :: b
    CALL f_dd_sub_d_dd(a, b, sub_d_dd)
  END FUNCTION sub_d_dd

! Unary Minus
  TYPE (dd_real) FUNCTION neg_dd(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_neg(a, neg_dd)
  END FUNCTION neg_dd


! Multiplications
  TYPE (dd_real) FUNCTION mul_dd(a, b)
    TYPE (dd_real), INTENT(IN) :: a, b
    CALL f_dd_mul(a, b, mul_dd)
  END FUNCTION mul_dd

  TYPE (dd_real) FUNCTION mul_dd_d(a, b)
    TYPE (dd_real), INTENT(IN) :: a
    REAL*8, INTENT(IN) :: b
    CALL f_dd_mul_dd_d(a, b, mul_dd_d)
  END FUNCTION mul_dd_d

  TYPE (dd_real) FUNCTION mul_d_dd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (dd_real), INTENT(IN) :: b
    CALL f_dd_mul_d_dd(a, b, mul_d_dd)
  END FUNCTION mul_d_dd


! Divisions
  TYPE (dd_real) FUNCTION div_dd(a, b)
    TYPE (dd_real), INTENT(IN) :: a, b
    CALL f_dd_div(a, b, div_dd)
  END FUNCTION div_dd

  TYPE (dd_real) FUNCTION div_dd_d(a, b)
    TYPE (dd_real), INTENT(IN) :: a
    REAL*8, INTENT(IN) :: b
    CALL f_dd_div_dd_d(a, b, div_dd_d)
  END FUNCTION div_dd_d

  TYPE (dd_real) FUNCTION div_d_dd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (dd_real), INTENT(IN) :: b
    CALL f_dd_div_d_dd(a, b, div_d_dd)
  END FUNCTION div_d_dd


! Power
  TYPE (dd_real) FUNCTION pwr_dd (a, b)
    TYPE (dd_real), INTENT(IN) :: a, b
    TYPE (dd_real) q1, q2
    CALL f_dd_log(a, q1)
    CALL f_dd_mul(q1, b, q2)
    CALL f_dd_exp(q2, pwr_dd)
  END FUNCTION pwr_dd

  TYPE (dd_real) FUNCTION pwr_dd_i(a, n)
    TYPE (dd_real), INTENT(IN) :: a
    INTEGER, INTENT(IN) :: n
    CALL f_dd_npwr(a, n, pwr_dd_i)
  END FUNCTION pwr_dd_i

  TYPE (dd_real) FUNCTION pwr_d_dd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (dd_real), INTENT(IN) :: b
    TYPE (dd_real) q1, q2, q3
    q1%re(1) = a
    q1%re(2) = 0.d0
    CALL f_dd_log(q1, q2)
    CALL f_dd_mul(q2, b, q3)
    CALL f_dd_exp(q3, pwr_d_dd)
  END FUNCTION pwr_d_dd

! Trigonometric Functions
  TYPE (dd_real) FUNCTION ddsin(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_sin(a, ddsin)
  END FUNCTION ddsin

  TYPE (dd_real) FUNCTION ddcos(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_cos(a, ddcos)
  END FUNCTION ddcos

  TYPE (dd_real) FUNCTION ddtan(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_tan(a, ddtan)
  END FUNCTION ddtan

  SUBROUTINE ddsincos(a, s, c)
    TYPE (dd_real), INTENT(IN) :: a
    TYPE (dd_real), INTENT(OUT) :: s, c
    CALL f_dd_sincos(a, s, c)
  END SUBROUTINE ddsincos

  SUBROUTINE ddcossin(a, c, s)
    TYPE (dd_real), INTENT(IN) :: a
    TYPE (dd_real), INTENT(OUT) :: s, c
    CALL f_dd_sincos(a, s, c)
  END SUBROUTINE ddcossin


! Inverse Trigonometric Functions
  TYPE (dd_real) FUNCTION ddasin(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_asin(a, ddasin)
  END FUNCTION ddasin

  TYPE (dd_real) FUNCTION ddacos(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_acos(a, ddacos)
  END FUNCTION ddacos

  TYPE (dd_real) FUNCTION ddatan(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_atan(a, ddatan)
  END FUNCTION ddatan

  TYPE (dd_real) FUNCTION ddatan2(a, b)
    TYPE (dd_real), INTENT(IN) :: a, b
    CALL f_dd_atan2(a, b, ddatan2)
  END FUNCTION ddatan2

! Exponential and Logarithms
  TYPE (dd_real) FUNCTION ddexp(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_exp(a, ddexp)
  END FUNCTION ddexp

  TYPE (dd_real) FUNCTION ddlog(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_log(a, ddlog)
  END FUNCTION ddlog

  TYPE (dd_real) FUNCTION ddlog10(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_log10(a, ddlog10)
  END FUNCTION ddlog10


! SQRT, etc.
  TYPE (dd_real) FUNCTION ddsqrt(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_sqrt(a, ddsqrt)
  END FUNCTION ddsqrt

  TYPE (dd_real) FUNCTION ddsqr(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_sqr(a, ddsqr)
  END FUNCTION ddsqr

  TYPE (dd_real) FUNCTION ddnroot(a, n)
    TYPE (dd_real), INTENT(IN) :: a
    INTEGER, INTENT(IN) :: n
    CALL f_dd_nroot(a, n, ddnroot)
  END FUNCTION ddnroot


! Hyperbolic Functions
  TYPE (dd_real) FUNCTION ddsinh(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_sinh(a, ddsinh)
  END FUNCTION ddsinh

  TYPE (dd_real) FUNCTION ddcosh(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_cosh(a, ddcosh)
  END FUNCTION ddcosh

  TYPE (dd_real) FUNCTION ddtanh(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_tanh(a, ddtanh)
  END FUNCTION ddtanh

  SUBROUTINE ddsincosh(a, s, c)
    TYPE (dd_real), INTENT(IN) :: a
    TYPE (dd_real), INTENT(OUT) :: s, c
    CALL f_dd_sincosh(a, s, c)
  END SUBROUTINE ddsincosh

  SUBROUTINE ddcossinh(a, c, s)
    TYPE (dd_real), INTENT(IN) :: a
    TYPE (dd_real), INTENT(OUT) :: s, c
    CALL f_dd_sincosh(a, s, c)
  END SUBROUTINE ddcossinh

! Inverse Hyperbolic Functions
  TYPE (dd_real) FUNCTION ddasinh(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_asinh(a, ddasinh)
  END FUNCTION ddasinh

  TYPE (dd_real) FUNCTION ddacosh(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_acosh(a, ddacosh)
  END FUNCTION ddacosh

  TYPE (dd_real) FUNCTION ddatanh(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_atanh(a, ddatanh)
  END FUNCTION ddatanh


! Rounding
  TYPE (dd_real) FUNCTION ddaint(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_aint(a, ddaint)
  END FUNCTION ddaint

  TYPE (dd_real) FUNCTION ddanint(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_nint(a, ddanint)
  END FUNCTION ddanint


! Random Number Generator
  TYPE (dd_real) FUNCTION ddrand()
    CALL f_dd_rand(ddrand)
  END FUNCTION ddrand


! Equality
  LOGICAL FUNCTION eq_dd(a, b)
    TYPE (dd_real), INTENT(IN) :: a, b
    INTEGER :: r
    CALL f_dd_comp(a, b, r)
    IF (r == 0) THEN
       eq_dd = .TRUE.
    ELSE
       eq_dd = .FALSE.
    END IF
  END FUNCTION eq_dd

  LOGICAL FUNCTION eq_dd_d(a, b)
    TYPE (dd_real), INTENT(IN) :: a
    REAL*8, INTENT(IN) :: b
    INTEGER :: r
    CALL f_dd_comp_dd_d(a, b, r)
    IF (r == 0) THEN
       eq_dd_d = .TRUE.
    ELSE
       eq_dd_d = .FALSE.
    END IF
  END FUNCTION eq_dd_d

  LOGICAL FUNCTION eq_d_dd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (dd_real), INTENT(IN) :: b
    INTEGER :: r
    CALL f_dd_comp_d_dd(a, b, r)
    IF (r == 0) THEN
       eq_d_dd = .TRUE.
    ELSE
       eq_d_dd = .FALSE.
    END IF
  END FUNCTION eq_d_dd


! Non-Equality
  LOGICAL FUNCTION ne_dd(a, b)
    TYPE (dd_real), INTENT(IN) :: a, b
    INTEGER :: r
    CALL f_dd_comp(a, b, r)
    IF (r == 0) THEN
       ne_dd = .FALSE.
    ELSE
       ne_dd = .TRUE.
    END IF
  END FUNCTION ne_dd

  LOGICAL FUNCTION ne_dd_d(a, b)
    TYPE (dd_real), INTENT(IN) :: a
    REAL*8, INTENT(IN) :: b
    INTEGER :: r
    CALL f_dd_comp_dd_d(a, b, r)
    IF (r == 0) THEN
       ne_dd_d = .FALSE.
    ELSE
       ne_dd_d = .TRUE.
    END IF
  END FUNCTION ne_dd_d

  LOGICAL FUNCTION ne_d_dd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (dd_real), INTENT(IN) :: b
    INTEGER :: r
    CALL f_dd_comp_d_dd(a, b, r)
    IF (r == 0) THEN
       ne_d_dd = .FALSE.
    ELSE
       ne_d_dd = .TRUE.
    END IF
  END FUNCTION ne_d_dd

! Greater-Than
  LOGICAL FUNCTION gt_dd(a, b)
    TYPE (dd_real), INTENT(IN) :: a, b
    INTEGER :: r
    CALL f_dd_comp(a, b, r)
    IF (r == 1) THEN
       gt_dd = .TRUE.
    ELSE
       gt_dd = .FALSE.
    END IF
  END FUNCTION gt_dd

  LOGICAL FUNCTION gt_dd_d(a, b)
    TYPE (dd_real), INTENT(IN) :: a
    REAL*8, INTENT(IN) :: b
    INTEGER :: r
    CALL f_dd_comp_dd_d(a, b, r)
    IF (r == 1) THEN
       gt_dd_d = .TRUE.
    ELSE
       gt_dd_d = .FALSE.
    END IF
  END FUNCTION gt_dd_d

  LOGICAL FUNCTION gt_d_dd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (dd_real), INTENT(IN) :: b
    INTEGER :: r
    CALL f_dd_comp_d_dd(a, b, r)
    IF (r == 1) THEN
       gt_d_dd = .TRUE.
    ELSE
       gt_d_dd = .FALSE.
    END IF
  END FUNCTION gt_d_dd


! Less-Than
  LOGICAL FUNCTION lt_dd(a, b)
    TYPE (dd_real), INTENT(IN) :: a, b
    INTEGER :: r
    CALL f_dd_comp(a, b, r)
    IF (r == -1) THEN
       lt_dd = .TRUE.
    ELSE
       lt_dd = .FALSE.
    END IF
  END FUNCTION lt_dd

  LOGICAL FUNCTION lt_dd_d(a, b)
    TYPE (dd_real), INTENT(IN) :: a
    REAL*8, INTENT(IN) :: b
    INTEGER :: r
    CALL f_dd_comp_dd_d(a, b, r)
    IF (r == -1) THEN
       lt_dd_d = .TRUE.
    ELSE
       lt_dd_d = .FALSE.
    END IF
  END FUNCTION lt_dd_d

  LOGICAL FUNCTION lt_d_dd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (dd_real), INTENT(IN) :: b
    INTEGER :: r
    CALL f_dd_comp_d_dd(a, b, r)
    IF (r == -1) THEN
       lt_d_dd = .TRUE.
    ELSE
       lt_d_dd = .FALSE.
    END IF
  END FUNCTION lt_d_dd

! Greater-Than-Or-Equal-To
  LOGICAL FUNCTION ge_dd(a, b)
    TYPE (dd_real), INTENT(IN) :: a, b
    INTEGER :: r
    CALL f_dd_comp(a, b, r)
    IF (r >= 0) THEN
       ge_dd = .TRUE.
    ELSE
       ge_dd = .FALSE.
    END IF
  END FUNCTION ge_dd

  LOGICAL FUNCTION ge_dd_d(a, b)
    TYPE (dd_real), INTENT(IN) :: a
    REAL*8, INTENT(IN) :: b
    INTEGER :: r
    CALL f_dd_comp_dd_d(a, b, r)
    IF (r >= 0) THEN
       ge_dd_d = .TRUE.
    ELSE
       ge_dd_d = .FALSE.
    END IF
  END FUNCTION ge_dd_d

  LOGICAL FUNCTION ge_d_dd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (dd_real), INTENT(IN) :: b
    INTEGER :: r
    CALL f_dd_comp_d_dd(a, b, r)
    IF (r >= 0) THEN
       ge_d_dd = .TRUE.
    ELSE
       ge_d_dd = .FALSE.
    END IF
  END FUNCTION ge_d_dd

! Less-Than-Or-Equal-To
  LOGICAL FUNCTION le_dd(a, b)
    TYPE (dd_real), INTENT(IN) :: a, b
    INTEGER :: r
    CALL f_dd_comp(a, b, r)
    IF (r <= 0) THEN
       le_dd = .TRUE.
    ELSE
       le_dd = .FALSE.
    END IF
  END FUNCTION le_dd

  LOGICAL FUNCTION le_dd_d(a, b)
    TYPE (dd_real), INTENT(IN) :: a
    REAL*8, INTENT(IN) :: b
    INTEGER :: r
    CALL f_dd_comp_dd_d(a, b, r)
    IF (r <= 0) THEN
       le_dd_d = .TRUE.
    ELSE
       le_dd_d = .FALSE.
    END IF
  END FUNCTION le_dd_d

  LOGICAL FUNCTION le_d_dd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (dd_real), INTENT(IN) :: b
    INTEGER :: r
    CALL f_dd_comp_d_dd(a, b, r)
    IF (r <= 0) THEN
       le_d_dd = .TRUE.
    ELSE
       le_d_dd = .FALSE.
    END IF
  END FUNCTION le_d_dd


! Absolute Value
  TYPE (dd_real) FUNCTION ddabs(a)
    TYPE (dd_real), INTENT(IN) :: a
    CALL f_dd_abs(a, ddabs)
  END FUNCTION ddabs

! Input
  SUBROUTINE ddinpq(u, q1, q2, q3, q4, q5, q6, q7, q8, q9)
    INTEGER, INTENT(IN) :: u
    TYPE (dd_real), INTENT(IN) :: q1
    TYPE (dd_real), INTENT(IN), OPTIONAL :: q2, q3, q4, q5, q6, q7, q8, q9
!    CHARACTER (LEN=72) :: str

    call ddinp (u, q1%re(1))

    IF (PRESENT(q2)) THEN
      call ddinp (u, q2%re(1))
    END IF

    IF (PRESENT(q3)) THEN
      call ddinp (u, q3%re(1))
    END IF

    IF (PRESENT(q4)) THEN
      call ddinp (u, q4%re(1))
    END IF

    IF (PRESENT(q5)) THEN
      call ddinp (u, q5%re(1))
    END IF

    IF (PRESENT(q6)) THEN
      call ddinp (u, q6%re(1))
    END IF

    IF (PRESENT(q7)) THEN
      call ddinp (u, q7%re(1))
    END IF

    IF (PRESENT(q8)) THEN
      call ddinp (u, q8%re(1))
    END IF

    IF (PRESENT(q9)) THEN
      call ddinp (u, q9%re(1))
   END IF

  END SUBROUTINE ddinpq

! Output
  SUBROUTINE ddoutq(u, q1, q2, q3, q4, q5, q6, q7, q8, q9)
    INTEGER, INTENT(IN) :: u
    TYPE (dd_real), INTENT(IN) :: q1
    TYPE (dd_real), INTENT(IN), OPTIONAL :: q2, q3, q4, q5, q6, q7, q8, q9
!    CHARACTER (LEN=72) :: str

    call ddout (u, q1%re(1))

    IF (PRESENT(q2)) THEN
      call ddout (u, q2%re(1))
    END IF

    IF (PRESENT(q3)) THEN
      call ddout (u, q3%re(1))
    END IF

    IF (PRESENT(q4)) THEN
      call ddout (u, q4%re(1))
    END IF

    IF (PRESENT(q5)) THEN
      call ddout (u, q5%re(1))
    END IF

    IF (PRESENT(q6)) THEN
      call ddout (u, q6%re(1))
    END IF

    IF (PRESENT(q7)) THEN
      call ddout (u, q7%re(1))
    END IF

    IF (PRESENT(q8)) THEN
      call ddout (u, q8%re(1))
    END IF

    IF (PRESENT(q9)) THEN
      call ddout (u, q9%re(1))
   END IF

  END SUBROUTINE ddoutq

  REAL*8 FUNCTION dd_to_d(a)
    TYPE (dd_real), INTENT(IN) :: a
    dd_to_d = a%re(1)
  END FUNCTION dd_to_d

  TYPE (dd_real) FUNCTION ddmin(a, b)
    TYPE (dd_real), INTENT(IN) :: a, b
    INTEGER :: r
    CALL f_dd_comp(a, b, r)
    IF (r == 1) THEN
       ddmin = b
    ELSE
       ddmin = a
    END IF
  END FUNCTION ddmin

  TYPE (dd_real) FUNCTION ddmin3(a, b, c)
    TYPE (dd_real), INTENT(IN) :: a, b, c
    INTEGER r
    CALL f_dd_comp(a, b, r)
    IF (r == 1) THEN
       ! b < a.  
       CALL f_dd_comp(b, c, r)
       IF (r == 1) THEN
          ddmin3 = c
       ELSE
          ddmin3 = b
       END IF
    ELSE
       ! b > a
       CALL f_dd_comp(a, c, r)
       IF (r == 1) THEN 
          ddmin3 = c
       ELSE
          ddmin3 = a
       END IF
    END IF
  END FUNCTION ddmin3

  TYPE (dd_real) FUNCTION ddmax(a, b)
    TYPE (dd_real), INTENT(IN) :: a, b
    INTEGER :: r
    CALL f_dd_comp(a, b, r)
    IF (r == -1) THEN
       ddmax = b
    ELSE
       ddmax = a
    END IF
  END FUNCTION ddmax

  TYPE (dd_real) FUNCTION ddmax3(a, b, c)
    TYPE (dd_real), INTENT(IN) :: a, b, c
    INTEGER r
    CALL f_dd_comp(a, b, r)
    IF (r == -1) THEN
       ! b > a.  
       CALL f_dd_comp(b, c, r)
       IF (r == -1) THEN
          ddmax3 = c
       ELSE
          ddmax3 = b
       END IF
    ELSE
       ! b < a
       CALL f_dd_comp(a, c, r)
       IF (r == -1) THEN 
          ddmax3 = c
       ELSE
          ddmax3 = a
       END IF
    END IF
  END FUNCTION ddmax3

  TYPE (dd_real) FUNCTION dd_pi()
    CALL f_dd_pi(dd_pi)
  END FUNCTION dd_pi

subroutine ddinp (iu, a)

!   This routine reads the DD number A from logical unit IU.  The input
!   value must be placed on a single line of not more than 80 characters.

implicit none
integer iu, ln
parameter (ln = 80)
character*80 cs
real*8 a(2)

read (iu, '(a)', end = 100) cs
call ddinpc (cs, a)
goto 110

100 write (6, 1)
1  format ('*** DDINP: End-of-file encountered.')
! call ddabrt
stop

110 return
end subroutine

subroutine ddinpc (a, b)

!   Converts the CHARACTER*80 array A into the DD number B.

implicit none
integer i, ib, id, ie, inz, ip, is, ix, k, ln, lnn
parameter (ln = 80)
real*8 bi
character*80 a
character*1 ai
character*10 dig
character*16 ca
parameter (dig = '0123456789')
real*8 b(2), f(2), s0(2), s1(2), s2(2)

id = 0
ip = -1
is = 0
inz = 0
s1(1) = 0.d0
s1(2) = 0.d0

do i = 80, 1, -1
  if (a(i:i) /= ' ') goto 90
enddo

90 continue

lnn = i

!   Scan for digits, looking for the period also.

do i = 1, lnn
  ai = a(i:i)
  if (ai .eq. ' ' .and. id == 0) then
  elseif (ai .eq. '.') then
    if (ip >= 0) goto 210
    ip = id
    inz = 1
  elseif (ai .eq. '+') then
    if (id .ne. 0 .or. ip >= 0 .or. is .ne. 0) goto 210
    is = 1
  elseif (ai .eq. '-') then
    if (id .ne. 0 .or. ip >= 0 .or. is .ne. 0) goto 210
    is = -1
  elseif (ai .eq. 'e' .or. ai .eq. 'E' .or. ai .eq. 'd' .or. ai .eq. 'D') then
    goto 100
  elseif (index (dig, ai) .eq. 0) then
    goto 210
  else
!    read (ai, '(f1.0)') bi
    bi = index (dig, ai) - 1
    if (inz > 0 .or. bi > 0.d0) then
      inz = 1
      id = id + 1
!    call ddmuld (s1, 10.d0, s0)
      call f_dd_mul_dd_d (s1, 10.d0, s0)
      f(1) = bi
      f(2) = 0.d0
!    call dddqc (bi, f)
!    call ddadd (s0, f, s1)
      call f_dd_add (s0, f, s1)
    endif
  endif
enddo

100   continue
if (is .eq. -1) then
  s1(1) = - s1(1)
  s1(2) = - s1(2)
endif
k = i
if (ip == -1) ip = id
ie = 0
is = 0
ca = ' '

do i = k + 1, lnn
  ai = a(i:i)
  if (ai .eq. ' ') then
  elseif (ai .eq. '+') then
    if (ie .ne. 0 .or. is .ne. 0) goto 210
    is = 1
  elseif (ai .eq. '-') then
    if (ie .ne. 0 .or. is .ne. 0) goto 210
    is = -1
  elseif (index (dig, ai) .eq. 0) then
    goto 210
  else
    ie = ie + 1
    if (ie .gt. 3) goto 210
    ca(ie:ie) = ai
  endif
enddo

! read (ca, '(i4)') ie
ie = dddigin (ca, 4)
if (is .eq. -1) ie = - ie
ie = ie + ip - id
s0(1) = 10.d0
s0(2) = 0.d0
! call ddnpwr (s0, ie, s2)
call f_dd_npwr (s0, ie, s2)
! call ddmul (s1, s2, b)
call f_dd_mul (s1, s2, b)
goto 220

210  write (6, 1)
1 format ('*** DDINPC: Syntax error in literal string.')
! call ddabrt
stop

220  return
end subroutine

subroutine ddout (iu, a)

!   This routine writes the DD number A on logical unit iu using a standard
!   E format, with lines 40 characters long.

implicit none
integer i, iu, ln
parameter (ln = 40)
character*40 cs
real*8 a(2)

call ddoutc (a, cs)
write (iu, '(a)') cs

return
end subroutine

subroutine ddoutc (a, b)

!   Converts the DD number A into character form in the CHARACTER*1 array B
!   of length 40.  In other words, B is contained in B(1), ..., B(40).  
!   The format is analogous to the Fortran E format.

!   This routine is called by DDOUT, but it may be directly called by the user
!   if desired for custom output.

implicit none
integer i, ii, ix, ln, nx
parameter (ln = 40)
integer ib(ln)
real*8 t1
character*40 b
character*10 digits
character*16 ca
parameter (digits = '0123456789')
real*8 a(2), f(2), s0(2), s1(2)

f(1) = 10.d0
f(2) = 0.d0

do i = 1, ln
  ib(i) = 0
enddo

!   Determine exact power of ten for exponent.

if (a(1) .ne. 0.d0) then
  t1 = log10 (abs (a(1)))
  if (t1 .ge. 0.d0) then
    nx = t1
  else
    nx = t1 - 1.d0
  endif
!  call ddnpwr (f, nx, s0)
  call f_dd_npwr (f, nx, s0)

!  call dddiv (a, s0, s1)
  call f_dd_div (a, s0, s1)
  if (s1(1) .lt. 0.d0) then
    s1(1) = - s1(1)
    s1(2) = - s1(2)
  endif

!   If we didn't quite get it exactly right, multiply or divide by 10 to fix.

  i = 0

100 continue

  i = i + 1
  if (s1(1) .lt. 1.d0) then
    nx = nx - 1
!    call ddmuld (s1, 10.d0, s0)
    call f_dd_mul_dd_d (s1, 10.d0, s0)
    s1(1) = s0(1)
    s1(2) = s0(2)
    if (i <= 3) goto 100
  elseif (s1(1) .ge. 10.d0) then
    nx = nx + 1
!    call dddivd (s1, 10.d0, s0)
    call f_dd_div_dd_d (s1, 10.d0, s0)
    s1(1) = s0(1)
    s1(2) = s0(2)
    goto 100
  endif
else
  nx = 0
  s1(1) = 0.d0
  s1(2) = 0.d0
endif

!   Compute digits.

do i = 1, ln - 8
  ii = s1(1)
  ib(i) = ii
  f(1) = ii
!  call ddsub (s1, f, s0)
  call f_dd_sub (s1, f, s0)
!  call ddmuld (s0, 10.d0, s1)
  call f_dd_mul_dd_d (s0, 10.d0, s1)
enddo

!   Fix negative digits.

do i = ln - 8, 2, -1
  if (ib(i) .lt. 0) then
    ib(i) = ib(i) + 10
    ib(i-1) = ib(i-1) - 1
  endif
enddo

if (ib(1) .lt. 0) then
  write (6, 1) 
1 format ('ddoutc: negative leading digit')
!  call ddabrt
  stop
endif

!   Round.

if (ib(ln-8) .ge. 5) then
  ib(ln-9) = ib(ln-9) + 1

  do i = ln - 9, 2, -1
    if (ib(i) .eq. 10) then
      ib(i) = 0
      ib(i-1) = ib(i-1) + 1
    endif
  enddo

  if (ib(1) .eq. 10) then
    ib(1) = 1
    nx = nx + 1
  endif
endif

!   Insert digit characters in ib.

b(1:1) = ' '
b(2:2) = ' '
if (a(1) .ge. 0.d0) then
  b(3:3) = ' '
else
  b(3:3) = '-'
endif
ii = ib(1)
b(4:4) = digits(ii+1:ii+1)
b(5:5) = '.'
b(ln:ln) = ' '

do i = 2, ln - 9
  ii = ib(i)  
  b(i+4:i+4) = digits(ii+1:ii+1)
enddo

!   Insert exponent.

190  continue
! write (ca, '(i4)') nx
ca = dddigout (dble (nx), 4)
b(ln-4:ln-4) = 'E'
ii = 0

do i = 1, 4
  if (ca(i:i) /= ' ') then
    ii = ii + 1
    b(ln-4+ii:ln-4+ii) = ca(i:i)
  endif
enddo

do i = ii + 1, 4
  b(ln-4+i:ln-4+i) = ' '
enddo

return
end subroutine

  real*8 function dddigin (ca, n)
    implicit none
    real*8 d1
    character*(*), ca
    character*16 digits
    integer i, k, n
    parameter (digits = '0123456789')

    d1 = 0.d0

    do i = 1, n
      k = index (digits, ca(i:i)) - 1
      if (k < 0) then
        write (6, *) 'dddigin: non-digit in character string'
      elseif (k <= 9) then
        d1 = 10.d0 * d1 + k
      endif
    enddo

    dddigin = d1
  end function

  character*16 function dddigout (a, n)
    implicit none
    real*8 a, d1, d2
    character*16 ca, digits
    parameter (digits = '0123456789')
    integer i, is, k, n
    real*8 abs, aint

    ca = ' '
    is = sign (1.d0, a)
    d1 = abs (a)

    do i = n, 1, -1
      d2 = aint (d1 / 10.d0)
      k = 1.d0 + (d1 - 10.d0 * d2)
      d1 = d2
      ca(i:i) = digits(k:k)
      if (d1 == 0.d0) goto 100
    enddo

    i = 0

100 continue

    if (is < 0 .and. i > 1) then
      ca(i-1:i-1) = '-'
    elseif (i == 0 .or. is < 0 .and. i == 1) then
      ca = '****************'
    endif

    dddigout = ca
    return
  end function

END MODULE ddmodule


