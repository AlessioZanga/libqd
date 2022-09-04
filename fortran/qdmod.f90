!  qdmod.f
!  
!  This work was supported by the Director, Office of Science, Division
!  of Mathematical, Information, and Computational Sciences of the
!  U.S. Department of Energy under contract number DE-AC03-76SF00098.
!
!  Copyright (c) 2000-2005
!
!  Fortran-90 module file to use with quad-double numbers.
!
!  Yozo Hida
!  David H Bailey    2005-01-25

MODULE qdmodule
use ddmodule
  IMPLICIT NONE
  
  TYPE qd_real
     SEQUENCE
     REAL*8 :: re(4)
  END TYPE qd_real

  real*8 qdeps
  parameter (qdeps = 1.51929083932157d-64)

  INTERFACE ASSIGNMENT (=)
     MODULE PROCEDURE assign_qd_str
     MODULE PROCEDURE assign_qd_d
     MODULE PROCEDURE assign_d_qd
     module procedure assign_dd_qd
     module procedure assign_qd_dd
  END INTERFACE

  INTERFACE OPERATOR (+)
     MODULE PROCEDURE add_qd
     MODULE PROCEDURE add_qd_d
     MODULE PROCEDURE add_d_qd
  END INTERFACE

  INTERFACE OPERATOR (-)
     MODULE PROCEDURE sub_qd
     MODULE PROCEDURE sub_qd_d
     MODULE PROCEDURE sub_d_qd
     MODULE PROCEDURE neg_qd
  END INTERFACE

  INTERFACE OPERATOR (*)
     MODULE PROCEDURE mul_qd
     MODULE PROCEDURE mul_qd_d
     MODULE PROCEDURE mul_d_qd
  END INTERFACE

  INTERFACE OPERATOR (/)
     MODULE PROCEDURE div_qd
     MODULE PROCEDURE div_qd_d
     MODULE PROCEDURE div_d_qd
  END INTERFACE

  INTERFACE OPERATOR (**)
     MODULE PROCEDURE pwr_qd
     MODULE PROCEDURE pwr_qd_i
     MODULE PROCEDURE pwr_d_qd
  END INTERFACE

  INTERFACE qdreal
     MODULE PROCEDURE to_qd_d
     MODULE PROCEDURE to_qd_dd
     MODULE PROCEDURE to_qd_str
  END INTERFACE

  INTERFACE ddreal
     MODULE PROCEDURE to_dd_qd
  END INTERFACE

  INTERFACE REAL
     MODULE PROCEDURE to_real_qd
  END INTERFACE

  INTERFACE SIN
     MODULE PROCEDURE qdsin
  END INTERFACE
  INTERFACE COS
     MODULE PROCEDURE qdcos
  END INTERFACE
  INTERFACE TAN
     MODULE PROCEDURE qdtan
  END INTERFACE
  INTERFACE SINCOS
     MODULE PROCEDURE qdsincos
  END INTERFACE
  INTERFACE qdcssnf
     MODULE PROCEDURE qdcossin
  END INTERFACE

  INTERFACE ASIN
     MODULE PROCEDURE qdasin
  END INTERFACE
  INTERFACE ACOS
     MODULE PROCEDURE qdacos
  END INTERFACE
  INTERFACE ATAN
     MODULE PROCEDURE qdatan
  END INTERFACE
  INTERFACE ATAN2
     MODULE PROCEDURE qdatan2
  END INTERFACE

  INTERFACE EXP
     MODULE PROCEDURE qdexp
  END INTERFACE
  INTERFACE LOG
     MODULE PROCEDURE qdlog
  END INTERFACE

  INTERFACE SQRT
     MODULE PROCEDURE qdsqrt
  END INTERFACE
  INTERFACE SQR
     MODULE PROCEDURE qdsqr
  END INTERFACE
  INTERFACE NROOT
     MODULE PROCEDURE qdnroot
  END INTERFACE

  INTERFACE SINH
     MODULE PROCEDURE qdsinh
  END INTERFACE
  INTERFACE COSH
     MODULE PROCEDURE qdcosh
  END INTERFACE
  INTERFACE TANH
     MODULE PROCEDURE qdtanh
  END INTERFACE
  INTERFACE SINCOSH
     MODULE PROCEDURE qdsincosh
  END INTERFACE
  INTERFACE qdcsshf
     MODULE PROCEDURE qdcossinh
  END INTERFACE

  INTERFACE ASINH
     MODULE PROCEDURE qdasinh
  END INTERFACE
  INTERFACE ACOSH
     MODULE PROCEDURE qdacosh
  END INTERFACE
  INTERFACE ATANH
     MODULE PROCEDURE qdatanh
  END INTERFACE

  INTERFACE AINT
     MODULE PROCEDURE qdaint
  END INTERFACE

  INTERFACE ANINT
     MODULE PROCEDURE qdanint
  END INTERFACE

  INTERFACE ABS
     MODULE PROCEDURE qdabs
  END INTERFACE

  INTERFACE QDRAND
     MODULE PROCEDURE qdrand
  END INTERFACE

  INTERFACE OPERATOR (==)
     MODULE PROCEDURE eq_qd
     MODULE PROCEDURE eq_qd_d
     MODULE PROCEDURE eq_d_qd
  END INTERFACE

  INTERFACE OPERATOR (/=)
     MODULE PROCEDURE ne_qd
     MODULE PROCEDURE ne_qd_d
     MODULE PROCEDURE ne_d_qd
  END INTERFACE

  INTERFACE OPERATOR (>)
     MODULE PROCEDURE gt_qd
     MODULE PROCEDURE gt_qd_d
     MODULE PROCEDURE gt_d_qd
  END INTERFACE

  INTERFACE OPERATOR (<)
     MODULE PROCEDURE lt_qd
     MODULE PROCEDURE lt_qd_d
     MODULE PROCEDURE lt_d_qd
  END INTERFACE

  INTERFACE OPERATOR (>=)
     MODULE PROCEDURE ge_qd
     MODULE PROCEDURE ge_qd_d
     MODULE PROCEDURE ge_d_qd
  END INTERFACE

  INTERFACE OPERATOR (<=)
     MODULE PROCEDURE le_qd
     MODULE PROCEDURE le_qd_d
     MODULE PROCEDURE le_d_qd
  END INTERFACE

  INTERFACE qdread
     MODULE PROCEDURE qdinpq
  END INTERFACE

  INTERFACE qdwrite
     MODULE PROCEDURE qdoutq
  END INTERFACE

  INTERFACE dble
     MODULE PROCEDURE qd_to_d
  END INTERFACE

  INTERFACE min
     MODULE PROCEDURE qdmin
     MODULE PROCEDURE qdmin3
  END INTERFACE
  INTERFACE max
     MODULE PROCEDURE qdmax
     MODULE PROCEDURE qdmax3
  END INTERFACE

  INTERFACE qdpi
     MODULE PROCEDURE qd_pi
  END INTERFACE

CONTAINS

! Assignments
  SUBROUTINE assign_qd_str(a, s)
    TYPE (qd_real), INTENT(INOUT) :: a
    CHARACTER (LEN=*), INTENT(IN) :: s
    character*80 t
    t = s
    call qdinpc (t, a%re(1))
  END SUBROUTINE assign_qd_str

  SUBROUTINE assign_qd_d(a, d)
    TYPE (qd_real), INTENT(INOUT) :: a
    REAL*8, INTENT(IN) :: d
    a%re(1) = d
    a%re(2) = 0.0d0
    a%re(3) = 0.0d0
    a%re(4) = 0.0d0
  END SUBROUTINE assign_qd_d

  SUBROUTINE assign_d_qd(d, a)
    REAL*8, INTENT(INOUT) :: d
    TYPE (qd_real), INTENT(IN) :: a
    d = a%re(1)
  END SUBROUTINE assign_d_qd

  SUBROUTINE assign_dd_qd(dd, a)
    TYPE (dd_real), INTENT(INOUT) :: dd
    TYPE (qd_real), INTENT(IN) :: a
    dd%re(1) = a%re(1)
    dd%re(2) = a%re(2)
  END SUBROUTINE assign_dd_qd

  SUBROUTINE assign_qd_dd(a, dd)
    TYPE (qd_real), INTENT(INOUT) :: a
    TYPE (dd_real), INTENT(IN) :: dd
    a%re(1) = dd%re(1)
    a%re(2) = dd%re(2)
    a%re(3) = 0.d0
    a%re(4) = 0.d0
  END SUBROUTINE assign_qd_dd

! Conversions
  TYPE (qd_real) FUNCTION to_qd_d(a)
    REAL*8, INTENT(IN) :: a
    to_qd_d%re(1) = a
    to_qd_d%re(2) = 0.0d0
    to_qd_d%re(3) = 0.0d0
    to_qd_d%re(4) = 0.0d0
  END FUNCTION to_qd_d

  REAL*8 FUNCTION to_real_qd(a) 
    TYPE (qd_real), INTENT(IN) :: a
    to_real_qd = a%re(1)
  END FUNCTION to_real_qd

  TYPE (qd_real) FUNCTION to_qd_dd (dd)
     TYPE (dd_real), INTENT(IN) :: dd
     to_qd_dd%re(1) = dd%re(1)
     to_qd_dd%re(2) = dd%re(2)
     to_qd_dd%re(3) = 0.d0
     to_qd_dd%re(4) = 0.d0
  END FUNCTION to_qd_dd

  TYPE (dd_real) FUNCTION to_dd_qd (qd)
     TYPE (qd_real), INTENT(IN) :: qd
     to_dd_qd%re(1) = qd%re(1)
     to_dd_qd%re(2) = qd%re(2)
  END FUNCTION to_dd_qd

  TYPE (qd_real) FUNCTION to_qd_str(s)
    CHARACTER (LEN=*), INTENT(IN) :: s
    character*80 t
    t = s
    call qdinpc (t, to_qd_str%re(1))
  END FUNCTION to_qd_str

! Additions
  TYPE (qd_real) FUNCTION add_qd(a, b)
    TYPE (qd_real), INTENT(IN) :: a, b
    CALL f_qd_add(a, b, add_qd)
  END FUNCTION add_qd

  TYPE (qd_real) FUNCTION add_qd_d(a, b)
    TYPE (qd_real), INTENT(IN) :: a
    REAL*8, INTENT(IN) :: b
    CALL f_qd_add_qd_d(a, b, add_qd_d)
  END FUNCTION add_qd_d

  TYPE (qd_real) FUNCTION add_d_qd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (qd_real), INTENT(IN) :: b
    CALL f_qd_add_d_qd(a, b, add_d_qd)
  END FUNCTION add_d_qd

! Subtractions
  TYPE (qd_real) FUNCTION sub_qd(a, b)
    TYPE (qd_real), INTENT(IN) :: a, b
    CALL f_qd_sub(a, b, sub_qd)
  END FUNCTION sub_qd

  TYPE (qd_real) FUNCTION sub_qd_d(a, b)
    TYPE (qd_real), INTENT(IN) :: a
    REAL*8, INTENT(IN) :: b
    CALL f_qd_sub_qd_d(a, b, sub_qd_d)
  END FUNCTION sub_qd_d

  TYPE (qd_real) FUNCTION sub_d_qd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (qd_real), INTENT(IN) :: b
    CALL f_qd_sub_d_qd(a, b, sub_d_qd)
  END FUNCTION sub_d_qd

! Unary Minus
  TYPE (qd_real) FUNCTION neg_qd(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_neg(a, neg_qd)
  END FUNCTION neg_qd


! Multiplications
  TYPE (qd_real) FUNCTION mul_qd(a, b)
    TYPE (qd_real), INTENT(IN) :: a, b
    CALL f_qd_mul(a, b, mul_qd)
  END FUNCTION mul_qd

  TYPE (qd_real) FUNCTION mul_qd_d(a, b)
    TYPE (qd_real), INTENT(IN) :: a
    REAL*8, INTENT(IN) :: b
    CALL f_qd_mul_qd_d(a, b, mul_qd_d)
  END FUNCTION mul_qd_d

  TYPE (qd_real) FUNCTION mul_d_qd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (qd_real), INTENT(IN) :: b
    CALL f_qd_mul_d_qd(a, b, mul_d_qd)
  END FUNCTION mul_d_qd


! Divisions
  TYPE (qd_real) FUNCTION div_qd(a, b)
    TYPE (qd_real), INTENT(IN) :: a, b
    CALL f_qd_div(a, b, div_qd)
  END FUNCTION div_qd

  TYPE (qd_real) FUNCTION div_qd_d(a, b)
    TYPE (qd_real), INTENT(IN) :: a
    REAL*8, INTENT(IN) :: b
    CALL f_qd_div_qd_d(a, b, div_qd_d)
  END FUNCTION div_qd_d

  TYPE (qd_real) FUNCTION div_d_qd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (qd_real), INTENT(IN) :: b
    CALL f_qd_div_d_qd(a, b, div_d_qd)
  END FUNCTION div_d_qd


! Power
  TYPE (qd_real) FUNCTION pwr_qd (a, b)
    TYPE (qd_real), INTENT(IN) :: a, b
    TYPE (qd_real) q1, q2
    CALL f_qd_log(a, q1)
    CALL f_qd_mul(q1, b, q2)
    CALL f_qd_exp(q2, pwr_qd)
  END FUNCTION pwr_qd

  TYPE (qd_real) FUNCTION pwr_qd_i(a, n)
    TYPE (qd_real), INTENT(IN) :: a
    INTEGER, INTENT(IN) :: n
    CALL f_qd_npwr(a, n, pwr_qd_i)
  END FUNCTION pwr_qd_i

  TYPE (qd_real) FUNCTION pwr_d_qd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (qd_real), INTENT(IN) :: b
    TYPE (qd_real) q1, q2, q3
    q1%re(1) = a
    q1%re(2) = 0.d0
    q1%re(3) = 0.d0
    q1%re(4) = 0.d0
    CALL f_qd_log(q1, q2)
    CALL f_qd_mul(q2, b, q3)
    CALL f_qd_exp(q3, pwr_d_qd)
  END FUNCTION pwr_d_qd

! Trigonometric Functions
  TYPE (qd_real) FUNCTION qdsin(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_sin(a, qdsin)
  END FUNCTION qdsin

  TYPE (qd_real) FUNCTION qdcos(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_cos(a, qdcos)
  END FUNCTION qdcos

  TYPE (qd_real) FUNCTION qdtan(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_tan(a, qdtan)
  END FUNCTION qdtan

  SUBROUTINE qdsincos(a, s, c)
    TYPE (qd_real), INTENT(IN) :: a
    TYPE (qd_real), INTENT(OUT) :: s, c
    CALL f_qd_sincos(a, s, c)
  END SUBROUTINE qdsincos

  SUBROUTINE qdcossin(a, c, s)
    TYPE (qd_real), INTENT(IN) :: a
    TYPE (qd_real), INTENT(OUT) :: s, c
    CALL f_qd_sincos(a, s, c)
  END SUBROUTINE qdcossin


! Inverse Trigonometric Functions
  TYPE (qd_real) FUNCTION qdasin(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_asin(a, qdasin)
  END FUNCTION qdasin

  TYPE (qd_real) FUNCTION qdacos(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_acos(a, qdacos)
  END FUNCTION qdacos

  TYPE (qd_real) FUNCTION qdatan(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_atan(a, qdatan)
  END FUNCTION qdatan

  TYPE (qd_real) FUNCTION qdatan2(a, b)
    TYPE (qd_real), INTENT(IN) :: a, b
    CALL f_qd_atan2(a, b, qdatan2)
  END FUNCTION qdatan2

! Exponential and Logarithms
  TYPE (qd_real) FUNCTION qdexp(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_exp(a, qdexp)
  END FUNCTION qdexp

  TYPE (qd_real) FUNCTION qdlog(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_log(a, qdlog)
  END FUNCTION qdlog

  TYPE (qd_real) FUNCTION qdlog10(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_log10(a, qdlog10)
  END FUNCTION qdlog10


! SQRT, etc.
  TYPE (qd_real) FUNCTION qdsqrt(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_sqrt(a, qdsqrt)
  END FUNCTION qdsqrt

  TYPE (qd_real) FUNCTION qdsqr(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_sqr(a, qdsqr)
  END FUNCTION qdsqr

  TYPE (qd_real) FUNCTION qdnroot(a, n)
    TYPE (qd_real), INTENT(IN) :: a
    INTEGER, INTENT(IN) :: n
    CALL f_qd_nroot(a, n, qdnroot)
  END FUNCTION qdnroot


! Hyperbolic Functions
  TYPE (qd_real) FUNCTION qdsinh(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_sinh(a, qdsinh)
  END FUNCTION qdsinh

  TYPE (qd_real) FUNCTION qdcosh(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_cosh(a, qdcosh)
  END FUNCTION qdcosh

  TYPE (qd_real) FUNCTION qdtanh(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_tanh(a, qdtanh)
  END FUNCTION qdtanh

  SUBROUTINE qdsincosh(a, s, c)
    TYPE (qd_real), INTENT(IN) :: a
    TYPE (qd_real), INTENT(OUT) :: s, c
    CALL f_qd_sincosh(a, s, c)
  END SUBROUTINE qdsincosh

  SUBROUTINE qdcossinh(a, c, s)
    TYPE (qd_real), INTENT(IN) :: a
    TYPE (qd_real), INTENT(OUT) :: s, c
    CALL f_qd_sincosh(a, s, c)
  END SUBROUTINE qdcossinh

! Inverse Hyperbolic Functions
  TYPE (qd_real) FUNCTION qdasinh(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_asinh(a, qdasinh)
  END FUNCTION qdasinh

  TYPE (qd_real) FUNCTION qdacosh(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_acosh(a, qdacosh)
  END FUNCTION qdacosh

  TYPE (qd_real) FUNCTION qdatanh(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_atanh(a, qdatanh)
  END FUNCTION qdatanh


! Rounding
  TYPE (qd_real) FUNCTION qdaint(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_aint(a, qdaint)
  END FUNCTION qdaint

  TYPE (qd_real) FUNCTION qdanint(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_nint(a, qdanint)
  END FUNCTION qdanint


! Random Number Generator
  TYPE (qd_real) FUNCTION qdrand()
    CALL f_qd_rand(qdrand)
  END FUNCTION qdrand


! Equality
  LOGICAL FUNCTION eq_qd(a, b)
    TYPE (qd_real), INTENT(IN) :: a, b
    INTEGER :: r
    CALL f_qd_comp(a, b, r)
    IF (r == 0) THEN
       eq_qd = .TRUE.
    ELSE
       eq_qd = .FALSE.
    END IF
  END FUNCTION eq_qd

  LOGICAL FUNCTION eq_qd_d(a, b)
    TYPE (qd_real), INTENT(IN) :: a
    REAL*8, INTENT(IN) :: b
    INTEGER :: r
    CALL f_qd_comp_qd_d(a, b, r)
    IF (r == 0) THEN
       eq_qd_d = .TRUE.
    ELSE
       eq_qd_d = .FALSE.
    END IF
  END FUNCTION eq_qd_d

  LOGICAL FUNCTION eq_d_qd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (qd_real), INTENT(IN) :: b
    INTEGER :: r
    CALL f_qd_comp_d_qd(a, b, r)
    IF (r == 0) THEN
       eq_d_qd = .TRUE.
    ELSE
       eq_d_qd = .FALSE.
    END IF
  END FUNCTION eq_d_qd


! Non-Equality
  LOGICAL FUNCTION ne_qd(a, b)
    TYPE (qd_real), INTENT(IN) :: a, b
    INTEGER :: r
    CALL f_qd_comp(a, b, r)
    IF (r == 0) THEN
       ne_qd = .FALSE.
    ELSE
       ne_qd = .TRUE.
    END IF
  END FUNCTION ne_qd

  LOGICAL FUNCTION ne_qd_d(a, b)
    TYPE (qd_real), INTENT(IN) :: a
    REAL*8, INTENT(IN) :: b
    INTEGER :: r
    CALL f_qd_comp_qd_d(a, b, r)
    IF (r == 0) THEN
       ne_qd_d = .FALSE.
    ELSE
       ne_qd_d = .TRUE.
    END IF
  END FUNCTION ne_qd_d

  LOGICAL FUNCTION ne_d_qd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (qd_real), INTENT(IN) :: b
    INTEGER :: r
    CALL f_qd_comp_d_qd(a, b, r)
    IF (r == 0) THEN
       ne_d_qd = .FALSE.
    ELSE
       ne_d_qd = .TRUE.
    END IF
  END FUNCTION ne_d_qd

! Greater-Than
  LOGICAL FUNCTION gt_qd(a, b)
    TYPE (qd_real), INTENT(IN) :: a, b
    INTEGER :: r
    CALL f_qd_comp(a, b, r)
    IF (r == 1) THEN
       gt_qd = .TRUE.
    ELSE
       gt_qd = .FALSE.
    END IF
  END FUNCTION gt_qd

  LOGICAL FUNCTION gt_qd_d(a, b)
    TYPE (qd_real), INTENT(IN) :: a
    REAL*8, INTENT(IN) :: b
    INTEGER :: r
    CALL f_qd_comp_qd_d(a, b, r)
    IF (r == 1) THEN
       gt_qd_d = .TRUE.
    ELSE
       gt_qd_d = .FALSE.
    END IF
  END FUNCTION gt_qd_d

  LOGICAL FUNCTION gt_d_qd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (qd_real), INTENT(IN) :: b
    INTEGER :: r
    CALL f_qd_comp_d_qd(a, b, r)
    IF (r == 1) THEN
       gt_d_qd = .TRUE.
    ELSE
       gt_d_qd = .FALSE.
    END IF
  END FUNCTION gt_d_qd


! Less-Than
  LOGICAL FUNCTION lt_qd(a, b)
    TYPE (qd_real), INTENT(IN) :: a, b
    INTEGER :: r
    CALL f_qd_comp(a, b, r)
    IF (r == -1) THEN
       lt_qd = .TRUE.
    ELSE
       lt_qd = .FALSE.
    END IF
  END FUNCTION lt_qd

  LOGICAL FUNCTION lt_qd_d(a, b)
    TYPE (qd_real), INTENT(IN) :: a
    REAL*8, INTENT(IN) :: b
    INTEGER :: r
    CALL f_qd_comp_qd_d(a, b, r)
    IF (r == -1) THEN
       lt_qd_d = .TRUE.
    ELSE
       lt_qd_d = .FALSE.
    END IF
  END FUNCTION lt_qd_d

  LOGICAL FUNCTION lt_d_qd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (qd_real), INTENT(IN) :: b
    INTEGER :: r
    CALL f_qd_comp_d_qd(a, b, r)
    IF (r == -1) THEN
       lt_d_qd = .TRUE.
    ELSE
       lt_d_qd = .FALSE.
    END IF
  END FUNCTION lt_d_qd

! Greater-Than-Or-Equal-To
  LOGICAL FUNCTION ge_qd(a, b)
    TYPE (qd_real), INTENT(IN) :: a, b
    INTEGER :: r
    CALL f_qd_comp(a, b, r)
    IF (r >= 0) THEN
       ge_qd = .TRUE.
    ELSE
       ge_qd = .FALSE.
    END IF
  END FUNCTION ge_qd

  LOGICAL FUNCTION ge_qd_d(a, b)
    TYPE (qd_real), INTENT(IN) :: a
    REAL*8, INTENT(IN) :: b
    INTEGER :: r
    CALL f_qd_comp_qd_d(a, b, r)
    IF (r >= 0) THEN
       ge_qd_d = .TRUE.
    ELSE
       ge_qd_d = .FALSE.
    END IF
  END FUNCTION ge_qd_d

  LOGICAL FUNCTION ge_d_qd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (qd_real), INTENT(IN) :: b
    INTEGER :: r
    CALL f_qd_comp_d_qd(a, b, r)
    IF (r >= 0) THEN
       ge_d_qd = .TRUE.
    ELSE
       ge_d_qd = .FALSE.
    END IF
  END FUNCTION ge_d_qd

! Less-Than-Or-Equal-To
  LOGICAL FUNCTION le_qd(a, b)
    TYPE (qd_real), INTENT(IN) :: a, b
    INTEGER :: r
    CALL f_qd_comp(a, b, r)
    IF (r <= 0) THEN
       le_qd = .TRUE.
    ELSE
       le_qd = .FALSE.
    END IF
  END FUNCTION le_qd

  LOGICAL FUNCTION le_qd_d(a, b)
    TYPE (qd_real), INTENT(IN) :: a
    REAL*8, INTENT(IN) :: b
    INTEGER :: r
    CALL f_qd_comp_qd_d(a, b, r)
    IF (r <= 0) THEN
       le_qd_d = .TRUE.
    ELSE
       le_qd_d = .FALSE.
    END IF
  END FUNCTION le_qd_d

  LOGICAL FUNCTION le_d_qd(a, b)
    REAL*8, INTENT(IN) :: a
    TYPE (qd_real), INTENT(IN) :: b
    INTEGER :: r
    CALL f_qd_comp_d_qd(a, b, r)
    IF (r <= 0) THEN
       le_d_qd = .TRUE.
    ELSE
       le_d_qd = .FALSE.
    END IF
  END FUNCTION le_d_qd


! Absolute Value
  TYPE (qd_real) FUNCTION qdabs(a)
    TYPE (qd_real), INTENT(IN) :: a
    CALL f_qd_abs(a, qdabs)
  END FUNCTION qdabs

! Input
  SUBROUTINE qdinpq(u, q1, q2, q3, q4, q5, q6, q7, q8, q9)
    INTEGER, INTENT(IN) :: u
    TYPE (qd_real), INTENT(IN) :: q1
    TYPE (qd_real), INTENT(IN), OPTIONAL :: q2, q3, q4, q5, q6, q7, q8, q9

    call qdinp (u, q1%re(1))

    IF (PRESENT(q2)) THEN
      call qdinp (u, q2%re(1))
    END IF

    IF (PRESENT(q3)) THEN
      call qdinp (u, q3%re(1))
    END IF

    IF (PRESENT(q4)) THEN
      call qdinp (u, q4%re(1))
    END IF

    IF (PRESENT(q5)) THEN
      call qdinp (u, q5%re(1))
    END IF

    IF (PRESENT(q6)) THEN
      call qdinp (u, q6%re(1))
    END IF

    IF (PRESENT(q7)) THEN
      call qdinp (u, q7%re(1))
    END IF

    IF (PRESENT(q8)) THEN
      call qdinp (u, q8%re(1))
    END IF

    IF (PRESENT(q9)) THEN
      call qdinp (u, q9%re(1))
    END IF

  END SUBROUTINE qdinpq

! Output
  SUBROUTINE qdoutq(u, q1, q2, q3, q4, q5, q6, q7, q8, q9)
    INTEGER, INTENT(IN) :: u
    TYPE (qd_real), INTENT(IN) :: q1
    TYPE (qd_real), INTENT(IN), OPTIONAL :: q2, q3, q4, q5, q6, q7, q8, q9

    call qdout (u, q1%re(1))

    IF (PRESENT(q2)) THEN
      call qdout (u, q2%re(1))
    END IF

    IF (PRESENT(q3)) THEN
      call qdout (u, q3%re(1))
    END IF

    IF (PRESENT(q4)) THEN
      call qdout (u, q4%re(1))
    END IF

    IF (PRESENT(q5)) THEN
      call qdout (u, q5%re(1))
    END IF

    IF (PRESENT(q6)) THEN
      call qdout (u, q6%re(1))
    END IF

    IF (PRESENT(q7)) THEN
      call qdout (u, q7%re(1))
    END IF

    IF (PRESENT(q8)) THEN
      call qdout (u, q8%re(1))
    END IF

    IF (PRESENT(q9)) THEN
      call qdout (u, q9%re(1))
    END IF

  END SUBROUTINE qdoutq

  REAL*8 FUNCTION qd_to_d(a)
    TYPE (qd_real), INTENT(IN) :: a
    qd_to_d = a%re(1)
  END FUNCTION qd_to_d

  TYPE (qd_real) FUNCTION qdmin(a, b)
    TYPE (qd_real), INTENT(IN) :: a, b
    INTEGER :: r
    CALL f_qd_comp(a, b, r)
    IF (r == 1) THEN
       qdmin = b
    ELSE
       qdmin = a
    END IF
  END FUNCTION qdmin

  TYPE (qd_real) FUNCTION qdmin3(a, b, c)
    TYPE (qd_real), INTENT(IN) :: a, b, c
    INTEGER r
    CALL f_qd_comp(a, b, r)
    IF (r == 1) THEN
       ! b < a.  
       CALL f_qd_comp(b, c, r)
       IF (r == 1) THEN
          qdmin3 = c
       ELSE
          qdmin3 = b
       END IF
    ELSE
       ! b > a
       CALL f_qd_comp(a, c, r)
       IF (r == 1) THEN 
          qdmin3 = c
       ELSE
          qdmin3 = a
       END IF
    END IF
  END FUNCTION qdmin3

  TYPE (qd_real) FUNCTION qdmax(a, b)
    TYPE (qd_real), INTENT(IN) :: a, b
    INTEGER :: r
    CALL f_qd_comp(a, b, r)
    IF (r == -1) THEN
       qdmax = b
    ELSE
       qdmax = a
    END IF
  END FUNCTION qdmax

  TYPE (qd_real) FUNCTION qdmax3(a, b, c)
    TYPE (qd_real), INTENT(IN) :: a, b, c
    INTEGER r
    CALL f_qd_comp(a, b, r)
    IF (r == -1) THEN
       ! b > a.  
       CALL f_qd_comp(b, c, r)
       IF (r == -1) THEN
          qdmax3 = c
       ELSE
          qdmax3 = b
       END IF
    ELSE
       ! b < a
       CALL f_qd_comp(a, c, r)
       IF (r == -1) THEN 
          qdmax3 = c
       ELSE
          qdmax3 = a
       END IF
    END IF
  END FUNCTION qdmax3

  TYPE (qd_real) FUNCTION qd_pi()
    CALL f_qd_pi(qd_pi)
  END FUNCTION qd_pi

subroutine qdinp (iu, a)

!   This routine reads the DD number A from logical unit IU.  The input
!   value must be placed on a single line of not more than 80 characters.

implicit none
integer iu, ln
parameter (ln = 80)
character*80 cs
real*8 a(4)

read (iu, '(80a1)', end = 100) cs
call qdinpc (cs, a)
goto 110

100 write (6, 1)
1  format ('*** QDINP: End-of-file encountered.')
! call qdabrt
stop

110 return
end subroutine

subroutine qdinpc (a, b)

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
real*8 b(4), f(4), s0(4), s1(4), s2(4)

id = 0
ip = -1
is = 0
inz = 0
s1(1) = 0.d0
s1(2) = 0.d0
s1(3) = 0.d0
s1(4) = 0.d0

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
! call qdmuld (s1, 10.d0, s0)
      call f_qd_mul_qd_d (s1, 10.d0, s0)
      f(1) = bi
      f(2) = 0.d0
      f(3) = 0.d0
      f(4) = 0.d0
!    call qddqc (bi, f)
!    call qdadd (s0, f, s1)
      call f_qd_add (s0, f, s1)
    endif
  endif
enddo

100   continue
if (is .eq. -1) then
  s1(1) = - s1(1)
  s1(2) = - s1(2)
  s1(3) = - s1(3)
  s1(4) = - s1(4)
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
s0(3) = 0.d0
s0(4) = 0.d0
! call qdnpwr (s0, ie, s2)
call f_qd_npwr (s0, ie, s2)
! call qdmul (s1, s2, b)
call f_qd_mul (s1, s2, b)
goto 220

210  write (6, 1)
1 format ('*** DDINPC: Syntax error in literal string.')
! call qdabrt
stop

220  return
end subroutine

subroutine qdout (iu, a)

!   This routine writes the DD number A on logical unit iu using a standard
!   E format, with lines 72 characters long.

implicit none
integer i, iu, ln
parameter (ln = 72)
character*72 cs
real*8 a(4)

call qdoutc (a, cs)
write (iu, '(a)') cs

return
end subroutine

subroutine qdoutc (a, b)

!   Converts the QD number A into character form in the CHARACTER*72 array B
!   The format is analogous to the Fortran E format.

!   This routine is called by QDOUT, but it may be directly called by the user
!   if desired for custom output.

implicit none
integer i, ii, ix, ln, nx
parameter (ln = 72)
integer ib(ln)
real*8 t1
character*72 b
character*10 digits
character*16 ca
parameter (digits = '0123456789')
real*8 a(4), f(4), s0(4), s1(4)
real*8 dble

f(1) = 10.d0
f(2) = 0.d0
f(3) = 0.d0
f(4) = 0.d0

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
!  call qdnpwr (f, nx, s0)
  call f_qd_npwr (f, nx, s0)

!  call qddiv (a, s0, s1)
  call f_qd_div (a, s0, s1)
  if (s1(1) .lt. 0.d0) then
    s1(1) = - s1(1)
    s1(2) = - s1(2)
    s1(3) = - s1(3)
    s1(4) = - s1(4)
  endif

!   If we didn't quite get it exactly right, multiply or divide by 10 to fix.

  i = 0

100 continue

  i = i + 1
  if (s1(1) .lt. 1.d0) then
    nx = nx - 1
!    call qdmuld (s1, 10.d0, s0)
    call f_qd_mul_qd_d (s1, 10.d0, s0)
    s1(1) = s0(1)
    s1(2) = s0(2)
    s1(3) = s0(3)
    s1(4) = s0(4)
    if (i <= 3) goto 100
  elseif (s1(1) .ge. 10.d0) then
    nx = nx + 1
!    call qddivd (s1, 10.d0, s0)
    call f_qd_div_qd_d (s1, 10.d0, s0)
    s1(1) = s0(1)
    s1(2) = s0(2)
    s1(3) = s0(3)
    s1(4) = s0(4)
    goto 100
  endif
else
  nx = 0
  s1(1) = 0.d0
  s1(2) = 0.d0
  s1(3) = 0.d0
  s1(4) = 0.d0
endif

!   Compute digits.

do i = 1, ln - 8
  ii = s1(1)
  ib(i) = ii
  f(1) = ii
!  call qdsub (s1, f, s0)
  call f_qd_sub (s1, f, s0)
!  call qdmuld (s0, 10.d0, s1)
  call f_qd_mul_qd_d (s0, 10.d0, s1)
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
1 format ('qdoutc: negative leading digit')
!  call qdabrt
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

END MODULE qdmodule


