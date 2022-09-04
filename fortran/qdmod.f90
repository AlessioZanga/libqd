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
!  David H Bailey    2005-08-24

module qdmodule
use ddmodule
  implicit none
  
  type qd_real
     sequence
     real*8 :: re(4)
  end type qd_real

  real*8 d_qd_eps
  parameter (d_qd_eps = 1.51929083932157d-64)

  type (qd_real) qd_one, qd_zero, qd_eps, qd_huge, qd_tiny
  parameter (qd_one = qd_real((/1.0d0, 0.0d0, 0.0d0, 0.0d0/)))
  parameter (qd_zero = qd_real((/0.0d0, 0.0d0, 0.0d0, 0.0d0/)))
  parameter (qd_eps = qd_real((/d_qd_eps, 0.0d0, 0.0d0, 0.0d0/)))
  parameter (qd_huge = qd_real((/ &
    1.79769313486231570815d+308, 9.97920154767359795037d+291, &
    5.53956966280111259858d+275, 3.07507889307840487279d+259/)))
  parameter (qd_tiny = qd_real((/3.25194908739046463067d-260, &
    0.0d0, 0.0d0, 0.0d0/)))

  interface assignment (=)
     module procedure assign_qd_str
     module procedure assign_qd_d
     module procedure assign_d_qd
     module procedure assign_dd_qd
     module procedure assign_qd_dd
     module procedure assign_qd_i
     module procedure assign_i_qd
  end interface

  interface operator (+)
     module procedure add_qd
     module procedure add_qd_d
     module procedure add_d_qd
     module procedure add_qd_i
     module procedure add_i_qd
  end interface

  interface operator (-)
     module procedure sub_qd
     module procedure sub_qd_d
     module procedure sub_d_qd
     module procedure neg_qd
  end interface

  interface operator (*)
     module procedure mul_qd
     module procedure mul_qd_d
     module procedure mul_d_qd
     module procedure mul_qd_i
     module procedure mul_i_qd
  end interface

  interface operator (/)
     module procedure div_qd
     module procedure div_qd_d
     module procedure div_d_qd
     module procedure div_qd_i
     module procedure div_i_qd
  end interface

  interface operator (**)
     module procedure pwr_qd
     module procedure pwr_qd_i
     module procedure pwr_d_qd
  end interface

  interface qdreal
     module procedure to_qd_i
     module procedure to_qd_d
     module procedure to_qd_dd
     module procedure to_qd_qd
     module procedure to_qd_str
  end interface

  interface ddreal
     module procedure to_dd_qd
  end interface

  interface real
     module procedure to_real_qd
  end interface

  interface int
     module procedure to_int_qd
  end interface

  interface sin
     module procedure qdsin
  end interface
  interface cos
     module procedure qdcos
  end interface
  interface tan
     module procedure qdtan
  end interface
  interface sincos
     module procedure qdsincos
  end interface
  interface qdcssnf
     module procedure qdcossin
  end interface

  interface asin
     module procedure qdasin
  end interface
  interface acos
     module procedure qdacos
  end interface
  interface atan
     module procedure qdatan
  end interface
  interface atan2
     module procedure qdatan2
  end interface

  interface exp
     module procedure qdexp
  end interface
  interface log
     module procedure qdlog
  end interface
  interface log10
     module procedure qdlog10
  end interface

  interface sqrt
     module procedure qdsqrt
  end interface
  interface sqr
     module procedure qdsqr
  end interface
  interface nroot
     module procedure qdnroot
  end interface

  interface sinh
     module procedure qdsinh
  end interface
  interface cosh
     module procedure qdcosh
  end interface
  interface tanh
     module procedure qdtanh
  end interface
  interface sincosh
     module procedure qdsincosh
  end interface
  interface qdcsshf
     module procedure qdcossinh
  end interface

  interface asinh
     module procedure qdasinh
  end interface
  interface acosh
     module procedure qdacosh
  end interface
  interface atanh
     module procedure qdatanh
  end interface

  interface aint
     module procedure qdaint
  end interface

  interface nint
     module procedure qdnint
  end interface

  interface anint
     module procedure qdanint
  end interface

  interface abs
     module procedure qdabs
  end interface

  interface sign
     module procedure qdsign
     module procedure qdsign_dd_d
  end interface

  interface qdrand
     module procedure qdrand
  end interface

  interface operator (==)
     module procedure eq_qd
     module procedure eq_qd_d
     module procedure eq_d_qd
     module procedure eq_qd_i
     module procedure eq_i_qd
  end interface

  interface operator (/=)
     module procedure ne_qd
     module procedure ne_qd_d
     module procedure ne_d_qd
     module procedure ne_qd_i
     module procedure ne_i_qd
  end interface

  interface operator (>)
     module procedure gt_qd
     module procedure gt_qd_d
     module procedure gt_d_qd
     module procedure gt_qd_i
     module procedure gt_i_qd
  end interface

  interface operator (<)
     module procedure lt_qd
     module procedure lt_qd_d
     module procedure lt_d_qd
     module procedure lt_qd_i
     module procedure lt_i_qd
  end interface

  interface operator (>=)
     module procedure ge_qd
     module procedure ge_qd_d
     module procedure ge_d_qd
     module procedure ge_qd_i
     module procedure ge_i_qd
  end interface

  interface operator (<=)
     module procedure le_qd
     module procedure le_qd_d
     module procedure le_d_qd
     module procedure le_qd_i
     module procedure le_i_qd
  end interface

  interface qdread
     module procedure qdinpq
  end interface

  interface qdwrite
     module procedure qdoutq
  end interface

  interface dble
     module procedure qd_to_d
  end interface

  interface min
     module procedure qdmin
     module procedure qdmin2
  end interface
  interface max
     module procedure qdmax
     module procedure qdmax2
  end interface

  interface qdpi
     module procedure qd_pi
  end interface

  interface huge
     module procedure qdhuge
  end interface

  interface tiny
     module procedure qdtiny
  end interface

  interface epsilon
     module procedure qdepsilon
  end interface

contains

! Assignments
  subroutine assign_qd_str(a, s)
    type (qd_real), intent(inout) :: a
    character (len=*), intent(in) :: s
    character*80 t
    t = s
    call qdinpc (t, a%re(1))
  end subroutine assign_qd_str

  subroutine assign_qd_d(a, d)
    type (qd_real), intent(inout) :: a
    real*8, intent(in) :: d
    a%re(1) = d
    a%re(2) = 0.0d0
    a%re(3) = 0.0d0
    a%re(4) = 0.0d0
  end subroutine assign_qd_d

  subroutine assign_d_qd(d, a)
    real*8, intent(inout) :: d
    type (qd_real), intent(in) :: a
    d = a%re(1)
  end subroutine assign_d_qd

  subroutine assign_qd_i(a, i)
    type (qd_real), intent(inout) :: a
    integer, intent(in) :: i
    a%re(1) = i
    a%re(2) = 0.0d0
    a%re(3) = 0.0d0
    a%re(4) = 0.0d0
  end subroutine assign_qd_i

  subroutine assign_i_qd(i, a)
    integer, intent(inout) :: i
    type (qd_real), intent(in) :: a
    i = a%re(1)
  end subroutine assign_i_qd


  subroutine assign_dd_qd(dd, a)
    type (dd_real), intent(inout) :: dd
    type (qd_real), intent(in) :: a
    dd%re(1) = a%re(1)
    dd%re(2) = a%re(2)
  end subroutine assign_dd_qd

  subroutine assign_qd_dd(a, dd)
    type (qd_real), intent(inout) :: a
    type (dd_real), intent(in) :: dd
    a%re(1) = dd%re(1)
    a%re(2) = dd%re(2)
    a%re(3) = 0.d0
    a%re(4) = 0.d0
  end subroutine assign_qd_dd

! Conversions

  type (qd_real) function to_qd_i(ia)
    integer, intent(in) :: ia
    to_qd_i%re(1) = ia
    to_qd_i%re(2) = 0.d0
    to_qd_i%re(3) = 0.d0
    to_qd_i%re(4) = 0.d0
  end function to_qd_i

  type (qd_real) function to_qd_d(a)
    real*8, intent(in) :: a
    to_qd_d%re(1) = a
    to_qd_d%re(2) = 0.0d0
    to_qd_d%re(3) = 0.0d0
    to_qd_d%re(4) = 0.0d0
  end function to_qd_d

  real*8 function to_real_qd(a) 
    type (qd_real), intent(in) :: a
    to_real_qd = a%re(1)
  end function to_real_qd

  integer function to_int_qd(a) 
    type (qd_real), intent(in) :: a
    to_int_qd = a%re(1)
  end function to_int_qd

  type (qd_real) function to_qd_dd (dd)
     type (dd_real), intent(in) :: dd
     to_qd_dd%re(1) = dd%re(1)
     to_qd_dd%re(2) = dd%re(2)
     to_qd_dd%re(3) = 0.d0
     to_qd_dd%re(4) = 0.d0
  end function to_qd_dd

  type (qd_real) function to_qd_qd (qd)
     type (qd_real), intent(in) :: qd
     to_qd_qd%re(1) = qd%re(1)
     to_qd_qd%re(2) = qd%re(2)
     to_qd_qd%re(3) = qd%re(3)
     to_qd_qd%re(4) = qd%re(4)
  end function to_qd_qd

  type (dd_real) function to_dd_qd (qd)
     type (qd_real), intent(in) :: qd
     to_dd_qd%re(1) = qd%re(1)
     to_dd_qd%re(2) = qd%re(2)
  end function to_dd_qd

  type (qd_real) function to_qd_str(s)
    character (len=*), intent(in) :: s
    character*80 t
    t = s
    call qdinpc (t, to_qd_str%re(1))
  end function to_qd_str

! Additions
  type (qd_real) function add_qd(a, b)
    type (qd_real), intent(in) :: a, b
    call f_qd_add(a, b, add_qd)
  end function add_qd

  type (qd_real) function add_qd_d(a, b)
    type (qd_real), intent(in) :: a
    real*8, intent(in) :: b
    call f_qd_add_qd_d(a, b, add_qd_d)
  end function add_qd_d

  type (qd_real) function add_d_qd(a, b)
    real*8, intent(in) :: a
    type (qd_real), intent(in) :: b
    call f_qd_add_d_qd(a, b, add_d_qd)
  end function add_d_qd

  type (qd_real) function add_i_qd(a, b)
    integer, intent(in) :: a
    type (qd_real), intent(in) :: b
    call f_qd_add_d_qd(dble(a), b, add_i_qd)
  end function add_i_qd

  type (qd_real) function add_qd_i(a, b)
    type (qd_real), intent(in) :: a
    integer, intent(in) :: b
    call f_qd_add_qd_d(a, dble(b), add_qd_i)
  end function add_qd_i

! Subtractions
  type (qd_real) function sub_qd(a, b)
    type (qd_real), intent(in) :: a, b
    call f_qd_sub(a, b, sub_qd)
  end function sub_qd

  type (qd_real) function sub_qd_d(a, b)
    type (qd_real), intent(in) :: a
    real*8, intent(in) :: b
    call f_qd_sub_qd_d(a, b, sub_qd_d)
  end function sub_qd_d

  type (qd_real) function sub_d_qd(a, b)
    real*8, intent(in) :: a
    type (qd_real), intent(in) :: b
    call f_qd_sub_d_qd(a, b, sub_d_qd)
  end function sub_d_qd

! Unary Minus
  type (qd_real) function neg_qd(a)
    type (qd_real), intent(in) :: a
    call f_qd_neg(a, neg_qd)
  end function neg_qd


! Multiplications
  type (qd_real) function mul_qd(a, b)
    type (qd_real), intent(in) :: a, b
    call f_qd_mul(a, b, mul_qd)
  end function mul_qd

  type (qd_real) function mul_qd_d(a, b)
    type (qd_real), intent(in) :: a
    real*8, intent(in) :: b
    call f_qd_mul_qd_d(a, b, mul_qd_d)
  end function mul_qd_d

  type (qd_real) function mul_d_qd(a, b)
    real*8, intent(in) :: a
    type (qd_real), intent(in) :: b
    call f_qd_mul_d_qd(a, b, mul_d_qd)
  end function mul_d_qd

  type (qd_real) function mul_qd_i(a, b)
    type (qd_real), intent(in) :: a
    integer, intent(in) :: b
    call f_qd_mul_qd_d(a, dble(b), mul_qd_i)
  end function mul_qd_i

  type (qd_real) function mul_i_qd(a, b)
    integer, intent(in) :: a
    type (qd_real), intent(in) :: b
    call f_qd_mul_d_qd(dble(a), b, mul_i_qd)
  end function mul_i_qd

! Divisions
  type (qd_real) function div_qd(a, b)
    type (qd_real), intent(in) :: a, b
    call f_qd_div(a, b, div_qd)
  end function div_qd

  type (qd_real) function div_qd_d(a, b)
    type (qd_real), intent(in) :: a
    real*8, intent(in) :: b
    call f_qd_div_qd_d(a, b, div_qd_d)
  end function div_qd_d

  type (qd_real) function div_d_qd(a, b)
    real*8, intent(in) :: a
    type (qd_real), intent(in) :: b
    call f_qd_div_d_qd(a, b, div_d_qd)
  end function div_d_qd

  type (qd_real) function div_qd_i(a, b)
    type (qd_real), intent(in) :: a
    integer, intent(in) :: b
    call f_qd_div_qd_d(a, dble(b), div_qd_i)
  end function div_qd_i

  type (qd_real) function div_i_qd(a, b)
    integer, intent(in) :: a
    type (qd_real), intent(in) :: b
    call f_qd_div_d_qd(dble(a), b, div_i_qd)
  end function div_i_qd


! Power
  type (qd_real) function pwr_qd (a, b)
    type (qd_real), intent(in) :: a, b
    type (qd_real) q1, q2
    call f_qd_log(a, q1)
    call f_qd_mul(q1, b, q2)
    call f_qd_exp(q2, pwr_qd)
  end function pwr_qd

  type (qd_real) function pwr_qd_i(a, n)
    type (qd_real), intent(in) :: a
    integer, intent(in) :: n
    call f_qd_npwr(a, n, pwr_qd_i)
  end function pwr_qd_i

  type (qd_real) function pwr_d_qd(a, b)
    real*8, intent(in) :: a
    type (qd_real), intent(in) :: b
    type (qd_real) q1, q2, q3
    q1%re(1) = a
    q1%re(2) = 0.d0
    q1%re(3) = 0.d0
    q1%re(4) = 0.d0
    call f_qd_log(q1, q2)
    call f_qd_mul(q2, b, q3)
    call f_qd_exp(q3, pwr_d_qd)
  end function pwr_d_qd

! Trigonometric Functions
  type (qd_real) function qdsin(a)
    type (qd_real), intent(in) :: a
    call f_qd_sin(a, qdsin)
  end function qdsin

  type (qd_real) function qdcos(a)
    type (qd_real), intent(in) :: a
    call f_qd_cos(a, qdcos)
  end function qdcos

  type (qd_real) function qdtan(a)
    type (qd_real), intent(in) :: a
    call f_qd_tan(a, qdtan)
  end function qdtan

  subroutine qdsincos(a, s, c)
    type (qd_real), intent(in) :: a
    type (qd_real), intent(out) :: s, c
    call f_qd_sincos(a, s, c)
  end subroutine qdsincos

  subroutine qdcossin(a, c, s)
    type (qd_real), intent(in) :: a
    type (qd_real), intent(out) :: s, c
    call f_qd_sincos(a, s, c)
  end subroutine qdcossin


! Inverse Trigonometric Functions
  type (qd_real) function qdasin(a)
    type (qd_real), intent(in) :: a
    call f_qd_asin(a, qdasin)
  end function qdasin

  type (qd_real) function qdacos(a)
    type (qd_real), intent(in) :: a
    call f_qd_acos(a, qdacos)
  end function qdacos

  type (qd_real) function qdatan(a)
    type (qd_real), intent(in) :: a
    call f_qd_atan(a, qdatan)
  end function qdatan

  type (qd_real) function qdatan2(a, b)
    type (qd_real), intent(in) :: a, b
    call f_qd_atan2(a, b, qdatan2)
  end function qdatan2

! Exponential and Logarithms
  type (qd_real) function qdexp(a)
    type (qd_real), intent(in) :: a
    call f_qd_exp(a, qdexp)
  end function qdexp

  type (qd_real) function qdlog(a)
    type (qd_real), intent(in) :: a
    call f_qd_log(a, qdlog)
  end function qdlog

  type (qd_real) function qdlog10(a)
    type (qd_real), intent(in) :: a
    call f_qd_log10(a, qdlog10)
  end function qdlog10


! SQRT, etc.
  type (qd_real) function qdsqrt(a)
    type (qd_real), intent(in) :: a
    call f_qd_sqrt(a, qdsqrt)
  end function qdsqrt

  type (qd_real) function qdsqr(a)
    type (qd_real), intent(in) :: a
    call f_qd_sqr(a, qdsqr)
  end function qdsqr

  type (qd_real) function qdnroot(a, n)
    type (qd_real), intent(in) :: a
    integer, intent(in) :: n
    call f_qd_nroot(a, n, qdnroot)
  end function qdnroot


! Hyperbolic Functions
  type (qd_real) function qdsinh(a)
    type (qd_real), intent(in) :: a
    call f_qd_sinh(a, qdsinh)
  end function qdsinh

  type (qd_real) function qdcosh(a)
    type (qd_real), intent(in) :: a
    call f_qd_cosh(a, qdcosh)
  end function qdcosh

  type (qd_real) function qdtanh(a)
    type (qd_real), intent(in) :: a
    call f_qd_tanh(a, qdtanh)
  end function qdtanh

  subroutine qdsincosh(a, s, c)
    type (qd_real), intent(in) :: a
    type (qd_real), intent(out) :: s, c
    call f_qd_sincosh(a, s, c)
  end subroutine qdsincosh

  subroutine qdcossinh(a, c, s)
    type (qd_real), intent(in) :: a
    type (qd_real), intent(out) :: s, c
    call f_qd_sincosh(a, s, c)
  end subroutine qdcossinh

! Inverse Hyperbolic Functions
  type (qd_real) function qdasinh(a)
    type (qd_real), intent(in) :: a
    call f_qd_asinh(a, qdasinh)
  end function qdasinh

  type (qd_real) function qdacosh(a)
    type (qd_real), intent(in) :: a
    call f_qd_acosh(a, qdacosh)
  end function qdacosh

  type (qd_real) function qdatanh(a)
    type (qd_real), intent(in) :: a
    call f_qd_atanh(a, qdatanh)
  end function qdatanh


! Rounding
  type (qd_real) function qdaint(a)
    type (qd_real), intent(in) :: a
    call f_qd_aint(a, qdaint)
  end function qdaint

  type (qd_real) function qdanint(a)
    type (qd_real), intent(in) :: a
    call f_qd_nint(a, qdanint)
  end function qdanint

  integer function qdnint(a)
    type (qd_real), intent(in) :: a
    qdnint = to_int_qd(qdaint(a));
  end function qdnint


! Random Number Generator
  type (qd_real) function qdrand()
    call f_qd_rand(qdrand)
  end function qdrand


! Equality
  logical function eq_qd(a, b)
    type (qd_real), intent(in) :: a, b
    integer :: r
    call f_qd_comp(a, b, r)
    if (r == 0) then
       eq_qd = .true.
    else
       eq_qd = .false.
    end if
  end function eq_qd

  logical function eq_qd_d(a, b)
    type (qd_real), intent(in) :: a
    real*8, intent(in) :: b
    integer :: r
    call f_qd_comp_qd_d(a, b, r)
    if (r == 0) then
       eq_qd_d = .true.
    else
       eq_qd_d = .false.
    end if
  end function eq_qd_d

  logical function eq_d_qd(a, b)
    real*8, intent(in) :: a
    type (qd_real), intent(in) :: b
    integer :: r
    call f_qd_comp_d_qd(a, b, r)
    if (r == 0) then
       eq_d_qd = .true.
    else
       eq_d_qd = .false.
    end if
  end function eq_d_qd

  logical function eq_qd_i(a, b)
    type (qd_real), intent(in) :: a
    integer, intent(in) :: b
    eq_qd_i = eq_qd_d(a, dble(b))
  end function eq_qd_i

  logical function eq_i_qd(a, b)
    integer, intent(in) :: a
    type (qd_real), intent(in) :: b
    eq_i_qd = eq_d_qd(dble(a), b)
  end function eq_i_qd

! Non-Equality
  logical function ne_qd(a, b)
    type (qd_real), intent(in) :: a, b
    integer :: r
    call f_qd_comp(a, b, r)
    if (r == 0) then
       ne_qd = .false.
    else
       ne_qd = .true.
    end if
  end function ne_qd

  logical function ne_qd_d(a, b)
    type (qd_real), intent(in) :: a
    real*8, intent(in) :: b
    integer :: r
    call f_qd_comp_qd_d(a, b, r)
    if (r == 0) then
       ne_qd_d = .false.
    else
       ne_qd_d = .true.
    end if
  end function ne_qd_d

  logical function ne_d_qd(a, b)
    real*8, intent(in) :: a
    type (qd_real), intent(in) :: b
    integer :: r
    call f_qd_comp_d_qd(a, b, r)
    if (r == 0) then
       ne_d_qd = .false.
    else
       ne_d_qd = .true.
    end if
  end function ne_d_qd

  logical function ne_qd_i(a, b)
    type (qd_real), intent(in) :: a
    integer, intent(in) :: b
    ne_qd_i = ne_qd_d(a, dble(b))
  end function ne_qd_i

  logical function ne_i_qd(a, b)
    integer, intent(in) :: a
    type (qd_real), intent(in) :: b
    ne_i_qd = ne_d_qd(dble(a), b)
  end function ne_i_qd

! Greater-Than
  logical function gt_qd(a, b)
    type (qd_real), intent(in) :: a, b
    integer :: r
    call f_qd_comp(a, b, r)
    if (r == 1) then
       gt_qd = .true.
    else
       gt_qd = .false.
    end if
  end function gt_qd

  logical function gt_qd_d(a, b)
    type (qd_real), intent(in) :: a
    real*8, intent(in) :: b
    integer :: r
    call f_qd_comp_qd_d(a, b, r)
    if (r == 1) then
       gt_qd_d = .true.
    else
       gt_qd_d = .false.
    end if
  end function gt_qd_d

  logical function gt_d_qd(a, b)
    real*8, intent(in) :: a
    type (qd_real), intent(in) :: b
    integer :: r
    call f_qd_comp_d_qd(a, b, r)
    if (r == 1) then
       gt_d_qd = .true.
    else
       gt_d_qd = .false.
    end if
  end function gt_d_qd

  logical function gt_qd_i(a, b)
    type (qd_real), intent(in) :: a
    integer, intent(in) :: b
    gt_qd_i = gt_qd_d(a, dble(b))
  end function gt_qd_i

  logical function gt_i_qd(a, b)
    integer, intent(in) :: a
    type (qd_real), intent(in) :: b
    gt_i_qd = gt_d_qd(dble(a), b)
  end function gt_i_qd

! Less-Than
  logical function lt_qd(a, b)
    type (qd_real), intent(in) :: a, b
    integer :: r
    call f_qd_comp(a, b, r)
    if (r == -1) then
       lt_qd = .true.
    else
       lt_qd = .false.
    end if
  end function lt_qd

  logical function lt_qd_d(a, b)
    type (qd_real), intent(in) :: a
    real*8, intent(in) :: b
    integer :: r
    call f_qd_comp_qd_d(a, b, r)
    if (r == -1) then
       lt_qd_d = .true.
    else
       lt_qd_d = .false.
    end if
  end function lt_qd_d

  logical function lt_d_qd(a, b)
    real*8, intent(in) :: a
    type (qd_real), intent(in) :: b
    integer :: r
    call f_qd_comp_d_qd(a, b, r)
    if (r == -1) then
       lt_d_qd = .true.
    else
       lt_d_qd = .false.
    end if
  end function lt_d_qd

  logical function lt_qd_i(a, b)
    type (qd_real), intent(in) :: a
    integer, intent(in) :: b
    lt_qd_i = lt_qd_d(a, dble(b))
  end function lt_qd_i

  logical function lt_i_qd(a, b)
    integer, intent(in) :: a
    type (qd_real), intent(in) :: b
    lt_i_qd = lt_d_qd(dble(a), b)
  end function lt_i_qd

! Greater-Than-Or-Equal-To
  logical function ge_qd(a, b)
    type (qd_real), intent(in) :: a, b
    integer :: r
    call f_qd_comp(a, b, r)
    if (r >= 0) then
       ge_qd = .true.
    else
       ge_qd = .false.
    end if
  end function ge_qd

  logical function ge_qd_d(a, b)
    type (qd_real), intent(in) :: a
    real*8, intent(in) :: b
    integer :: r
    call f_qd_comp_qd_d(a, b, r)
    if (r >= 0) then
       ge_qd_d = .true.
    else
       ge_qd_d = .false.
    end if
  end function ge_qd_d

  logical function ge_d_qd(a, b)
    real*8, intent(in) :: a
    type (qd_real), intent(in) :: b
    integer :: r
    call f_qd_comp_d_qd(a, b, r)
    if (r >= 0) then
       ge_d_qd = .true.
    else
       ge_d_qd = .false.
    end if
  end function ge_d_qd

  logical function ge_qd_i(a, b)
    type (qd_real), intent(in) :: a
    integer, intent(in) :: b
    ge_qd_i = ge_qd_d(a, dble(b))
  end function ge_qd_i

  logical function ge_i_qd(a, b)
    integer, intent(in) :: a
    type (qd_real), intent(in) :: b
    ge_i_qd = ge_d_qd(dble(a), b)
  end function ge_i_qd

! Less-Than-Or-Equal-To
  logical function le_qd(a, b)
    type (qd_real), intent(in) :: a, b
    integer :: r
    call f_qd_comp(a, b, r)
    if (r <= 0) then
       le_qd = .true.
    else
       le_qd = .false.
    end if
  end function le_qd

  logical function le_qd_d(a, b)
    type (qd_real), intent(in) :: a
    real*8, intent(in) :: b
    integer :: r
    call f_qd_comp_qd_d(a, b, r)
    if (r <= 0) then
       le_qd_d = .true.
    else
       le_qd_d = .false.
    end if
  end function le_qd_d

  logical function le_d_qd(a, b)
    real*8, intent(in) :: a
    type (qd_real), intent(in) :: b
    integer :: r
    call f_qd_comp_d_qd(a, b, r)
    if (r <= 0) then
       le_d_qd = .true.
    else
       le_d_qd = .false.
    end if
  end function le_d_qd

  logical function le_qd_i(a, b)
    type (qd_real), intent(in) :: a
    integer, intent(in) :: b
    le_qd_i = le_qd_d(a, dble(b))
  end function le_qd_i

  logical function le_i_qd(a, b)
    integer, intent(in) :: a
    type (qd_real), intent(in) :: b
    le_i_qd = le_d_qd(dble(a), b)
  end function le_i_qd


! Absolute Value
  type (qd_real) function qdabs(a)
    type (qd_real), intent(in) :: a
    call f_qd_abs(a, qdabs)
  end function qdabs

! Sign transfer
  type (qd_real) function qdsign(a, b) result (c)
    type (qd_real), intent(in) :: a, b
    integer :: r
    if (b%re(1) .gt. 0.0d0) then
      c%re(1) = abs(a%re(1))
      c%re(2) = abs(a%re(2))
      c%re(3) = abs(a%re(3))
      c%re(4) = abs(a%re(4))
    else
      c%re(1) = -abs(a%re(1))
      c%re(2) = -abs(a%re(2))
      c%re(3) = -abs(a%re(3))
      c%re(4) = -abs(a%re(4))
    endif
  end function qdsign

  type (qd_real) function qdsign_dd_d(a, b) result (c)
    type (qd_real), intent(in) :: a
    real*8, intent(in) :: b
    integer :: r
    if (b .gt. 0.0d0) then
      c%re(1) = abs(a%re(1))
      c%re(2) = abs(a%re(2))
      c%re(3) = abs(a%re(3))
      c%re(4) = abs(a%re(4))
    else
      c%re(1) = -abs(a%re(1))
      c%re(2) = -abs(a%re(2))
      c%re(3) = -abs(a%re(3))
      c%re(4) = -abs(a%re(4))
    endif
  end function qdsign_dd_d

! Input
  subroutine qdinpq(u, q1, q2, q3, q4, q5, q6, q7, q8, q9)
    integer, intent(in) :: u
    type (qd_real), intent(in) :: q1
    type (qd_real), intent(in), optional :: q2, q3, q4, q5, q6, q7, q8, q9

    call qdinp (u, q1%re(1))

    if (present(q2)) then
      call qdinp (u, q2%re(1))
    end if

    if (present(q3)) then
      call qdinp (u, q3%re(1))
    end if

    if (present(q4)) then
      call qdinp (u, q4%re(1))
    end if

    if (present(q5)) then
      call qdinp (u, q5%re(1))
    end if

    if (present(q6)) then
      call qdinp (u, q6%re(1))
    end if

    if (present(q7)) then
      call qdinp (u, q7%re(1))
    end if

    if (present(q8)) then
      call qdinp (u, q8%re(1))
    end if

    if (present(q9)) then
      call qdinp (u, q9%re(1))
    end if

  end subroutine qdinpq

! Output
  subroutine qdoutq(u, q1, q2, q3, q4, q5, q6, q7, q8, q9)
    integer, intent(in) :: u
    type (qd_real), intent(in) :: q1
    type (qd_real), intent(in), optional :: q2, q3, q4, q5, q6, q7, q8, q9

    call qdout (u, q1%re(1))

    if (present(q2)) then
      call qdout (u, q2%re(1))
    end if

    if (present(q3)) then
      call qdout (u, q3%re(1))
    end if

    if (present(q4)) then
      call qdout (u, q4%re(1))
    end if

    if (present(q5)) then
      call qdout (u, q5%re(1))
    end if

    if (present(q6)) then
      call qdout (u, q6%re(1))
    end if

    if (present(q7)) then
      call qdout (u, q7%re(1))
    end if

    if (present(q8)) then
      call qdout (u, q8%re(1))
    end if

    if (present(q9)) then
      call qdout (u, q9%re(1))
    end if

  end subroutine qdoutq

  real*8 function qd_to_d(a)
    type (qd_real), intent(in) :: a
    qd_to_d = a%re(1)
  end function qd_to_d

  type (qd_real) function qdmin2(a, b)
    type (qd_real), intent(in) :: a, b
    integer :: r
    call f_qd_comp(a, b, r)
    if (r == 1) then
       qdmin2 = b
    else
       qdmin2 = a
    end if
  end function qdmin2

  type (qd_real) function qdmin(a1, a2, a3, a4, a5, a6, a7, a8, a9)
    type (qd_real), intent(in) :: a1, a2, a3
    type (qd_real), intent(in), optional :: a4, a5, a6, a7, a8, a9
    qdmin = qdmin2(qdmin2(a1, a2), a3)
    if (present(a4)) qdmin = qdmin2(qdmin, a4)
    if (present(a5)) qdmin = qdmin2(qdmin, a5)
    if (present(a6)) qdmin = qdmin2(qdmin, a6)
    if (present(a7)) qdmin = qdmin2(qdmin, a7)
    if (present(a8)) qdmin = qdmin2(qdmin, a8)
    if (present(a9)) qdmin = qdmin2(qdmin, a9)
  end function qdmin

  type (qd_real) function qdmax2(a, b)
    type (qd_real), intent(in) :: a, b
    integer :: r
    call f_qd_comp(a, b, r)
    if (r == -1) then
       qdmax2 = b
    else
       qdmax2 = a
    end if
  end function qdmax2

  type (qd_real) function qdmax(a1, a2, a3, a4, a5, a6, a7, a8, a9)
    type (qd_real), intent(in) :: a1, a2, a3
    type (qd_real), intent(in), optional :: a4, a5, a6, a7, a8, a9
    qdmax = qdmax2(qdmax2(a1, a2), a3)
    if (present(a4)) qdmax = qdmax2(qdmax, a4)
    if (present(a5)) qdmax = qdmax2(qdmax, a5)
    if (present(a6)) qdmax = qdmax2(qdmax, a6)
    if (present(a7)) qdmax = qdmax2(qdmax, a7)
    if (present(a8)) qdmax = qdmax2(qdmax, a8)
    if (present(a9)) qdmax = qdmax2(qdmax, a9)
  end function qdmax

  type (qd_real) function qd_pi()
    call f_qd_pi(qd_pi)
  end function qd_pi

subroutine qdinp (iu, a)

!   This routine reads the DD number A from logical unit IU.  The input
!   value must be placed on a single line of not more than 80 characters.

implicit none
integer iu, ln
parameter (ln = 80)
character*80 cs
real*8 a(4)

read (iu, '(a)', end = 100) cs
call qdinpc (cs, a)
goto 110

100 write (6, 1)
1  format ('*** qdinp: End-of-file encountered.')
! call qdabrt
stop

110 return

end subroutine

subroutine qdinpc (a, b)

!   Converts the CHARACTER*80 array A into the DD number B.

implicit none
integer i, ib, id, ie, inz, ip, is, ix, k, ln, lnn, beg
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

do i = 1, 80
  if (a(i:i) /= ' ') then
    beg = i
    goto 80
  end if
end do
80 continue

do i = beg, 80
  if (a(i:i) == ' ') then
    lnn = i-1
    goto 90
  end if
enddo

lnn = 80
90 continue

!   Scan for digits, looking for the period also.

do i = beg, lnn
  ai = a(i:i)
  if (ai .eq. '.') then
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
1 format ('*** qdinpc: Syntax error in literal string.')
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

type (qd_real) function qdhuge(a) 
  type (qd_real), intent(in) :: a
  qdhuge = qd_huge
end function qdhuge

type (qd_real) function qdtiny(a) 
  type (qd_real), intent(in) :: a
  qdtiny = qd_tiny
end function qdtiny

type (qd_real) function qdepsilon(a) 
  type (qd_real), intent(in) :: a
  qdepsilon = qd_eps
end function qdepsilon

end module qdmodule


