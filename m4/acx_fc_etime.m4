AC_DEFUN([ACX_FC_ETIME], [
AC_MSG_CHECKING([for etime])
AC_LANG_PUSH(Fortran)
acx_fc_etime=
acx_fc_etime_names="etime etime_"
for name in $acx_fc_etime_names; do
  AC_LINK_IFELSE([AC_LANG_PROGRAM(, [[
      real*4 t(2), tot
      tot = $name(t)]])], 
    [acx_fc_etime=$name], [])
  if test "x$acx_fc_etime" != "x"; then
    break;
  fi
done
AC_LANG_POP(Fortran)
if test "x$acx_fc_etime" != "x"; then
  AC_MSG_RESULT($acx_fc_etime)
  $1
else
  AC_MSG_RESULT(none)
  ifelse([$2],,AC_MSG_ERROR([Cannot find etime.]), [$2])
fi
])
