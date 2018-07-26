/*
  Get safe math to work, at least at a basic level.
*/

#include <stdint.h>


// These includes are in the csmith runtime dir
//#include "csmith.h"
//#include "safe_math_macros_notmp.h"
//#include "custom_limits.h"
#define CHAR_BIT 8
#include "safe_math_macros.h"

#define safe_add_func_float_f_f(a,b) (a + b)
#define safe_sub_func_float_f_f(a,b) (a - b)
#define safe_mul_func_float_f_f(a,b) (a * b)
#define safe_div_func_float_f_f(a,b) (a / b)
#define safe_mod_func_float_f_f(a,b) (a % b)

