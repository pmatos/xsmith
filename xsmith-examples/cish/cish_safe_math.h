/* -*- mode: C -*-
 *
 * Copyright (c) 2018-2020 The University of Utah
 * All rights reserved.
 *
 * This file is part of Xsmith, a generator of highly effective fuzz testers.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
   
/*****************************************************************************/

/*
  Get safe math to work, at least at a basic level.
*/


#define STATIC static
#define FUNC_NAME(x) (safe_##x)
#define LOG_INDEX
#define LOG_EXEC
#define UNDEFINED(__val) (__val)
#include <math.h>
#include <stdint.h>
//#include "custom_limits.h"
#define FLT_MAX 3.40282347e+38F
#define DBL_MAX 1.7976931348623158e+308
#include "safe_math.h"



/*
// These includes are in the csmith runtime dir
//#include "csmith.h"
//#include "safe_math_macros_notmp.h"
//#include "custom_limits.h"
#define CHAR_BIT 8
#include "safe_math_macros.h"
*/


/*****************************************************************************/



/*
 * Local Variables:
 * c-basic-offset: 4
 * tab-width: 4
 * End:
 */

/* End of file. */
