// Copyright (c) HPC Lab, Department of Electrical Engineering, IIT Bombay
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

package PtoF_Types;

import GetPut       :: *;
import ClientServer :: *;
import Posit_Numeric_Types :: *;
import Posit_User_Types :: *;
import Extracter_Types	:: *;

typedef struct {Bit#(1) sign;
		PositType zero_infinity_flag;
		Int#(FloatExpWidth) scale;
		Bit#(FloatFracWidth) frac;
		Int#(LogFloatFracWidthPlus1) frac_change;
		Bit#(1) truncated_frac_msb;
		Bit#(1) truncated_frac_zero;} Stage0_pf deriving(Bits,FShow);

typedef struct {Bit#(FloatWidth) float_out;
		PositType zero_infinity_flag;
		Bool rounding;} Output_float deriving(Bits,FShow);

interface PtoF_IFC ;
   interface Server #(Output_posit,Output_float) inoutifc;
endinterface


endpackage: PtoF_Types
