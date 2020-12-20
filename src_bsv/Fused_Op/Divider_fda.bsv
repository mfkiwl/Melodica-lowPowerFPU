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

package Divider_fda;

// --------------------------------------------------------------
// This package defines:
//
// mkDivider: 2-stage posit Divider
// --------------------------------------------------------------

import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;

import Divider_Types_fda :: *;
import Posit_Numeric_Types :: *;
import Posit_User_Types :: *;
import IntDivide_generic ::*;
import Common_Fused_Op :: *;

module mkDivider (Divider_IFC );
`ifdef PIPELINED
	// make a FIFO to store 
   	FIFOF #(Outputs_md )  fifo_output_reg <- mkFIFOF;
	FIFOF #(Stage0_d )  fifo_stage0_reg <- mkFIFOF;
`else
	// make a FIFO to store 
   	FIFOF #(Outputs_md )  fifo_output_reg <- mkFIFOF1;
	FIFOF #(Stage0_d )  fifo_stage0_reg <- mkFIFOF1;
`endif
	//This function is used to identify nan cases
	IntDivide intDivide <- mkIntDivide;
	function Bit#(1) check_for_nan_div(PositType z_i1, PositType z_i2,Bit#(1) nan1,Bit#(1) nan2 );
		if ((z_i1 == INF && z_i2 == ZERO)||(z_i2 == INF && z_i1 == INF)||(nan1 == 1'b1)||(nan2 == 1'b1))
			//nan flag = 1 when one input is infinity and other zero
			return 1'b1;
		else 
			return 1'b0;
	endfunction
	//This function is used to identify zer or infinity cases depending only on the flag value of inputs

	function PositType check_for_z_i_div(PositType z_i1, PositType z_i2);
		if ((z_i1 == ZERO && z_i2 != ZERO) || (z_i1 != INF && z_i2 == INF))
			// if both inputs are zero then output is zero
			return ZERO;
		else if ((z_i1 == INF && z_i2 != INF) || (z_i1 != ZERO && z_i2 == ZERO))
			// if one of the inputs is infinity then output is infinity
			return INF;
		else 
			return REGULAR;
	endfunction
	
	// --------
        // Pipeline stages
	//stage_1: fraction calculation
	rule stage_1;
		//dIn reads the values from input pipeline register 
      		let dIn = fifo_stage0_reg.first;  fifo_stage0_reg.deq;
		let div_out <- intDivide.inoutifc.response.get();
		// data to be stored in stored in fifo that will be used in stage 1
		//put an extra 0 infront of quotient because of the way the multiplier is designed
		match{.int_frac0,.carry0,.truncated_frac_msb0,.truncated_frac_zero0} = calculate_frac_int({1'b0,div_out.quotient},dIn.scale, div_out.truncated_frac_msb,div_out.truncated_frac_zero);
		//carry bit extended
		Bit#(CarryWidthQuire) carry = extend(carry0);
		//the Quire value is signed extend
		Bit#(QuireWidth) twos_complement_carry_int_frac = dIn.sign == 1'b0 ? {dIn.sign,carry,int_frac0} : {dIn.sign,twos_complement({carry,int_frac0})};
		//taking care of corner cases for zero infinity flag
		PositType zero_infinity_flag0 = twos_complement_carry_int_frac == 0 && dIn.ziflag == REGULAR ? ZERO :dIn.ziflag;
		let output_regf = Outputs_md {
		nan_flag : dIn.nan_flag,
		//also include the case when fraction bit msb = 0
		zero_infinity_flag : zero_infinity_flag0,
		quire_md : unpack(twos_complement_carry_int_frac),			
		truncated_frac_msb : truncated_frac_msb0,//zero_infinity_flag0 == ZERO ? 1'b0 : 
		truncated_frac_zero : truncated_frac_zero0};//zero_infinity_flag0 == ZERO ? 1'b1 :
		`ifdef RANDOM_PRINT
			$display("int_frac0 %b carry0 %h",int_frac0,carry0);
			$display("twos_complement_carry_int_frac %b",twos_complement_carry_int_frac);
		`endif
   		fifo_output_reg.enq(output_regf);
	endrule

interface Server inoutifc;
      interface Put request;
         method Action put (Inputs_md p);
		// stage_0: INPUT STAGE and scale calculation
		//dIn reads the values from input pipeline register 
      		let dIn = p;
		// data to be stored in stored in fifo that will be used in stage 0
		//see the corner cases due to zero infinity flag
		let ziflag = check_for_z_i_div(dIn.zero_infinity_flag1,dIn.zero_infinity_flag2);
		//to see what the hidden bit of each fraction bit will be thus sending that bit for product that can be seen as the two bits of zero flag
		let zero_flag = dIn.zero_infinity_flag1 == ZERO ? 2'b01 : ( dIn.zero_infinity_flag2 == ZERO ? 2'b10 : 2'b11);
		//calling function to get sum of scale
		let scale0 = calculate_sum_scale(dIn.scale1,-dIn.scale2);
		// calling function to get division of fractions
		intDivide.inoutifc.request.put (Input_intdiv{numerator : {zero_flag[1],dIn.frac1},denominator : {zero_flag[0],dIn.frac2}});

                let stage0_regf = Stage0_d {
			//taking care of corner cases for nan flag 
			nan_flag : check_for_nan_div(dIn.zero_infinity_flag1,dIn.zero_infinity_flag2,dIn.nanflag1,dIn.nanflag2),
			//also include the case when fraction bit msb = 0
			ziflag : ziflag,
			sign : dIn.sign1 ^ dIn.sign2,
			scale : scale0};
   		fifo_stage0_reg.enq(stage0_regf);
		`ifdef RANDOM_PRINT
			$display("zero_infinity_flag %b",stage0_regf.ziflag);
			$display("sign0 %b",stage0_regf.sign);
			$display("scale0 %h",scale0);
		`endif
	
   endmethod
      endinterface
      interface Get response = toGet (fifo_output_reg);
   endinterface
endmodule

endpackage: Divider_fda


