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
// THE SOFTWARE.package Extracter_Types;
package QtoP_Tb;

// -----------------------------------------------------------------
// This package defines:
//
//    mkTestbench      : Pipeline level testbench for the PE sub-
//                     pipelines. These testbenches are meant to
//                     also run on FPGA when compiled with the
//                     -D FPGA flag
//
// -----------------------------------------------------------------

import ClientServer     :: *;
import GetPut           :: *;
import FIFO             :: *;
import LFSR             :: *;
import Posit_Numeric_Types :: *;
import Posit_User_Types :: *;
import Normalizer_Types :: *;
import QuireToPosit ::*;
import QtoP_Types	:: *;
import QuireToPosit_PNE ::*;
import "BDPI" quireToPosit16  = function Bit#(PositWidth) checkoperation (Bit#(64) in1,Bit#(64) in1)	;
`ifdef FPGA
interface FpgaLedIfc;
(* always_ready *)
method Bool chkComplete;
(* always_ready *)
method Bool completeWithErrors;
endinterface
`endif

// Number of random tests to be run
`ifdef P8
typedef 255 Num_Tests;
`elsif P16
typedef 1024 Num_Tests;
`elsif P32
typedef 4096 Num_Tests;
`endif

typedef 20 Pipe_Depth;      // Estimated pipeline depth of the PNE

// -----------------------------------------------------------------


//
// Module definition
(* synthesize *)
`ifdef FPGA
module mkTestbench (FpgaLedIfc);
`else
module mkTestbench (Empty);
`endif
// Depending on which input mode we are using, the input to the DUT will be
// from a LFSR or from a counter. The LFSR is always sized to the maximal size
`ifdef RANDOM
LFSR  #(Bit #(32))            lfsr1          <- mkLFSR_32;
LFSR  #(Bit #(32))            lfsr2          <- mkLFSR_32;
LFSR  #(Bit #(32))            lfsr11          <- mkLFSR_32;
LFSR  #(Bit #(32))            lfsr22          <- mkLFSR_32;
Reg   #(Bool)                 rgSetup        <- mkReg (False);
`endif

Reg   #(Bool)                 rgGenComplete  <- mkReg (False);

Reg   #(Bit #(QuireWidthBy2))   rgCurInput     <- mkReg (0);
Reg   #(Bit #(QuireWidthBy2))   rgCurInput1     <- mkReg (0);

FIFO  #(Bit #(QuireWidthBy2))   ffInputVals    <- mkSizedFIFO (valueOf (
                                                   TAdd# (Pipe_Depth,2)));
FIFO  #(Bit #(QuireWidthBy2))   ffInputVals1    <- mkSizedFIFO (valueOf (
                                                   TAdd# (Pipe_Depth,2)));
Reg   #(Bit#(TAdd#(PositWidth,PositWidth)))   wrongOut    <- mkReg (0);
Reg   #(Bit #(QuireWidthBy2))   rgCurOutput    <- mkReg (0);
Reg   #(Bit #(QuireWidthBy2))   rgCurOutput1    <- mkReg (0);
Reg   #(Bool)                 rgChkComplete  <- mkReg (False);
Reg   #(Bool)                 rgError        <- mkReg (False);

QuireToPosit_PNE            dut            <- mkPNE_test;	
Reg #(Bool) doneSet <-mkReg(False);
// -----------------------------------------------------------------

rule lfsrGenerate(!doneSet);
`ifdef RANDOM
	lfsr1.seed('h03);// to create different random series
	lfsr2.seed('h02);// to create different random series
	lfsr11.seed('h03);// to create different random series
	lfsr22.seed('h02);// to create different random series
`endif
	doneSet<= True;
endrule

rule rlGenerate (!rgGenComplete && doneSet);
`ifdef RANDOM
   // Drive input into DUT
   //let inPosit11 = 64'h0000000000000000;
   //let inPosit22 = 64'h0100111111000001;
   Bit#(64) inPosit11 = {lfsr1.value(),lfsr11.value()};
   Bit#(64) inPosit22 = {lfsr2.value(),lfsr22.value()};
   dut.compute.request.put ({inPosit11,inPosit22});

   // Bookkeeping
   rgCurInput <= rgCurInput + 1;
   ffInputVals.enq (truncate (inPosit11));
   ffInputVals1.enq (truncate (inPosit22));
   // Prepare LFSR for the next input
   lfsr1.next ();
   lfsr2.next ();
   lfsr11.next ();
   lfsr22.next ();
   // Completion of test generation

   rgGenComplete <= ((rgCurInput + 1) == fromInteger (valueOf (Num_Tests)));

`else
   // Drive input into DUT
   dut.compute.request.put ({rgCurInput,rgCurInput1});
  // dut.compute.request.put (rgCurInput);
   // Prepare for next input
	ffInputVals.enq (rgCurInput);
	ffInputVals1.enq (rgCurInput1);
	   if((rgCurInput1 + 1) == 0)
		begin
			rgCurInput1 <= 0;
			rgCurInput <= rgCurInput + 1;
		end
		else
		begin
			rgCurInput1 <= rgCurInput1 + 1;
		end
		
	   // Completion of test generation
	   rgGenComplete <= ((rgCurInput + 1) == 0 && (rgCurInput1 + 1) == 0);
`endif
endrule



// --------
//rule rlCheck (!rgChkComplete && !rgError);
rule rlCheck (!rgChkComplete && doneSet );
      let rsp <- dut.compute.response.get ();
	let input1_c = ffInputVals.first; ffInputVals.deq;
	let input2_c = ffInputVals1.first; ffInputVals1.deq;
	let expected = checkoperation(input1_c,input2_c);
   `ifdef RANDOM
      
      // Detected an error
      if (rsp.out_posit != expected) begin
         $display ("[%0d]::ERR::Input=%b::Input2=%b::Expected Output=%b::Output=%b", $time, input1_c,input2_c,expected, rsp.out_posit);
         rgError <= True;
	 wrongOut <= wrongOut+1;
         
      end
      
         rgCurOutput <= rgCurOutput + 1;

         // Completion condition
         rgChkComplete <= ((rgCurOutput + 1) == fromInteger (valueOf (Num_Tests)));
     // end

   `else
      //let expected = rgCurOutput;

      // Detected an error
      if (rsp.out_posit != expected) begin
         $display ("[%0d]::ERR::Input=%b::Input2=%b::Expected Output=%b::Output=%b", $time, input1_c,input2_c,expected, rsp.out_posit);
         rgError <= True;
	 wrongOut <= wrongOut+1;
      end
	if((rgCurOutput1 + 1) == 0)
		begin
			rgCurOutput1 <= 0;
			rgCurOutput <= rgCurOutput + 1;
		end
		else
		begin
			rgCurOutput1 <= rgCurOutput1 + 1;
	end

         // Completion condition
         rgChkComplete <= ((rgCurOutput + 1) == 0);
      //end
   `endif
   endrule



// --------
//rule rlFinish (rgError || rgChkComplete);
rule rlFinish ( rgChkComplete && doneSet);
	$display ("%d",wrongOut);
   if (!rgError) $display ("[%0d]::INF::No errors found.", $time);
	else $display ("[%0d]::INF::with errors found.", $time);
   $finish;
endrule


// -----------------------------------------------------------------

//
// Interfaces
`ifdef FPGA
method Bool chkComplete = rgChkComplete;
method Bool completeWithErrors = rgError;
`endif

// -----------------------------------------------------------------

endmodule
endpackage

// -----------------------------------------------------------------


