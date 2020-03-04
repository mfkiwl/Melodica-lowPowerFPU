package PositCore;

// Library imports
import FIFOF        :: *;
import FIFO        :: *;
import GetPut       :: *;
import ClientServer :: *;

// Project imports
import Utils :: *;

import Normalizer_Types :: *;
import Posit_Numeric_Types :: *;
import Posit_User_Types :: *;
import FDP_PNE_Quire_PC :: *;
import FtoP_PNE_PC :: *;
import PtoF_PNE_PC :: *;
import PositToQuire_PNE_PC :: *;
import QuireToPosit_PNE_PC :: *;
import FloatingPoint :: *;

// Type definitions
typedef FloatingPoint#(11,52) FDouble;
typedef FloatingPoint#(8,23)  FSingle;

typedef union tagged {
   FDouble D;
   FSingle S;
   Bit #(PositWidth) P;
   } FloatU deriving(Bits,Eq);

typedef enum {PFDP, FCVT_P_S, FCVT_S_P, FCVT_P_Q, FCVT_Q_P} PositCmds
deriving (Bits, Eq, FShow);

interface PositCore_IFC;
   interface Server #(
      Tuple4 #(FloatU, FloatU, RoundMode, PositCmds),
      Tuple2 #(FloatU, FloatingPoint::Exception)) server_core;
endinterface

(* synthesize *)
module mkPositCore (PositCore_IFC);

	Reg #(Bit#(QuireWidth))  rg_quire       <- mkReg(0);
	FDP_PNE_Quire       fdp            <- mkFDP_PNE_Quire(rg_quire);	
	PositToQuire_PNE    ptoq            <- mkPositToQuire_PNE(rg_quire);
	QuireToPosit_PNE    qtop            <- mkQuireToPosit_PNE(rg_quire);	
	FtoP_PNE            ftop            <- mkFtoP_PNE;	
	PtoF_PNE            ptof            <- mkPtoF_PNE;	

	FIFO #(PositCmds) opcode <- mkFIFO;

	FIFO #(Tuple4 #(FloatU, FloatU, RoundMode, PositCmds)) ffI <- mkFIFO;
	FIFO #(Tuple2 #(FloatU, FloatingPoint::Exception)) ffO <- mkFIFO;
	
	rule rl_fdp(tpl_4(ffI.first) == PFDP);
		fdp.compute.request.put((InputTwoPosit{posit_inp1 : tpl_1(ffI.first).P,posit_inp2 : tpl_2(ffI.first).P}));
		opcode.enq(PFDP);
		ffI.deq;
	endrule
	
	rule rl_ptof(tpl_4(ffI.first) == FCVT_P_S);
		ptof.compute.request.put(tpl_1(ffI.first).P);
		opcode.enq(FCVT_P_S);
		ffI.deq;
	endrule

	rule rl_ftop(tpl_4(ffI.first) == FCVT_S_P);
		let a = tpl_1(ffI.first).S;
		Bit#(FloatWidth) f = {pack(a.sign),a.exp,a.sfd};
		ftop.compute.request.put(f);
		opcode.enq(FCVT_S_P);
		ffI.deq;
	endrule

	rule rl_ptoq(tpl_4(ffI.first) == FCVT_P_Q);
		ptoq.compute.request.put(tpl_1(ffI.first).P);
		opcode.enq(FCVT_P_Q);
		ffI.deq;
	endrule
	
	rule rl_qtop(tpl_4(ffI.first) == FCVT_Q_P);
		qtop.compute.request.put(?);
		opcode.enq(FCVT_Q_P);
		ffI.deq;
	endrule

	rule rl_out;
		let op = opcode.first;
		FloatingPoint::Exception excep = FloatingPoint::Exception{invalid_op : False, divide_0: False, overflow: False, underflow: False, inexact : False};
		//FloatU posit_out;
		if(op == PFDP)
			begin
				let a <- fdp.compute.response.get();
				FloatU posit_out = tagged P 0;
				ffO.enq(tuple2(posit_out,excep));
				opcode.deq;
			end
		else if(op == FCVT_P_Q)
			begin
				let a <- ptoq.compute.response.get();
				FloatU posit_out = tagged P 0;
				ffO.enq(tuple2(posit_out,excep));
				opcode.deq;
			end
		else if(op == FCVT_P_S)
			begin
				let out_pf <- ptof.compute.response.get();
				FSingle fs = FSingle{sign : unpack(msb(out_pf)), exp : (out_pf[valueOf(FloatExpoBegin):valueOf(FloatFracWidth)]), sfd : truncate(out_pf) };
				FloatU posit_out = tagged S fs;
				ffO.enq(tuple2(posit_out,excep));
				opcode.deq;
			end
		else if(op == FCVT_S_P)
			begin
				let out_pf <- ftop.compute.response.get();
				excep.invalid_op = out_pf.nan_flag == 1'b1;
				excep.overflow = out_pf.zero_infinity_flag == INF;
				excep.underflow = out_pf.zero_infinity_flag == ZERO && out_pf.rounding;
				excep.inexact = out_pf.rounding;
				FloatU posit_out = tagged P out_pf.out_posit;
				ffO.enq(tuple2(posit_out,excep));
				opcode.deq;
			end
		else if(op == FCVT_Q_P)
			begin
				let out_pf <- qtop.compute.response.get();
				excep.invalid_op = out_pf.nan_flag == 1'b1;
				excep.overflow = out_pf.zero_infinity_flag == INF;
				excep.underflow = out_pf.zero_infinity_flag == ZERO && out_pf.rounding;
				excep.inexact = out_pf.rounding;
				FloatU posit_out = tagged P out_pf.out_posit;
				ffO.enq(tuple2(posit_out,excep));
				opcode.deq;
			end
	
	endrule
interface server_core = toGPServer (ffI,ffO);

endmodule
endpackage