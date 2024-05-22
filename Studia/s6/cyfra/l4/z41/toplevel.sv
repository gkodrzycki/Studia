module z41(
  	input [3:0] i,
	input l, r,
    output [3:0] o
);
  
  logic isneutral;
  
  nor(isneutral, l, r);
  
  assign o[0] = (     1'b0) || (r && i[1]) || (isneutral && i[0]);
  assign o[1] = (l && i[0]) || (r && i[2]) || (isneutral && i[1]);
  assign o[2] = (l && i[1]) || (r && i[3]) || (isneutral && i[2]);
  assign o[3] = (l && i[2]) || (     1'b0) || (isneutral && i[3]);
endmodule