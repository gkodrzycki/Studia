module funnel_shifter(
  input [7:0] a, b,
  input [3:0] n,
  output [7:0] o
);
  
  logic [15:0] combined, shifted;
  
  assign combined = {a, b};
  
  assign shifted = combined >> n;
  assign o = shifted[7:0];
endmodule

module zad42(
  input [7:0] i,
  input [3:0] n,
  input ar, lr, rot,
  output [7:0] o
);
  
  logic [7:0] l, r, ll, rr, max;
  
  assign max = ar ? (lr ? 8'b00000000 : (i[7] ? 8'b11111111 : 8'b00000000)) : 8'b00000000;
  
  assign l = lr ? i : max;
  assign r = lr ? max : i;
  
  logic [3:0] newn;
  assign newn = lr ? (8 - n) : n; // w prawo to w 8-n w lewo
  
  assign ll = rot ? i : l;
  assign rr = rot ? i : r;
  
  funnel_shifter ss(ll,rr,newn,o);
  
endmodule