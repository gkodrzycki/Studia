module d_latch(output q, nq, input en, d);
    logic nr, ns;
    nand gq(q, nr, nq);
    nand gnq(nq, ns, q);
    nand gr(nr, d, en);
    nand gs(ns, nr, en);
endmodule

module dff_ms(
  input clk, d,
  output q
);
  logic q1;
  d_latch dl1(q1, , !clk, d );
  d_latch dl2(q , ,  clk, q1);
endmodule

// Powyżej kod ze slajdów 

module multi(
  input l, r, i0, i1, i2, i3,
  output o
);
  assign o = (l & r) ? i3 : 
                  r  ? i1 : 
                  l  ? i2 : i0;
endmodule

module z61(
  output [7:0] q,
  input [7:0] d,
  input i, c, l, r
);
  logic [7:0] q_intermediate;
  
  dff_ms dff0(c, q_intermediate[0], q[0]);
  dff_ms dff1(c, q_intermediate[1], q[1]);
  dff_ms dff2(c, q_intermediate[2], q[2]);
  dff_ms dff3(c, q_intermediate[3], q[3]);
  dff_ms dff4(c, q_intermediate[4], q[4]);
  dff_ms dff5(c, q_intermediate[5], q[5]);
  dff_ms dff6(c, q_intermediate[6], q[6]);
  dff_ms dff7(c, q_intermediate[7], q[7]);
  
  multi multi_output0(l, r, q[0],    i, q[1], d[0], q_intermediate[0]);
  multi multi_output1(l, r, q[1], q[0], q[2], d[1], q_intermediate[1]);
  multi multi_output2(l, r, q[2], q[1], q[3], d[2], q_intermediate[2]);
  multi multi_output3(l, r, q[3], q[2], q[4], d[3], q_intermediate[3]);
  multi multi_output4(l, r, q[4], q[3], q[5], d[4], q_intermediate[4]);
  multi multi_output5(l, r, q[5], q[4], q[6], d[5], q_intermediate[5]);
  multi multi_output6(l, r, q[6], q[5], q[7], d[6], q_intermediate[6]);
  multi multi_output7(l, r, q[7], q[6],    i, d[7], q_intermediate[7]);
endmodule
