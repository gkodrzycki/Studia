module tff(output q, nq, 
           input t, clk, nrst);
  logic ns, nr, ns1, nr1, j, k;
  nand n1(ns, clk, j), n2(nr, clk, k),
  n3(q, ns, nq), n4(nq, nr,  q, nrst), n5(ns1,!clk, t, nq), 
  n6(nr1, !clk, t, q), n7(j, ns1, k), n8(k, nr1, j, nrst);
  endmodule

module z71(
  output [3:0]out,
  input  clk, nrst, step, down
);
  logic [3:0]prev;
  tff t1(out[0], prev[0], step ? 1'b0 : 1'b1, clk, nrst);
  tff t2(out[1], prev[1], step ? 1'b1 : (down ? prev[0] : out[0]), clk, nrst);
  tff t3(out[2], prev[2], step ? (down ? prev[1] : out[1]) : 
                                 (down ? (prev[0] & prev[1]) : 
                                         (out[0] & out[1])), clk, nrst);
  tff t4(out[3], prev[3], step ? (down ? prev[2] & prev[1] : 
                                         out[2] & out[1]) :  
                                 (down ? prev[0] & prev[1] & prev[2] : 
                                         out[0] & out[1] & out[2]), clk, nrst);
endmodule
