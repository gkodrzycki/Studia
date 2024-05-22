module zad32(
  input  [15:0] a, b,
  output [15:0] o 
);
  
  function [5:0] block(
    input [3:0] x, y,
    input c0,
  	);
    
    reg p0, p1, p2, p3, g0, g1, g2, g3, c1, c2, c3, P, G;
    reg [3:0] s;
    
    g0 = x[0] && y[0];
    p0 = x[0] || y[0];
    s[0] = x[0] ^ y[0] ^ c0; 
    
    g1 = x[1] && y[1];
    p1 = x[1] || y[1];
    c1 = g0 || p0 && c0;
    s[1] = x[1] ^ y[1] ^ c1;
    
    g2 = x[2] && y[2];
    p2 = x[2] || y[2];
    c2 = p0 & p1 & c0 | p1 & g0 | g1;
    s[2] = x[2] ^ y[2] ^ c2;
    
    g3 = x[3] && y[3];
    p3 = x[3] || y[3];
    c3 = p0 & p1 & p2 & c0 | p1 & p2 & g0 | p2 & g1 | g2;
    s[3] = x[3] ^ y[3] ^ c3;
    
    P = p0 && p1 && p2 && p3;
    G = (g0 && p1 && p2 && p3) || (g1 && p2 && p3) || (g2 && p3) || g3;
    block = {P, G, s};
    
  endfunction
  
  logic P0, P1, P2, P3, G0, G1, G2, G3, c8, c12, c16;
  
  assign {P0, G0, o[3:0]} = block(a[3:0], b[3:0], 0); 	
  assign {P1, G1, o[7:4]} = block(a[7:4], b[7:4], G0); 
  assign c8 = G0 && P1 || G1 ;
  
  assign {P2, G2, o[11:8]} = block(a[11:8], b[11:8], c8);
  assign c12 = (G0 && P1 && P2) || (G1 && P2) || G2;
  
  assign {P3, G3, o[15:12]} = block(a[15:12], b[15:12], c12);
  
endmodule