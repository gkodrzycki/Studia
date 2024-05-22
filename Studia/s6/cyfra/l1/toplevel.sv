module zad1(
  input x, y, a, b, c, d,
  output o
);
  logic ha, hb, hc, hd;
  
  assign ha = (~x && ~y && a);
  assign hb = (~x &&  y && b);
  assign hc = ( x && ~y && c);
  assign hd = ( x &&  y && d);
  
  assign o = (ha || hb || hc || hd);
endmodule
