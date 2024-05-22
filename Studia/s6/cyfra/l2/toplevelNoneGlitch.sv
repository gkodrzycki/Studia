module zad2(
  input [0:3] i,
  output o
);
  // chcemy true gdy 2 lub 3
  // false 0, 1, 4
  // Rozpisanie mapy Karnaugh
  logic h1, h2, h3, v1, v2, v3;
  
  assign v1 = (~i[0] &&  i[1]) &&  (i[2] || i[3]);
  assign v2 = ( i[0] &&  i[1]) && ~(i[2] && i[3]);
  assign v3 = ( i[0] && ~i[1]) &&  (i[2] || i[3]);

  assign h1 = (~i[2] &&  i[3]) &&  (i[0] || i[1]);
  assign h2 = ( i[2] &&  i[3]) && ~(i[0] && i[1]);
  assign h3 = ( i[2] && ~i[3]) &&  (i[0] || i[1]);
    
  assign o = (h1 || h2 || h3 || v1 || v2 || v3);
endmodule