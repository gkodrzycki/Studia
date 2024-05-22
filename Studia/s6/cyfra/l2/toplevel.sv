module zad2(
  input [0:3] i,
  output o
);
  // chcemy true gdy 2 lub 3
  // false 0, 1, 4
  // Rozpisanie mapy Karnaugh
  logic h, v, p;
  
  assign v =  (~i[0] && ~i[1]) && ~( i[2] &&  i[3]);
  assign h = ~( i[0] &&  i[1]) &&  (~i[2] && ~i[3]);
  assign p =  ( i[0] &&  i[1]  &&    i[2] &&  i[3]);
          
  assign o = ~(h || v || p);
endmodule