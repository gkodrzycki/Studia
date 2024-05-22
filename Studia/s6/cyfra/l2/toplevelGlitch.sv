module zad2(
  input [3:0] i,
  output o
);
  // chcemy true gdy 2 lub 3
  // false 0, 1, 4
  logic all, none, b01, b02, b03, b12, b13, b23, one;
  
  assign all  =  (i[0] && i[1] && i[2] && i[3]);
  assign none = ~(i[0] || i[1] || i[2] || i[3]);
  
  assign b01 = i[0] && i[1];
  assign b02 = i[0] && i[2];
  assign b03 = i[0] && i[3];
  
  assign b12 = i[1] && i[2];
  assign b13 = i[1] && i[3];
  
  assign b23 = i[2] && i[3];
  
  assign one = ((i[0] || i[1] || i[2] || i[3]) && ~(b01 || b02 || b03 || b12 || b13 || b23));
  
  assign o = ~(all || none || one);
endmodule
