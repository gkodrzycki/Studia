module z52(
  input [31:0] i,
  output logic [31:0] o
);
  logic temp;
  integer x;
  

  always_comb begin
    o[31] = i[31];
    temp = i[31];
    for(x = 30; x >= 0; x = x - 1) begin
      o[x] = i[x] ^ temp;
      temp = o[x];
    end
  end
  
endmodule
