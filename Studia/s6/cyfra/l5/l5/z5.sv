module z5(
    input [3:0] i,
  output logic [3:0] o
);
    always_comb
        case(i) 
            4'b0111: o = 4'b0001;
            4'b0110: o = 4'b0010;
            4'b0101: o = 4'b0011;
            4'b0100: o = 4'b0100;
            4'b1011: o = 4'b0101;
            4'b1010: o = 4'b0110;
            4'b1001: o = 4'b0111;
            4'b1000: o = 4'b1000;
            4'b1111: o = 4'b1001;
            default: o = 4'b0000;
        endcase
endmodule