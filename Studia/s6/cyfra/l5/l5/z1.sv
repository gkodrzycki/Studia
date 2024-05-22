module z1(
    input [7:0] i,
    input [2:0] x,
    output logic [7:0] o
);
    logic [7:0] a,b,c,d;

    assign a = i; //1
    assign b = i << 1; //2 
    assign c = (i << 1) + i; //3
    assign d = i << 2; //4
    always_comb
        case(x)
            3'b001: o = a;
            3'b010: o = b;
            3'b011: o = c;
            3'b100: o = d;
            default: o = 8'b0; 
        endcase 
endmodule

