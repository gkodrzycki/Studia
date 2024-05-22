module dec2to4(
    input [1:0] i,
    input en,
    output logic [3:0] o //Add logic
);
    integer k;
    always_comb begin // Add begin
        o = 4'b0; // Init
        for (k = 0; k <= 3; k = k + 1)
          if (i == k)
                o[k] = en;
    end
endmodule

module dec3to8(
    input [2:0] i,
    output logic [7:0] o
);
   
    dec2to4 ans1(i[1:0],  i[2], o[7:4]);
    dec2to4 ans2(i[1:0], !i[2], o[3:0]);
    
endmodule