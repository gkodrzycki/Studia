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