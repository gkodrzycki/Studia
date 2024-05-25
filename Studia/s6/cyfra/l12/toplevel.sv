module selection_sort(
    input clk,
    input nrst,     
    input start,
    input wr,
    input [2:0] addr,
    input [7:0] datain,
    output logic [7:0] dataout,
    output logic ready
);
    logic [7:0] d [0:7];  
  	logic [3:0] i, j, jm;
    logic [7:0] c, m;
    logic sorting;

    always @(posedge clk or negedge nrst) begin
        if (!nrst) begin
            ready <= 1;
            sorting <= 0;
            i <= 0;
            j <= 0;
            jm <= 0;
            c <= 0;
            m <= 0;
        end else begin
            if (ready && start) begin
                ready <= 0;
                sorting <= 1;
                i <= 0;
            end else if (sorting) begin
              if (i < 8) begin
                    if (j == 0) begin
                        j <= i + 1;
                        jm <= i;
                        m <= d[i];
                        c <= d[i + 1];
                    end else if (j < 8) begin
                        if (c < m) begin
                            m <= c;
                            jm <= j;
                        end
                        j <= j + 1;
                        c <= d[j + 1];
                    end else begin
                        if (i != jm) begin
                            d[jm] <= d[i];
                            d[i] <= m;
                        end
                        i <= i + 1;
                        j <= 0;
                    end
                end else begin
                    ready <= 1;
                    sorting <= 0;
                end
            end else if (ready) begin
                if (wr) begin
                    d[addr] <= datain;
                end else begin
                    dataout <= d[addr];
                end
            end
        end
    end
endmodule


module cir(
    input clk,
    input nrst,
    input start,
    input wr,
    input [2:0] addr,
    input [7:0] datain,
    output logic [7:0] dataout,
    output logic ready
);
  selection_sort x(clk, nrst, start, wr, addr, datain, dataout, ready);
endmodule
