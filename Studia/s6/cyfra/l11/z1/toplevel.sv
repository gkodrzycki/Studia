module multi(
  input [15:0] a, b,
  output [15:0] o
);
  assign o = a*b;
endmodule

module zad1(
  input clk, nrst, start,
  input [15:0] inx,
  input [7:0] inn,
  output logic ready,
  output logic [15:0] out
);
  const logic READY = 1'b0;
  const logic BUSY  = 1'b1;
  logic [15:0] a, x, mult_a, mult_b, mult_res;
  logic [ 7:0] n;
  logic s;

  multi m1(mult_a, mult_b, mult_res);
  
  always_ff @(posedge clk or negedge nrst) begin 
    if(!nrst) begin
      ready <= 1'b1;
      s <= READY;
    end else case (s)
      READY: if(start) begin
          a <= 16'b1;
          x <= inx;
          n <= inn;
          ready <= 1'b0;
          s <= BUSY;
          mult_a <= inn[0] ? 16'b1 : inx;
          mult_b <= inx;
        end
      BUSY: if(n == 0) begin
          out <= a;
          ready <= 1'b1;
          s <= READY;
        end
        else begin
          if(!n[0]) begin
            x <= mult_res;
            mult_a <= n[1] ? a : mult_res;
            mult_b <= mult_res;
            n <= n>>1;
          end else begin
            a <= mult_res;
            mult_a <= x;
            mult_b <= x;
            n <= n - 1;
          end
        end
    endcase
  end 
endmodule

module circ(
  input clk, nrst, start,
  input [15:0] inx,
  input [7:0] inn,
  output logic ready,
  output logic [15:0] out
);
  zad1 test(clk, nrst, start, inx, inn, ready, out);
endmodule
