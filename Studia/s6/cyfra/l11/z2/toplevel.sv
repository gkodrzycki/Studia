module ctlpath(
  input clk, nrst, start,
  input eq,
  output logic ready,
  output logic s
);
  const logic READY = 1'b0;
  const logic BUSY  = 1'b1;

  always_ff @(posedge clk or negedge nrst) begin 
    if(!nrst) begin
      ready <= 1'b1;
      s <= READY;
    end else case (s)
      READY: if(start) begin
            s <= BUSY;
            ready <= 1'b0;
          end
      BUSY: if(eq) begin
            ready <= 1'b1;
            s <= READY;
          end
    endcase
  end
endmodule

module datapath(
  input clk, nrst,
  input logic [7:0] ina, inb,
  input logic s,
  output logic [7:0] a, b,
  output logic [7:0] out
);
  const logic READY = 1'b0;
  const logic BUSY  = 1'b1;

  always_ff @(posedge clk or negedge nrst) begin 
    if(!nrst) begin
      a <= 8'b0;
      b <= 8'b0;
    end else case (s)
      READY: begin
            a <= ina;
            b <= inb;
          end 
      BUSY: if (a == b)
        		out <= a;
      		else if (a >= b)
        		a <= a - b;
      		else begin
              a <= b;
              b <= a;
            end
    endcase
  end 
endmodule

module zad2(
  input clk, nrst, start,
  input [7:0] ina, inb,
  output logic ready,
  output logic [7:0] out
);
  logic [7:0] a, b;
  logic s;
  
  ctlpath ctl(clk, nrst, start, a == b, ready, s);
  datapath data (clk, nrst, ina, inb, s, a, b, out);
endmodule

module circ(
  input clk, nrst, start,
  input [7:0] ina, inb,
  output logic ready,
  output logic [7:0] out
);
  zad2 test(clk, nrst, start, ina, inb, ready, out);
endmodule
