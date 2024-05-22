module memory(
  input logic clk,
  input logic [9:0] raddr, waddr,
  input logic [15:0] in,
  output logic [15:0] out
);
  logic [15:0] mem [0:999];

  assign out = mem[raddr];
  always_ff @(posedge clk)
    mem[waddr] <= in;
endmodule


module circuit(
  input nrst, step, push, 
  input logic [1:0] op,
  input logic [15:0] d,
  output logic [15:0] out,
  output logic [9:0] cnt
);
  
  logic [15:0] help_out, top;
  logic [ 9:0] help_cnt;

  assign out = nrst ? help_out : 0;
  assign cnt = nrst ? help_cnt : 0;
  
  memory h(step, cnt-2, cnt-1, out, top);
  
  always_ff @(posedge step or negedge nrst) begin
    if (!nrst) begin
      help_out <= 0;
      help_cnt <= 0;
    end
    else if(push) begin
      help_out <= d;
      help_cnt <= cnt + 1;
    end
    else if(op == 1 && cnt > 0)  
      help_out <= 0-out;
    else if(op == 2 && cnt > 1) begin  
      help_out <= out + top;
      help_cnt <= cnt - 1;
    end
    else if(op == 3 && cnt > 1)  begin
      help_out <= out * top;
      help_cnt <= cnt - 1;
    end
    else begin
      help_out <= out;
      help_cnt <= cnt;
    end
  end
endmodule

module z(
  input nrst, step, push, 
  input [1:0] op,
  input [15:0] d,
  output [15:0] out,
  output [9:0] cnt
);
  circuit x(nrst, step, push, op, d, out, cnt);
endmodule

