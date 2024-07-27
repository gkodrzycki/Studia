module memory(
  input logic clk, wr,
  input logic [9:0] raddr, waddr,
  input logic [15:0] in,
  output logic [15:0] out
);
  logic [15:0] mem [0:999];

  assign out = mem[raddr]; 
  always_ff @(posedge clk)
    if(wr) mem[waddr] <= in;  
endmodule


module calculator(
  input nrst, step, push, en, 
  input logic [2:0] op,
  input logic [15:0] d,
  output logic [15:0] out,
  output logic [9:0] cnt
);
  logic wr;
  logic [15:0] top;
  logic [ 9:0] raddr, waddr;

  memory h(step, wr, raddr, waddr, out, top);
  
  always_ff @(posedge step or negedge nrst) begin
    if (!nrst) begin
      out <= 0;
      cnt <= 0;
    end
    else if(en) begin
      if(push) begin
        out <= d;
        cnt <= cnt + 1;
      end else casez(op)
        //ustaw na 1 jeśli top > 0 wpp 0
      	0: out <= (out > 16'b0111111111111111 || out == 16'b0) ? 0 : 1; 
        1: out <= 0-out; //minus unarny
        2: if(cnt > 1) begin  //dodawanie
          	out <= out + top; 
          	cnt <= cnt - 1;
        end
        3: if(cnt > 1) begin //mnożenie
        	out <= out * top;
        	cnt <= cnt - 1;
        end
        4: out <= top; //swap
        5: out <= top; //load
        default: begin
          cnt <= cnt - 1;
          out <= top;
        end
      endcase
    end
  end

   always_comb begin
     raddr = cnt - 1;
     waddr = cnt;
     
      if(push) begin
        if(cnt == 0) raddr = cnt;
        else raddr = cnt - 1;
      end
      else if(op == 4 && cnt > 1) begin //swap
        raddr = cnt - 1;
        waddr = cnt - 1;
      end
      else if(op == 5) //zamień top na na ity element, gdzie i to wartość topa
        raddr = cnt - out[9:0] - 1;
     
     wr = (push && cnt > 0) || (op == 4);
   end
endmodule


module control(
  input logic clk, nrst, wr, start,
  input logic [9:0] addr,
  input logic [15:0] datain,
  output logic ready,
  output logic [15:0] out
);
  
  logic en, push;
  logic [2:0] op;
  logic [9:0] pc;
  logic [15:0] d;

  logic prog_wr;
  logic [9:0] prog_waddr;
  logic [15:0] prog_data, p_out;

  memory prog_mem(clk, prog_wr, pc, prog_waddr, prog_data, p_out);
  calculator calc_inst(nrst, clk, push, en, op, d, out,);

  always_ff @(posedge clk or negedge nrst) begin
    if (!nrst) begin
      ready <= 1;
    end 
    else begin
      if (ready) begin
        if(start) begin
          pc <= 0;
          ready <= 0;
        end else ready <= 1;  
      end
      else begin
        ready <= 0;
        if(p_out[15]) begin
          if(!p_out[14]) begin
            if(p_out[2:0] == 7) pc <= out;
            else pc <= pc + 1;
          end
          else ready <= 1;
        end else pc <= pc + 1;
      end
    end
  end

  always_comb begin
    if (!nrst) begin 
      en = 0;
      push = 0;
      en = 0;
      op = 0;
      d = 0;
      prog_wr = 0;
      prog_waddr = 0;
      prog_data = 0;
    end
    else if(ready && !start && wr) begin
        prog_data = datain;
        prog_waddr = addr;
        prog_wr = 1;
        push = 0;
        en = 0;
        op = 0;
        d = 0;
    end
    else begin
      prog_wr = 0;
      prog_waddr = 0;
      prog_data = 0;
      if(p_out[15]) begin
          push = 0;
          if(p_out[14]) begin
            en = 0;
            op = 0;
            d = 0;
          end else begin 
            en = 1;
            op = p_out[2:0];
            d = 0;
          end
        end 
      else begin 
        en = 1;
        push = 1;
        d = p_out;
        op = 0;
      end
    end
  end
endmodule

module control_module(
  input logic clk, nrst, wr, start,
  input logic [9:0] addr,
  input logic [15:0] datain,
  output logic ready,
  output logic [15:0] out
);
   control x(clk, nrst, wr, start, addr, datain, ready, out);
endmodule
