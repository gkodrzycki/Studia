module z9(
  input clk, nrst, door, start, finish, 
  output heat, light, bell
);
  logic [4:0] q;
  // {bell, open, pause, cook, closed}

  always_ff @(posedge clk or negedge nrst) begin
    if (!nrst) q <= 5'b00001; //stan niski resetuje
    else begin
      if(q[0]) begin  //closed
        q[0] <= door | (start & !door) ? 0 : 1; //closed
        q[3] <= door ? 1 : 0; //open
        q[1] <= start & !door ? 1 : 0; //cook
      end
      else if(q[1]) begin //cook
        q[1] <= door | (finish & !door) ? 0 : 1; //cook
        q[2] <= door ? 1 : 0; //pause
        q[4] <= finish & !door ? 1 : 0; //bell    
      end
      else if(q[2]) begin //pause
        q[2] <= door ? 1 : 0; //pause
        q[1] <= door ? 0 : 1; //cook
      end
      else if(q[3]) begin //open
        q[3] <= door ? 1 : 0; //open
        q[0] <= door ? 0 : 1; //closed   
      end
      else if(q[4]) begin //bell
        q[4] <= door ? 0 : 1; //bell
        q[3] <= door ? 1 : 0; //open  
      end
    end
  end
  // {bell, open, pause, cook, closed}
  logic [2:0]temp;
  always_comb casez(q)
      5'b????1 : temp = 3'b000; //closed/
      5'b???1? : temp = 3'b110; //cook/l&h 
      5'b??1?? : temp = 3'b010; //pause/l
      5'b?1??? : temp = 3'b010; //open/l
      5'b1???? : temp = 3'b001; //bell/b
      default : temp = 3'bxxx;
  endcase
  // heat, light, bell
  assign heat = temp[2];
  assign light = temp[1];
  assign bell = temp[0];
endmodule
