module z51(
  input [15:0] i,
  output [15:0] o //[15:12] >= [11:8] >= [7 :4] >= [3 :0]
);
  logic [15:0] phase_1, phase_2;
  //Implementacja prostej sieci sortującej 
  
  always_comb begin
    //First swaps
    if(i[3:0] > i[11:8]) begin
      phase_1[3:0] = i[11:8];
      phase_1[11:8] = i[3:0];
    end
    else begin
      phase_1[3:0] = i[3:0];
      phase_1[11:8] = i[11:8];
    end
    
    if(i[7:4] > i[15:12]) begin
      phase_1[7:4] = i[15:12];
      phase_1[15:12] = i[7:4];
    end
    else begin
      phase_1[7:4] = i[7:4];
      phase_1[15:12] = i[15:12];
    end
    
    //Second swaps
    if(phase_1[3:0] > phase_1[7:4]) begin
      phase_2[3:0] = phase_1[7:4];
      phase_2[7:4] = phase_1[3:0];
    end
    else begin
      phase_2[3:0] = phase_1[3:0];
      phase_2[7:4] = phase_1[7:4];
    end
    
    if(phase_1[11:8] > phase_1[15:12]) begin
      phase_2[11:8] = phase_1[15:12];
      phase_2[15:12] = phase_1[11:8];
    end
    else begin
      phase_2[11:8] = phase_1[11:8];
      phase_2[15:12] = phase_1[15:12];
    end
    
    //Final 
    if(phase_2[7:4] > phase_2[11:8]) begin
      o[7:4] = phase_2[11:8];
      o[11:8] = phase_2[7:4];
    end 
    else begin
      o[7:4] = phase_2[7:4];
      o[11:8] = phase_2[11:8];
    end 
    o[3:0] = phase_2[3:0];
    o[15:12] = phase_2[15:12];
  end
endmodule