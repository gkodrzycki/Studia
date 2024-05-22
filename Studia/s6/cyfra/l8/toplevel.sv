module z8(
  input clk, 
  input [15:0] d,
  input [1:0] sel,
  output reg [15:0] cnt, cmp, top,
  output out
);
    //licznika
  	//rejestru porównania
    //rejestru wartości szczytowej
    //układów porównujących

  always_ff @(posedge clk)
    if (sel == 2'b10) top <= d; //ładuj szczytowe
	  else if (sel == 2'b01) cmp <= d; //ładuj porównanie
  	
  //counter setup
  always_ff @(posedge clk) 
    if (sel == 2'b11) cnt <= d; //ładowanie licznika
  	else if (cnt >= top) cnt <= 16'b0; // >=top zerujemy 
    else cnt <= cnt + 1'b1; //zwykły increment licznika
  
  assign out = cnt < cmp;
endmodule