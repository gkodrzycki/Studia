module zad31(
  input  [7:0] a, b,
  input sub,
  output [7:0] o
);
  /*
  W tym zadaniu może lepiej opisać idee niż tłumaczyć się samym kodem. 

  Mamy napisane 4 funkcje 
    * ha -> half adder 
    * fa -> full adder
    * fa4 -> full adder dla 4 bitów
    * one_digit -> zwraca jedną cyfrę modulo 10

  O ile ha, fa i fa4 są raczej jasne, warto się skupić na one_digit 
  i samym początku poza wszelkimi funkcjami. 

  Pomysł jest prosty, skoro one_digit zwraca cyfrę mod 10 to możemy osobno
  dodać jedności i dziesiątki i mamy prawie gotowy wynik. 
  Możliwe żę od liczby dziesiątek będziemy musieli dodać/odjąć 1 (w zależności 
  czy pożyczyliśmy w czasie odejmowania, czy dodaliśmy coś co powstało)
  Rozpatrujemy więc oba przypadki, a sztuczka z maską final_mask powoduje, że
  bierzemy tylko interesujący nas wynik.

  To teraz do one_digit. 
  Rozwiązujemy tam odejmowanie korzystając dopełnienia do 10.
  a - b = a + comp(b) + 1.   
  Jeśli będziemy mieli overflow/ujemną liczbę wystarczy, że dodamy 6 do wyniku żeby
  go naprawić. Więc rozpatruję wszystkie 4 przypadki, a później przy pomocy masek wydobywam
  właściwy wynik 
  */

  function [1:0] ha(input X,Y);
    ha = ({X^Y, X&Y});
  endfunction

  function [1:0] fa(input X,Y,c);
    reg t, c1, c2, ss1;
    {t, c1} = ha(X, Y);
    {ss1, c2} = ha(t, c);
    fa = {ss1, c1 | c2};
  endfunction
  
  function [4:0] fa4(input [3:0] X, Y, input c);
    reg c1, c2, c3, c4, s1, s2, s3, s4;

    {s1, c1} = fa(X[0], Y[0], c); 
    {s2, c2} = fa(X[1], Y[1], c1);
    {s3, c3} = fa(X[2], Y[2], c2);
    {s4, c4} = fa(X[3], Y[3], c3);
    fa4 = {{s4, s3, s2, s1}, c4}; // Teraz tu jest dowolna 4 bitowa liczba % 16
  endfunction
  
  function [4:0] one_digit(input [3:0] X, Y, input c);
    reg carry1, carry2, testover1, testover2;
    reg [3:0] ssub, add;
    reg [4:0] extra_add, extra_sub, complement, good_add, good_sub, mask1, mask2, final_mask;
  
    final_mask = {c, c, c, c, c};
    {add, carry1} = fa4(X, Y, c);
    complement = fa4(~Y, 4'b1010, 1); 
    {ssub, carry2} = fa4(X, complement[4:1], 0);

    testover1 = (carry1 || add[3]&&(add[2] || add[1])); // 1 jeśli overflow
    testover2 = (carry2 || ssub[3]&&(ssub[2] || ssub[1])); // 1 jeśli overflow

    mask1 = {testover1, testover1, testover1, testover1, testover1};
    mask2 = {testover2, testover2, testover2, testover2, testover2};

    extra_add = fa4(add, 4'b0110, 0);
    extra_sub = fa4(ssub, 4'b0110, 0);

    good_add = ({carry1,add}&~mask1  | {carry1||extra_add[0],extra_add[4:1]}&mask1);
    good_sub = ({carry2,ssub}&~mask2 | {carry2||extra_sub[0],extra_sub[4:1]}&mask2);
  	one_digit = (good_sub&final_mask | good_add&~final_mask); 
  endfunction

  logic [4:0] temp_add, temp_sub;
  logic [3:0]digit1, digit2, fix_add, fix_sub, final_mask;
  logic carried1, carried2;
  
  assign final_mask = {sub, sub, sub, sub};
  
  assign {carried1, digit1} = one_digit(a[3:0], b[3:0], sub); // To ustawia poprawnie 1 cyfrę
  assign {carried2, digit2} = one_digit(a[7:4], b[7:4], sub); // To ustawia 2 cyfrę
  
  assign temp_add = one_digit(digit2, {3'b000, carried1}, 0); //możliwe że musimy dodać 1
  assign fix_add = temp_add[3:0];   					     	  
  
  assign temp_sub = one_digit(digit2, {3'b000, ~carried1}, 1); //możliwe że musimy odjąć 1
  assign fix_sub = temp_sub[3:0];
  
  assign o = {final_mask&fix_sub | ~final_mask&fix_add, digit1};
endmodule