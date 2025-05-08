module topEntity(a, b, c, d);
   parameter WIDTH = 8;

   input [WIDTH - 1:0] a;
   input [WIDTH - 1:0] b;
   output [WIDTH - 1:0]	c;
   output [WIDTH - 1:0]	d;
   
   assign c = a + b;
   assign d = 10;
   
endmodule // topEntity

