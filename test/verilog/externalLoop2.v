module topEntity (a, b, c, d, e);
   input [63:0] a;
   input [63:0]	b;

   output [63:0] c;
   output [63:0] d;
   output [63:0] e;

   assign c = 42;
   assign d = a;
   assign e = b;

endmodule // topEntity
