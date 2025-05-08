module topEntity(clk, a, b, c, d, e, f);
   parameter width = 64;

   input clk;
   input [width - 1:0]	a;   
   input [width - 1:0]	b;
   input [width - 1:0] c;

   output reg [width - 1:0] d = 0;
   output reg [width - 1:0] e = 0;
   output reg [width - 1:0] f = 0;

   always@(posedge clk) begin
	  if (clk) begin
		 d = a;
		 e = b;
		 f = c;
	  end;
   end;

endmodule // topEntity
