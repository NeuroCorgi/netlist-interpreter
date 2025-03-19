module loop(clk,rst, d);

   input clk;
   input rst;
   output[1:0] d;
		
   reg	  r1;
   reg	  r2;

   assign d = {r1, r2};

   always@(posedge clk or posedge rst) begin
	  if (rst)
		 r1 <= 1'b0;
	  else
		r1 <= r2;
   end

   always@(posedge clk or posedge rst) begin
	  if (rst)
		r2 <= 1'b1;
	  else
		r2 <= r1;
   end
   
endmodule
