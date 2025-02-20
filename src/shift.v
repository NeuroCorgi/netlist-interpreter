module self_ref_bitvector (
    input wire clk,
    input wire rst,
    input wire serial_in,
    output reg [7:0] data_out
);

always @(posedge clk or posedge rst) begin
    if (rst) 
        data_out <= 8'b0; // Reset the bitvector
    else
        data_out <= {data_out[6:0], serial_in}; // Self-referential update
end

endmodule
