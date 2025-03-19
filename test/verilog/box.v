/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.8.2. DO NOT MODIFY.
*/
`default_nettype none
`timescale 100fs/100fs
module topEntity
    ( // Inputs
      input wire [15:0] vec


      // Outputs
    , output wire [15:0] result_0
    , output wire [7:0] result_1
    , output wire  result_2_0
    , output wire  result_2_1
    , output wire  result_2_2
    , output wire  result_2_3
    , output wire  result_2_4
    , output wire  result_2_5
    , output wire  result_2_6
    , output wire  result_2_7
    );
  wire [15:0] ds;
  wire [7:0] result__dc_arg_res;
  wire [7:0] c$vec;
  wire [31:0] result;
  wire [7:0] result_2_8;

  assign ds = vec;

  assign c$vec = (8'b11110000);

  // map begin
  genvar i;
  generate
  for (i=0; i < 8; i = i + 1) begin : map
    wire [0:0] map_in;
    assign map_in = c$vec[i*1+:1];
    wire  map_out;
    assign map_out = map_in;


    assign result__dc_arg_res[i*1+:1] = map_out;
  end
  endgenerate
  // map end

  assign result = {({(((((ds[15:8]))))),(((((ds[7:0])))))}),
                   (({1'b0,   1'b0,   1'b0,   1'b0,   1'b1,   1'b1,   1'b1,   1'b1})),
                   result__dc_arg_res};

  assign result_0 = result[31:16];

  assign result_1 = result[15:8];

  assign result_2_8 = result[7:0];

  assign result_2_0 = result_2_8[7:7];

  assign result_2_1 = result_2_8[6:6];

  assign result_2_2 = result_2_8[5:5];

  assign result_2_3 = result_2_8[4:4];

  assign result_2_4 = result_2_8[3:3];

  assign result_2_5 = result_2_8[2:2];

  assign result_2_6 = result_2_8[1:1];

  assign result_2_7 = result_2_8[0:0];


endmodule

