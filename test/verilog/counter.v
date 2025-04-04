/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.8.2. DO NOT MODIFY.
*/
`default_nettype none
`timescale 100fs/100fs
module topEntity
    ( // Inputs
      input wire  clk // clock
    , input wire  rst // reset
    , input wire  en // enable

      // Outputs
    , output wire signed [63:0] result
    );
  wire signed [63:0] ds;
  wire signed [63:0] c$ds_app_arg;
  wire [63:0] \bus' ;
  wire [63:0] result_1;
  wire [63:0] c$ds_app_arg_0;
  wire signed [63:0] c$ds_app_arg_1;
  wire  c$app_arg;
  wire signed [63:0] c$app_arg_0;
  wire [63:0] c$app_arg_1;
  wire signed [63:0] c$ds_app_arg_2;
  reg signed [63:0] c$ds_case_alt;
  wire [64:0] c$ds_app_arg_3;
  reg [64:0] ds_0 = {1'b1,   64'sd0};
  wire  write;
  wire signed [63:0] prevread;
  wire signed [63:0] x1;
  wire signed [63:0] ds_1;
  wire signed [63:0] a1;
  wire signed [63:0] i;
  wire [63:0] c$ds_app_arg_4;
  wire signed [63:0] c$ds_app_arg_5;
  wire  c$app_arg_2;
  wire signed [63:0] c$app_arg_3;
  wire [63:0] c$app_arg_4;
  wire signed [63:0] ds_2;
  wire signed [63:0] a1_0;
  wire signed [63:0] i_0;
  wire signed [63:0] c$ds_app_arg_6;
  reg signed [63:0] c$ds_case_alt_0;
  wire [64:0] c$ds_app_arg_7;
  reg [64:0] ds_3 = {1'b0,   64'sd0};
  wire  write_0;
  wire signed [63:0] prevread_0;
  wire signed [63:0] x1_0;

  assign ds = $signed(c$ds_app_arg);

  assign c$ds_app_arg = $unsigned(result_1[0+:64]);



  // readFromBiSignal begin
  assign result_1 = \bus' ;
  // readFromBiSignal end

  // readFromBiSignal begin
  assign c$ds_app_arg_0 = \bus' ;
  // readFromBiSignal end

  assign c$ds_app_arg_1 = $unsigned(c$ds_app_arg_0[0+:64]);

  assign c$app_arg = c$ds_app_arg_3[64:64] ? 1'b1 : 1'b0;

  assign c$app_arg_0 = i;

  assign c$app_arg_1 = c$ds_app_arg_3[64:64] ? ($unsigned(c$app_arg_0[0+:64])) : ({64 {1'bx}});

  // writeToBiSignal# begin
  assign \bus'  = (c$app_arg == 1'b1) ? (c$app_arg_1) : {64 {1'bz}};
  // writeToBiSignal# end

  assign c$ds_app_arg_2 = write ? prevread : (ds_1);

  always @(*) begin
    case(x1)
      64'sd9223372036854775807 : c$ds_case_alt = {64 {1'bx}};
      default : c$ds_case_alt = (x1 + 64'sd1);
    endcase
  end

  assign c$ds_app_arg_3 = write ? {1'b1,c$ds_case_alt} : {1'b0,64'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx};

  // register begin
  always @(posedge clk or  posedge  rst) begin : ds_0_register
    if ( rst) begin
      ds_0 <= {1'b1,   64'sd0};
    end else if (en) begin
      ds_0 <= {~ write,   c$ds_app_arg_2};
    end
  end
  // register end

  assign write = ds_0[64:64];

  assign prevread = $signed(ds_0[63:0]);

  assign x1 = prevread;

  assign ds_1 = $signed(c$ds_app_arg_1);

  assign a1 = $signed(c$ds_app_arg_3[63:0]);

  assign i = a1;

  // readFromBiSignal begin
  assign c$ds_app_arg_4 = \bus' ;
  // readFromBiSignal end

  assign c$ds_app_arg_5 = $unsigned(c$ds_app_arg_4[0+:64]);

  assign c$app_arg_2 = c$ds_app_arg_7[64:64] ? 1'b1 : 1'b0;

  assign c$app_arg_3 = i_0;

  assign c$app_arg_4 = c$ds_app_arg_7[64:64] ? ($unsigned(c$app_arg_3[0+:64])) : ({64 {1'bx}});

  // writeToBiSignal# begin
  assign \bus'  = (c$app_arg_2 == 1'b1) ? (c$app_arg_4) : {64 {1'bz}};
  // writeToBiSignal# end

  assign ds_2 = $signed(c$ds_app_arg_5);

  assign a1_0 = $signed(c$ds_app_arg_7[63:0]);

  assign i_0 = a1_0;

  assign c$ds_app_arg_6 = write_0 ? prevread_0 : (ds_2);

  always @(*) begin
    case(x1_0)
      64'sd9223372036854775807 : c$ds_case_alt_0 = {64 {1'bx}};
      default : c$ds_case_alt_0 = (x1_0 + 64'sd1);
    endcase
  end

  assign c$ds_app_arg_7 = write_0 ? {1'b1,c$ds_case_alt_0} : {1'b0,64'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx};

  // register begin
  always @(posedge clk or  posedge  rst) begin : ds_3_register
    if ( rst) begin
      ds_3 <= {1'b0,   64'sd0};
    end else if (en) begin
      ds_3 <= {~ write_0,   c$ds_app_arg_6};
    end
  end
  // register end

  assign write_0 = ds_3[64:64];

  assign prevread_0 = $signed(ds_3[63:0]);

  assign x1_0 = prevread_0;

  assign result = ds;


endmodule

