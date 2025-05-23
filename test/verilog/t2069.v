/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.8.2. DO NOT MODIFY.
*/
`default_nettype none
`timescale 100fs/100fs
module topEntity
    ( // Inputs
      input wire  clkA // clock
    , input wire  clkB // clock
    , input wire [9:0] opA
    , input wire [9:0] opB

      // Outputs
    , output wire [7:0] result_0
    , output wire [7:0] result_1
    );
  reg  c$app_arg;
  reg  c$app_arg_0;
  reg [7:0] c$app_arg_2;
  reg  c$app_arg_3;
  reg  c$app_arg_4;
  reg [7:0] c$app_arg_6;
  wire [7:0] val;
  wire [7:0] val_0;
  wire [15:0] result;

  always @(*) begin
    case(opA[9:8])
      2'b10 : c$app_arg = 1'b0;
      default : c$app_arg = 1'b1;
    endcase
  end

  always @(*) begin
    case(opA[9:8])
      2'b01 : c$app_arg_0 = 1'b1;
      default : c$app_arg_0 = 1'b0;
    endcase
  end

  always @(*) begin
    case(opA[9:8])
      2'b01 : c$app_arg_2 = val;
      default : c$app_arg_2 = {8 {1'bx}};
    endcase
  end

  always @(*) begin
    case(opB[9:8])
      2'b10 : c$app_arg_3 = 1'b0;
      default : c$app_arg_3 = 1'b1;
    endcase
  end

  always @(*) begin
    case(opB[9:8])
      2'b01 : c$app_arg_4 = 1'b1;
      default : c$app_arg_4 = 1'b0;
    endcase
  end

  always @(*) begin
    case(opB[9:8])
      2'b01 : c$app_arg_6 = val_0;
      default : c$app_arg_6 = {8 {1'bx}};
    endcase
  end

  assign val = opA[7:0];

  assign val_0 = opB[7:0];

  trueDualPortBlockRamWrapper topEntity34_trueDualPortBlockRamWrapper_result
    ( .result (result)
    , .clkA (clkA)
    , .enA (c$app_arg)
    , .weA (c$app_arg_0)
    , .datA (c$app_arg_2)
    , .clkB (clkB)
    , .enB (c$app_arg_3)
    , .weB (c$app_arg_4)
    , .datB (c$app_arg_6) );

  assign result_0 = result[15:8];

  assign result_1 = result[7:0];


endmodule

module trueDualPortBlockRamWrapper
    ( // Inputs
      input wire  clkA // clock
    , input wire  enA
    , input wire  weA
    , input wire [7:0] datA
    , input wire  clkB // clock
    , input wire  enB
    , input wire  weB
    , input wire [7:0] datB

      // Outputs
    , output wire [15:0] result
    );


  // trueDualPortBlockRam begin
  // Shared memory
  reg [8-1:0] mem [1-1:0];

  reg [7:0] a_dout;
  reg [7:0] b_dout;

  // Port A
  always @(posedge clkA) begin
      if(enA) begin
          a_dout <= mem[0];
          if(weA) begin
              a_dout <= datA;
              mem[0] <= datA;
          end
      end
  end

  // Port B
  always @(posedge clkB) begin
      if(enB) begin
          b_dout <= mem[0];
          if(weB) begin
              b_dout <= datB;
              mem[0] <= datB;
          end
      end
  end

  assign result = {a_dout, b_dout};

  // end trueDualPortBlockRam


endmodule
