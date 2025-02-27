/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.8.2. DO NOT MODIFY.
*/
`default_nettype none
`timescale 100fs/100fs
module topEntity
    ( // Inputs
      input wire [2:0] ds_0_0
    , input wire [4:0] ds_0_1
    , input wire [2:0] ds_1
    , input wire [2:0] ds_2
    , input wire [2:0] ds_3
    , input wire [2:0] ds_4
    , input wire [8:0] ds_5
    , input wire [8:0] ds_6
    , input wire [9:0] ds_7
    , input wire [9:0] ds_8
    , input wire [9:0] ds_9
    , input wire [2:0] ds_10_0
    , input wire [4:0] ds_10_1

      // Outputs
    , output wire [151:0] result
    );
  wire [7:0] a;
  wire [2:0] b;
  wire [2:0] c;
  wire [2:0] d;
  wire [2:0] e;
  wire [8:0] f;
  wire [8:0] g;
  wire [9:0] h;
  wire [9:0] i;
  wire [9:0] j;
  wire [7:0] k;
  wire [75:0] packed;
  wire [2:0] a1;
  wire [4:0] b1;
  wire [2:0] l0;
  wire [4:0] r0;
  wire [2:0] l1;
  wire [4:0] r1;
  wire [71:0] result_1;
  wire [7:0] l;
  wire [7:0] packed_0;
  wire [2:0] l_0;
  wire [2:0] packed_1;
  wire [4:0] r;
  wire [4:0] packed_2;
  wire [68:0] result_2;
  wire [5:0] r_0;
  wire [4:0] packed_3;
  wire [71:0] result_3;
  wire signed [63:0] sc;
  wire [7:0] packedFields;
  wire signed [63:0] i_0;
  wire signed [63:0] c$app_arg;
  reg [8:0] c$app_arg_0;
  wire [4:0] g1;
  wire [2:0] g1_0;
  wire [2:0] g1_1;
  wire [4:0] g2;
  wire [9:0] result_4;
  wire [2:0] l0_0;
  wire [4:0] r0_0;
  wire [2:0] l1_0;
  wire [4:0] r1_0;
  wire [71:0] result_5;
  wire [7:0] l_1;
  wire [7:0] packed_4;
  wire [2:0] l_2;
  wire [2:0] packed_5;
  wire [4:0] r_1;
  wire [4:0] packed_6;
  wire [68:0] result_6;
  wire [5:0] r_2;
  wire [4:0] packed_7;
  wire [71:0] result_7;
  wire signed [63:0] sc_0;
  wire [7:0] packedFields_0;
  wire signed [63:0] i_1;
  wire signed [63:0] c$app_arg_1;
  reg [8:0] c$app_arg_2;
  wire [4:0] g1_2;
  wire [2:0] g1_3;
  wire [2:0] g1_4;
  wire [4:0] g2_0;
  wire [9:0] result_8;
  wire [2:0] l0_1;
  wire [4:0] r0_1;
  wire [2:0] l1_1;
  wire [4:0] r1_1;
  wire [71:0] result_9;
  wire [7:0] l_3;
  wire [7:0] packed_8;
  wire [2:0] l_4;
  wire [2:0] packed_9;
  wire [4:0] r_3;
  wire [4:0] packed_10;
  wire [68:0] result_10;
  wire [5:0] r_4;
  wire [4:0] packed_11;
  wire [71:0] result_11;
  wire signed [63:0] sc_1;
  wire [7:0] packedFields_1;
  wire signed [63:0] i_2;
  wire signed [63:0] c$app_arg_3;
  reg [8:0] c$app_arg_4;
  wire [4:0] g1_5;
  wire [2:0] g1_6;
  wire [2:0] g1_7;
  wire [4:0] g2_1;
  wire [9:0] result_12;
  wire [2:0] l0_2;
  wire [4:0] r0_2;
  wire [2:0] l1_2;
  wire [4:0] r1_2;
  wire [71:0] result_13;
  wire [7:0] l_5;
  wire [7:0] packed_12;
  wire [4:0] l0_3;
  wire [2:0] r0_3;
  wire [4:0] l1_3;
  wire [2:0] r1_3;
  wire [71:0] result_14;
  wire [7:0] r_5;
  wire [7:0] packed_13;
  wire [71:0] result_15;
  wire signed [63:0] sc_2;
  wire [7:0] packedFields_2;
  wire signed [63:0] i_3;
  wire signed [63:0] c$app_arg_5;
  wire [8:0] c$app_arg_6;
  wire [4:0] g1_8;
  wire [2:0] g2_2;
  wire [2:0] g1_9;
  wire [4:0] g2_3;
  wire [8:0] result_16;
  wire [2:0] l0_4;
  wire [4:0] r0_4;
  wire [2:0] l1_4;
  wire [4:0] r1_4;
  wire [71:0] result_17;
  wire [7:0] l_6;
  wire [7:0] packed_14;
  wire [4:0] l0_5;
  wire [2:0] r0_5;
  wire [4:0] l1_5;
  wire [2:0] r1_5;
  wire [71:0] result_18;
  wire [7:0] r_6;
  wire [7:0] packed_15;
  wire [71:0] result_19;
  wire signed [63:0] sc_3;
  wire [7:0] packedFields_3;
  wire signed [63:0] i_4;
  wire signed [63:0] c$app_arg_7;
  wire [8:0] c$app_arg_8;
  wire [4:0] g1_10;
  wire [2:0] g2_4;
  wire [2:0] g1_11;
  wire [4:0] g2_5;
  wire [8:0] result_20;
  wire signed [63:0] ds1;
  wire [0:0] r_7;
  wire signed [63:0] result_21;
  wire [1:0] l_7;
  wire signed [63:0] ds1_0;
  wire [0:0] l_8;
  wire signed [63:0] ds1_1;
  wire [0:0] r_8;
  wire signed [63:0] result_22;
  wire [1:0] r_9;
  wire signed [63:0] result_23;
  wire signed [63:0] sc_4;
  wire signed [63:0] i_5;
  wire signed [63:0] c$app_arg_9;
  reg [2:0] c$app_arg_10;
  wire [2:0] result_24;
  wire signed [63:0] ds1_2;
  wire [0:0] r_10;
  wire signed [63:0] result_25;
  wire [1:0] l_9;
  wire signed [63:0] ds1_3;
  wire [0:0] l_10;
  wire signed [63:0] ds1_4;
  wire [0:0] r_11;
  wire signed [63:0] result_26;
  wire [1:0] r_12;
  wire signed [63:0] result_27;
  wire signed [63:0] sc_5;
  wire signed [63:0] i_6;
  wire signed [63:0] c$app_arg_11;
  reg [2:0] c$app_arg_12;
  wire [2:0] result_28;
  wire signed [63:0] ds1_5;
  wire [0:0] r_13;
  wire signed [63:0] result_29;
  wire [1:0] l_11;
  wire signed [63:0] ds1_6;
  wire [0:0] l_12;
  wire signed [63:0] ds1_7;
  wire [0:0] r_14;
  wire signed [63:0] result_30;
  wire [1:0] r_15;
  wire signed [63:0] result_31;
  wire signed [63:0] sc_6;
  wire signed [63:0] i_7;
  wire signed [63:0] c$app_arg_13;
  reg [2:0] c$app_arg_14;
  wire [2:0] result_32;
  wire signed [63:0] ds1_8;
  wire [0:0] r_16;
  wire signed [63:0] result_33;
  wire [1:0] l_13;
  wire signed [63:0] ds1_9;
  wire [0:0] l_14;
  wire signed [63:0] ds1_10;
  wire [0:0] r_17;
  wire signed [63:0] result_34;
  wire [1:0] r_18;
  wire signed [63:0] result_35;
  wire signed [63:0] sc_7;
  wire signed [63:0] i_8;
  wire signed [63:0] c$app_arg_15;
  reg [2:0] c$app_arg_16;
  wire [2:0] result_36;
  wire [7:0] packedFields_8;
  wire [2:0] g1_12;
  wire [4:0] g2_6;
  wire [7:0] result_37;
  wire [75:0] ds;
  wire [63:0] c$bv;
  wire [63:0] c$bv_0;
  wire [63:0] c$bv_1;
  wire [63:0] c$bv_2;
  wire [63:0] c$bv_3;
  wire [63:0] c$bv_4;
  wire [63:0] c$bv_5;
  wire [63:0] c$bv_6;
  wire [63:0] c$bv_7;

  assign ds = {{ds_0_0,   ds_0_1},   ds_1,
               ds_2,   ds_3,   ds_4,   ds_5,   ds_6,   ds_7,
               ds_8,   ds_9,   {ds_10_0,   ds_10_1}};

  assign a = ds[75:68];

  assign b = ds[67:65];

  assign c = ds[64:62];

  assign d = ds[61:59];

  assign e = ds[58:56];

  assign f = ds[55:47];

  assign g = ds[46:38];

  assign h = ds[37:28];

  assign i = ds[27:18];

  assign j = ds[17:8];

  assign k = ds[7:0];

  assign packed = ({(result_37),(({(result_36),(({(result_32),(({(result_28),(({(result_24),(({(result_20),(({(result_16),(({(result_12),(({(result_8),(({(result_4),((({((a1)),((b1))})))}))}))}))}))}))}))}))}))}))});

  assign result = {packed,packed};

  assign a1 = k[7:5];

  assign b1 = k[4:0];

  assign l0 = l[7:5];

  assign r0 = l[4:0];

  assign l1 = (l0);

  assign r1 = (r0);

  assign result_1 = {64'sd0,   ({l1,r1})};

  assign l = c$app_arg_0[7:0];

  assign packed_0 = result_1[7:0];

  assign l_0 = r_0[4:2];

  assign packed_1 = (l_0);

  assign r = r_0[4:0];

  assign packed_2 = (r);

  assign result_2 = r_0[5:5] ? {64'sd2,
                                packed_2} : {64'sd1,   {packed_1,2'bxx}};

  assign r_0 = c$app_arg_0[7:2];

  assign packed_3 = result_2[4:0];

  assign result_3 = c$app_arg_0[8:8] ? {$signed(result_2[68:5]),
                                        {packed_3,3'bxxx}} : {$signed(result_1[71:8]),
                                                              packed_0};

  assign sc = $signed(result_3[71:8]);

  assign packedFields = result_3[7:0];

  assign i_0 = sc;

  assign c$app_arg = i_0;

  always @(*) begin
    case(j[9:8])
      2'b00 : c$app_arg_0 = {1'b0,{g1_1,   g2}};
      2'b01 : c$app_arg_0 = {1'b1,{1'b0,g1_0,2'bxx},2'bxx};
      default : c$app_arg_0 = {1'b1,{1'b1,g1},2'bxx};
    endcase
  end

  assign g1 = j[7:3];

  assign g1_0 = j[7:5];

  assign g1_1 = j[7:5];

  assign g2 = j[4:0];

  assign c$bv = (($unsigned(c$app_arg[0+:64])));

  assign result_4 = ({(c$bv[0+:2]),packedFields});

  assign l0_0 = l_1[7:5];

  assign r0_0 = l_1[4:0];

  assign l1_0 = (l0_0);

  assign r1_0 = (r0_0);

  assign result_5 = {64'sd0,   ({l1_0,r1_0})};

  assign l_1 = c$app_arg_2[7:0];

  assign packed_4 = result_5[7:0];

  assign l_2 = r_2[4:2];

  assign packed_5 = (l_2);

  assign r_1 = r_2[4:0];

  assign packed_6 = (r_1);

  assign result_6 = r_2[5:5] ? {64'sd2,
                                packed_6} : {64'sd1,   {packed_5,2'bxx}};

  assign r_2 = c$app_arg_2[7:2];

  assign packed_7 = result_6[4:0];

  assign result_7 = c$app_arg_2[8:8] ? {$signed(result_6[68:5]),
                                        {packed_7,3'bxxx}} : {$signed(result_5[71:8]),
                                                              packed_4};

  assign sc_0 = $signed(result_7[71:8]);

  assign packedFields_0 = result_7[7:0];

  assign i_1 = sc_0;

  assign c$app_arg_1 = i_1;

  always @(*) begin
    case(i[9:8])
      2'b00 : c$app_arg_2 = {1'b0,{g1_4,   g2_0}};
      2'b01 : c$app_arg_2 = {1'b1,{1'b0,g1_3,2'bxx},2'bxx};
      default : c$app_arg_2 = {1'b1,{1'b1,g1_2},2'bxx};
    endcase
  end

  assign g1_2 = i[7:3];

  assign g1_3 = i[7:5];

  assign g1_4 = i[7:5];

  assign g2_0 = i[4:0];

  assign c$bv_0 = (($unsigned(c$app_arg_1[0+:64])));

  assign result_8 = ({(c$bv_0[0+:2]),packedFields_0});

  assign l0_1 = l_3[7:5];

  assign r0_1 = l_3[4:0];

  assign l1_1 = (l0_1);

  assign r1_1 = (r0_1);

  assign result_9 = {64'sd0,   ({l1_1,r1_1})};

  assign l_3 = c$app_arg_4[7:0];

  assign packed_8 = result_9[7:0];

  assign l_4 = r_4[4:2];

  assign packed_9 = (l_4);

  assign r_3 = r_4[4:0];

  assign packed_10 = (r_3);

  assign result_10 = r_4[5:5] ? {64'sd2,
                                 packed_10} : {64'sd1,   {packed_9,2'bxx}};

  assign r_4 = c$app_arg_4[7:2];

  assign packed_11 = result_10[4:0];

  assign result_11 = c$app_arg_4[8:8] ? {$signed(result_10[68:5]),
                                         {packed_11,3'bxxx}} : {$signed(result_9[71:8]),
                                                                packed_8};

  assign sc_1 = $signed(result_11[71:8]);

  assign packedFields_1 = result_11[7:0];

  assign i_2 = sc_1;

  assign c$app_arg_3 = i_2;

  always @(*) begin
    case(h[9:8])
      2'b00 : c$app_arg_4 = {1'b0,{g1_7,   g2_1}};
      2'b01 : c$app_arg_4 = {1'b1,{1'b0,g1_6,2'bxx},2'bxx};
      default : c$app_arg_4 = {1'b1,{1'b1,g1_5},2'bxx};
    endcase
  end

  assign g1_5 = h[7:3];

  assign g1_6 = h[7:5];

  assign g1_7 = h[7:5];

  assign g2_1 = h[4:0];

  assign c$bv_1 = (($unsigned(c$app_arg_3[0+:64])));

  assign result_12 = ({(c$bv_1[0+:2]),packedFields_1});

  assign l0_2 = l_5[7:5];

  assign r0_2 = l_5[4:0];

  assign l1_2 = (l0_2);

  assign r1_2 = (r0_2);

  assign result_13 = {64'sd0,   ({l1_2,r1_2})};

  assign l_5 = c$app_arg_6[7:0];

  assign packed_12 = result_13[7:0];

  assign l0_3 = r_5[7:3];

  assign r0_3 = r_5[2:0];

  assign l1_3 = (l0_3);

  assign r1_3 = (r0_3);

  assign result_14 = {64'sd1,   ({l1_3,r1_3})};

  assign r_5 = c$app_arg_6[7:0];

  assign packed_13 = result_14[7:0];

  assign result_15 = c$app_arg_6[8:8] ? {$signed(result_14[71:8]),
                                         packed_13} : {$signed(result_13[71:8]),
                                                       packed_12};

  assign sc_2 = $signed(result_15[71:8]);

  assign packedFields_2 = result_15[7:0];

  assign i_3 = sc_2;

  assign c$app_arg_5 = i_3;

  assign c$app_arg_6 = g[8:8] ? {1'b1,{g1_8,
                                       g2_2}} : {1'b0,{g1_9,   g2_3}};

  assign g1_8 = g[7:3];

  assign g2_2 = g[2:0];

  assign g1_9 = g[7:5];

  assign g2_3 = g[4:0];

  assign c$bv_2 = (($unsigned(c$app_arg_5[0+:64])));

  assign result_16 = ({(c$bv_2[0+:1]),packedFields_2});

  assign l0_4 = l_6[7:5];

  assign r0_4 = l_6[4:0];

  assign l1_4 = (l0_4);

  assign r1_4 = (r0_4);

  assign result_17 = {64'sd0,   ({l1_4,r1_4})};

  assign l_6 = c$app_arg_8[7:0];

  assign packed_14 = result_17[7:0];

  assign l0_5 = r_6[7:3];

  assign r0_5 = r_6[2:0];

  assign l1_5 = (l0_5);

  assign r1_5 = (r0_5);

  assign result_18 = {64'sd1,   ({l1_5,r1_5})};

  assign r_6 = c$app_arg_8[7:0];

  assign packed_15 = result_18[7:0];

  assign result_19 = c$app_arg_8[8:8] ? {$signed(result_18[71:8]),
                                         packed_15} : {$signed(result_17[71:8]),
                                                       packed_14};

  assign sc_3 = $signed(result_19[71:8]);

  assign packedFields_3 = result_19[7:0];

  assign i_4 = sc_3;

  assign c$app_arg_7 = i_4;

  assign c$app_arg_8 = f[8:8] ? {1'b1,{g1_10,
                                       g2_4}} : {1'b0,{g1_11,   g2_5}};

  assign g1_10 = f[7:3];

  assign g2_4 = f[2:0];

  assign g1_11 = f[7:5];

  assign g2_5 = f[4:0];

  assign c$bv_3 = (($unsigned(c$app_arg_7[0+:64])));

  assign result_20 = ({(c$bv_3[0+:1]),packedFields_3});

  assign ds1 = r_7 ? (64'sd2) : (64'sd1);

  assign r_7 = l_7[0:0];

  assign result_21 = l_7[1:1] ? ds1 : (64'sd0);

  assign l_7 = c$app_arg_10[1:0];

  assign ds1_0 = l_8 ? (64'sd4) : (64'sd3);

  assign l_8 = r_9[0:0];

  assign ds1_1 = r_8 ? (64'sd6) : (64'sd5);

  assign r_8 = r_9[0:0];

  assign result_22 = r_9[1:1] ? ds1_1 : ds1_0;

  assign r_9 = c$app_arg_10[1:0];

  assign result_23 = c$app_arg_10[2:2] ? result_22 : result_21;

  assign sc_4 = result_23;

  assign i_5 = sc_4;

  assign c$app_arg_9 = i_5;

  always @(*) begin
    case(e)
      3'b000 : c$app_arg_10 = {1'b0,{1'b0,1'bx}};
      3'b001 : c$app_arg_10 = {1'b0,{1'b1,1'd0}};
      3'b010 : c$app_arg_10 = {1'b0,{1'b1,1'd1}};
      3'b011 : c$app_arg_10 = {1'b1,{1'b0,1'd0}};
      3'b100 : c$app_arg_10 = {1'b1,{1'b0,1'd1}};
      3'b101 : c$app_arg_10 = {1'b1,{1'b1,1'd0}};
      default : c$app_arg_10 = {1'b1,{1'b1,1'd1}};
    endcase
  end

  assign c$bv_4 = (($unsigned(c$app_arg_9[0+:64])));

  assign result_24 = ((c$bv_4[0+:3]));

  assign ds1_2 = r_10 ? (64'sd2) : (64'sd1);

  assign r_10 = l_9[0:0];

  assign result_25 = l_9[1:1] ? ds1_2 : (64'sd0);

  assign l_9 = c$app_arg_12[1:0];

  assign ds1_3 = l_10 ? (64'sd4) : (64'sd3);

  assign l_10 = r_12[0:0];

  assign ds1_4 = r_11 ? (64'sd6) : (64'sd5);

  assign r_11 = r_12[0:0];

  assign result_26 = r_12[1:1] ? ds1_4 : ds1_3;

  assign r_12 = c$app_arg_12[1:0];

  assign result_27 = c$app_arg_12[2:2] ? result_26 : result_25;

  assign sc_5 = result_27;

  assign i_6 = sc_5;

  assign c$app_arg_11 = i_6;

  always @(*) begin
    case(d)
      3'b000 : c$app_arg_12 = {1'b0,{1'b0,1'bx}};
      3'b001 : c$app_arg_12 = {1'b0,{1'b1,1'd0}};
      3'b010 : c$app_arg_12 = {1'b0,{1'b1,1'd1}};
      3'b011 : c$app_arg_12 = {1'b1,{1'b0,1'd0}};
      3'b100 : c$app_arg_12 = {1'b1,{1'b0,1'd1}};
      3'b101 : c$app_arg_12 = {1'b1,{1'b1,1'd0}};
      default : c$app_arg_12 = {1'b1,{1'b1,1'd1}};
    endcase
  end

  assign c$bv_5 = (($unsigned(c$app_arg_11[0+:64])));

  assign result_28 = ((c$bv_5[0+:3]));

  assign ds1_5 = r_13 ? (64'sd2) : (64'sd1);

  assign r_13 = l_11[0:0];

  assign result_29 = l_11[1:1] ? ds1_5 : (64'sd0);

  assign l_11 = c$app_arg_14[1:0];

  assign ds1_6 = l_12 ? (64'sd4) : (64'sd3);

  assign l_12 = r_15[0:0];

  assign ds1_7 = r_14 ? (64'sd6) : (64'sd5);

  assign r_14 = r_15[0:0];

  assign result_30 = r_15[1:1] ? ds1_7 : ds1_6;

  assign r_15 = c$app_arg_14[1:0];

  assign result_31 = c$app_arg_14[2:2] ? result_30 : result_29;

  assign sc_6 = result_31;

  assign i_7 = sc_6;

  assign c$app_arg_13 = i_7;

  always @(*) begin
    case(c)
      3'b000 : c$app_arg_14 = {1'b0,{1'b0,1'bx}};
      3'b001 : c$app_arg_14 = {1'b0,{1'b1,1'd0}};
      3'b010 : c$app_arg_14 = {1'b0,{1'b1,1'd1}};
      3'b011 : c$app_arg_14 = {1'b1,{1'b0,1'd0}};
      3'b100 : c$app_arg_14 = {1'b1,{1'b0,1'd1}};
      3'b101 : c$app_arg_14 = {1'b1,{1'b1,1'd0}};
      default : c$app_arg_14 = {1'b1,{1'b1,1'd1}};
    endcase
  end

  assign c$bv_6 = (($unsigned(c$app_arg_13[0+:64])));

  assign result_32 = ((c$bv_6[0+:3]));

  assign ds1_8 = r_16 ? (64'sd2) : (64'sd1);

  assign r_16 = l_13[0:0];

  assign result_33 = l_13[1:1] ? ds1_8 : (64'sd0);

  assign l_13 = c$app_arg_16[1:0];

  assign ds1_9 = l_14 ? (64'sd4) : (64'sd3);

  assign l_14 = r_18[0:0];

  assign ds1_10 = r_17 ? (64'sd6) : (64'sd5);

  assign r_17 = r_18[0:0];

  assign result_34 = r_18[1:1] ? ds1_10 : ds1_9;

  assign r_18 = c$app_arg_16[1:0];

  assign result_35 = c$app_arg_16[2:2] ? result_34 : result_33;

  assign sc_7 = result_35;

  assign i_8 = sc_7;

  assign c$app_arg_15 = i_8;

  always @(*) begin
    case(b)
      3'b000 : c$app_arg_16 = {1'b0,{1'b0,1'bx}};
      3'b001 : c$app_arg_16 = {1'b0,{1'b1,1'd0}};
      3'b010 : c$app_arg_16 = {1'b0,{1'b1,1'd1}};
      3'b011 : c$app_arg_16 = {1'b1,{1'b0,1'd0}};
      3'b100 : c$app_arg_16 = {1'b1,{1'b0,1'd1}};
      3'b101 : c$app_arg_16 = {1'b1,{1'b1,1'd0}};
      default : c$app_arg_16 = {1'b1,{1'b1,1'd1}};
    endcase
  end

  assign c$bv_7 = (($unsigned(c$app_arg_15[0+:64])));

  assign result_36 = ((c$bv_7[0+:3]));

  assign packedFields_8 = ({((g1_12)),((g2_6))});

  assign g1_12 = a[7:5];

  assign g2_6 = a[4:0];

  assign result_37 = (packedFields_8);


endmodule

