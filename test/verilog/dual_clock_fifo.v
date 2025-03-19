module dual_clock_fifo #(
    parameter DATA_WIDTH = 8,
    parameter ADDR_WIDTH = 4 // Depth = 2^ADDR_WIDTH
)(
    input wire wr_clk,      // Write clock domain
    input wire wr_rst_n,    // Active-low write reset
    input wire wr_en,       // Write enable
    input wire [DATA_WIDTH-1:0] wr_data, // Write data
    output wire full,       // FIFO full flag

    input wire rd_clk,      // Read clock domain
    input wire rd_rst_n,    // Active-low read reset
    input wire rd_en,       // Read enable
    output wire [DATA_WIDTH-1:0] rd_data, // Read data
    output wire empty       // FIFO empty flag
);

    localparam DEPTH = (1 << ADDR_WIDTH);

    // Internal Signals
    reg [DATA_WIDTH-1:0] mem [0:DEPTH-1]; // FIFO Memory
    reg [ADDR_WIDTH:0] wr_ptr, rd_ptr; // Binary pointers
    reg [ADDR_WIDTH:0] wr_ptr_gray, rd_ptr_gray; // Gray pointers
    reg [ADDR_WIDTH:0] rd_ptr_gray_sync1, rd_ptr_gray_sync2;
    reg [ADDR_WIDTH:0] wr_ptr_gray_sync1, wr_ptr_gray_sync2;

    // Write Logic
    always @(posedge wr_clk or negedge wr_rst_n) begin
        if (!wr_rst_n) begin
            wr_ptr <= 0;
            wr_ptr_gray <= 0;
        end else if (wr_en && !full) begin
            mem[wr_ptr[ADDR_WIDTH-1:0]] <= wr_data;
            wr_ptr <= wr_ptr + 1;
            wr_ptr_gray <= (wr_ptr >> 1) ^ wr_ptr; // Binary to Gray
        end
    end

    // Read Logic
    always @(posedge rd_clk or negedge rd_rst_n) begin
        if (!rd_rst_n) begin
            rd_ptr <= 0;
            rd_ptr_gray <= 0;
        end else if (rd_en && !empty) begin
            rd_ptr <= rd_ptr + 1;
            rd_ptr_gray <= (rd_ptr >> 1) ^ rd_ptr; // Binary to Gray
        end
    end

    assign rd_data = mem[rd_ptr[ADDR_WIDTH-1:0]];

    // Synchronize read pointer into write clock domain
    always @(posedge wr_clk or negedge wr_rst_n) begin
        if (!wr_rst_n) begin
            rd_ptr_gray_sync1 <= 0;
            rd_ptr_gray_sync2 <= 0;
        end else begin
            rd_ptr_gray_sync1 <= rd_ptr_gray;
            rd_ptr_gray_sync2 <= rd_ptr_gray_sync1;
        end
    end

    // Synchronize write pointer into read clock domain
    always @(posedge rd_clk or negedge rd_rst_n) begin
        if (!rd_rst_n) begin
            wr_ptr_gray_sync1 <= 0;
            wr_ptr_gray_sync2 <= 0;
        end else begin
            wr_ptr_gray_sync1 <= wr_ptr_gray;
            wr_ptr_gray_sync2 <= wr_ptr_gray_sync1;
        end
    end

    // Convert synchronized Gray code back to binary
    function [ADDR_WIDTH:0] gray_to_bin;
        input [ADDR_WIDTH:0] gray;
        integer i;
        begin
            gray_to_bin[ADDR_WIDTH] = gray[ADDR_WIDTH];
            for (i = ADDR_WIDTH-1; i >= 0; i = i - 1) 
                gray_to_bin[i] = gray[i] ^ gray_to_bin[i+1];
        end
    endfunction

    wire [ADDR_WIDTH:0] wr_ptr_bin = gray_to_bin(wr_ptr_gray_sync2);
    wire [ADDR_WIDTH:0] rd_ptr_bin = gray_to_bin(rd_ptr_gray_sync2);

    // FIFO full condition
    assign full = (wr_ptr_bin[ADDR_WIDTH-1:0] == rd_ptr_bin[ADDR_WIDTH-1:0]) &&
                  (wr_ptr_bin[ADDR_WIDTH] != rd_ptr_bin[ADDR_WIDTH]);

    // FIFO empty condition
    assign empty = (wr_ptr_bin == rd_ptr_bin);

endmodule
