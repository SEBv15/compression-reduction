
// Verilog version of the Merge.scala module that explicitly uses tristate multiplexers.
//
// Theoretically this should be smaller than the chisel version since chisel's MuxLookup gets translated 
// to a cascade of muxes which should be vastly worse space-wise than the tristate version, but iverilog says the opposite.

module MergeVerilog (data1, len1, data2, len2, out, outlen);
    parameter   WIDTH = 16,
                LEN1 = 10,
                LEN2 = 10;

    input   [(WIDTH*LEN1-1):0] data1;
    input   [MinBitWidth(LEN1)-1:0] len1;

    input   [(WIDTH*LEN2-1):0] data2;
    input   [MinBitWidth(LEN2)-1:0] len2;

    output  [(WIDTH*(LEN1+LEN2)-1):0] out;
    output  [MinBitWidth(LEN1+LEN2)-1:0] outlen;

    // 2D array to facility easier access
    wire [(LEN1-1):0][(WIDTH-1):0] d1;
    wire [(LEN2-1):0][(WIDTH-1):0] d2;
    wire [(LEN1+LEN2-1):0][(WIDTH-1):0] o;

    assign d1 = data1;
    assign d2 = data2;
    assign out = o;

    wire [MinBitWidth(LEN1):0] l1 = {1'b0, len1};
    wire [MinBitWidth(LEN2):0] l2 = {1'b0, len2};

    genvar i;
    genvar j;
    generate
        // Iterate through the output positions (i) and create a tristate mux for them
        for (i = 0; i < LEN1; i = i + 1) begin
            // default case if the element should just be from the first input
            assign o[LEN1 + LEN2 - i - 1] = (i < l1) ? d1[LEN1 - i - 1] : 'bz;
            // Go through all the elements from the second vector that can be at output position i
            for (j = 0; j <= i; j = j + 1) begin
                // Set the condition for when the jth element in the second vector should be the ith element in the output
                assign o[LEN1 + LEN2 - i - 1] = (i - l1 == j && i >= l1) ? d2[LEN2 - j - 1] : 'bz;
            end
        end

        // Do the same thing for the positions were the elements can only come from the second vector
        for (i = LEN1; i < LEN1 + LEN2; i = i + 1) begin
            for (j = i - LEN1; j < LEN2; j = j + 1) begin
                assign o[LEN1 + LEN2 - i - 1] = (i - l1 == j) ? d2[LEN2 - j - 1] : 'bz;
            end
        end
    endgenerate

    assign outlen = len1 + len2;

    // Function to calculate the bitwidth needed to represent a number
    function integer MinBitWidth;
        input [1023:0] value;
        begin
            for (MinBitWidth = 0; value > 0; MinBitWidth = MinBitWidth + 1)
            begin
                value = value >> 1;
            end
        end
    endfunction

endmodule
