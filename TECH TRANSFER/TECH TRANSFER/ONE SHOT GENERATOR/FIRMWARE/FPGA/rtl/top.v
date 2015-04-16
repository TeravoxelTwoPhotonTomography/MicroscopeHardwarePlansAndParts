`timescale 1ns/100ps

//
// Create a stretched synchronized reset pulse...
//
module reset_sync(
  clk, 
  hardreset,
  reset);

  input clk;
  input hardreset;
  output reset;

  reg [3:0] reset_reg, next_reset_reg;
  assign reset = reset_reg[3];

  initial reset_reg = 4'b0000;
  always @ (posedge clk or negedge hardreset)
  begin
    if (!hardreset)
      reset_reg = 4'b0000;
    else reset_reg = next_reset_reg;
  end

  always @*
  begin
    next_reset_reg = {reset_reg,1'b1};
  end
endmodule

module oneshot(
  clkin,
  hardreset,
  cmp_p,
  cmp_n,
  sw1,
  sw2,
  pulse,
  redLED
  );

  input clkin;
  input hardreset;
  input cmp_p;
  input cmp_n;
  input sw1;
  input sw2;
  output pulse;
  output redLED;

  parameter 
  IDLE  = 3'b001,
  DELAY = 3'b010,
  PULSE = 3'b100;

  parameter 
  BUTTON_IDLE = 4'b0001,
  BUTTON_DOWN = 4'b0010,
  BUTTON_UP = 4'b0100,
  BUTTON_RESET = 4'b1000;

  reg [27:0] cnt;
  reg [5:0] PWM_adj;
  reg [6:0] PWM_width;
  
  reg redLED;

  reg cmp_d1, cmp_d2, cmp_d3_p, cmp_d4_p, cmp_d5_p;
  reg pos_edge_detect;
  reg cmp_d3_n, cmp_d4_n, cmp_d5_n;
  reg neg_edge_detect;

  reg [14:0] width_cnt;
  reg [13:0] width;
  
  reg [17:0] button_cnt;
  reg bcnt_rollover;
  reg sw1_d1, sw1_d2, sw1_d3, sw1_d4;
  reg sw2_d1, sw2_d2, sw2_d3, sw2_d4;
  reg sw1_pressed, sw1_released;
  reg sw2_pressed, sw2_released;
      
  reg [3:0] bstate, next_bstate;
  reg delay_incr, next_delay_incr;
  reg delay_decr, next_delay_decr;
  reg delay_reset, next_delay_reset;
  reg [7:0] bcnt_repeat, next_bcnt_repeat;
  reg [7:0] bcnt, next_bcnt;
  reg [9:0] initial_delay, next_initial_delay;
  
  reg pulse, next_pulse;
  reg [2:0] state, next_state;
  reg [13:0] counter, next_counter;
  reg [11:0] pcounter, next_pcounter;
  reg [13:0] delay_value;
  reg [13:0] delay;

  wire cmp;
  
  DCM_SP DCM_SP_INST(
    .CLKIN(clkin),
    .CLKFB(clk),
    .RST(1'b0),
    .PSEN(1'b0),
    .PSINCDEC(1'b0),
    .PSCLK(1'b0),
    .DSSEN(1'b0),
    .CLK0(),
    .CLK90(),
    .CLK180(),
    .CLK270(),
    .CLKDV(),
    .CLK2X(clk2x),
    .CLK2X180(),
    .CLKFX(clk4x),
    .CLKFX180(),
    .STATUS(),
    .LOCKED(),
    .PSDONE());

  defparam DCM_SP_INST.CLKIN_DIVIDE_BY_2 = "FALSE";
  defparam DCM_SP_INST.CLKIN_PERIOD = 20.000;
  defparam DCM_SP_INST.CLK_FEEDBACK = "2X";
  defparam DCM_SP_INST.CLKFX_DIVIDE = 1;
  defparam DCM_SP_INST.CLKFX_MULTIPLY = 4;

  BUFG BUFG_clkfb(.I(clk2x), .O(clk));
  BUFG BUFG_clk4x(.I(clk4x), .O(clk200));
  
  
  // Reset synchonizer
  reset_sync reset_dly(clk, hardreset, reset);


  IBUFDS  #(.IOSTANDARD("LVDS_25"), .DIFF_TERM("FALSE")
  ) ibuf_cmp (.I(cmp_p), .IB(cmp_n), .O(cmp));

// comparitor input synchronizer and positive edge detector
  always @ (posedge clk200) begin
    cmp_d1 <= cmp;
    cmp_d2 <= cmp_d1;
    cmp_d3_p <= cmp_d2 & cmp_d1;
    cmp_d4_p <= cmp_d3_p & cmp_d1;
    cmp_d5_p <= cmp_d4_p & cmp_d1;
    pos_edge_detect <= cmp_d4_p & ~cmp_d5_p;
    cmp_d3_n <= cmp_d2 | cmp_d1;
    cmp_d4_n <= cmp_d3_n | cmp_d1;
    cmp_d5_n <= cmp_d4_n | cmp_d1;
    neg_edge_detect <= ~cmp_d4_n & cmp_d5_n;
  end

// pulse width measurment
  always @ (posedge clk200) begin
    if (!reset) begin
      width_cnt <= 15'd0;
      width <= 14'd0;
    end else begin
      if (pos_edge_detect)
        width_cnt <= 15'd0;
      else
        width_cnt <= width_cnt + 1'b1;
      if (neg_edge_detect)
        width <= width_cnt >> 1;
    end
  end

// button processing
  always @ (posedge clk or negedge reset) begin
    if (!reset) begin
      button_cnt <= 18'd0;
      bcnt_rollover <= 1'd0;
    end else begin
      button_cnt <= button_cnt + 1'b1;
      bcnt_rollover <= (button_cnt == 18'd100000);
    end
  end

  always @ (posedge clk) begin
    if (bcnt_rollover) begin
      sw1_d1 <= sw1;
      sw1_d2 <= sw1_d1;
      sw1_d3 <= sw1_d2;
      sw1_d4 <= sw1_d3;
      sw1_pressed <= ({sw1_d1, sw1_d2, sw1_d3, sw1_d4} == 4'b0000);
      sw1_released <= ({sw1_d1, sw1_d2, sw1_d3, sw1_d4} == 4'b1111);
      sw2_d1 <= sw2;
      sw2_d2 <= sw2_d1;
      sw2_d3 <= sw2_d2;
      sw2_d4 <= sw2_d3;
      sw2_pressed <= ({sw2_d1, sw2_d2, sw2_d3, sw2_d4} == 4'b0000);
      sw2_released <= ({sw2_d1, sw2_d2, sw2_d3, sw2_d4} == 4'b1111);
    end
  end

  always @ (posedge clk or negedge reset) begin
    if (!reset) begin
      bstate <= BUTTON_IDLE;
      delay_decr <= 1'b0;
      delay_incr <= 1'b0;
      delay_reset <= 1'b0;
      bcnt_repeat <= 8'd100;
      bcnt <= 8'd0;
      initial_delay <= 10'd0;
    end else if (bcnt_rollover) begin
      bstate <= next_bstate;
      delay_decr <= next_delay_decr;
      delay_incr <= next_delay_incr;
      delay_reset <= next_delay_reset;
      bcnt_repeat <= next_bcnt_repeat;
      bcnt <= next_bcnt;
      initial_delay <= next_initial_delay;
    end else begin
      delay_decr <= 1'b0;
      delay_incr <= 1'b0;
      delay_reset <= 1'b0;
    end
  end

  always @ (posedge clk or negedge reset) begin
    if (!reset) begin
      delay_value <= 14'h3ffc; // initial starting value = -20 nS
    end else begin
      if (delay_decr)
        delay_value <= delay_value - 1'b1;
      else if (delay_incr)
        delay_value <= delay_value + 1'b1;
      else if (delay_reset)
        delay_value <= 14'h3ffc;
    end
  end

  always @ * begin
    next_bstate = bstate;
    next_delay_decr = 1'b0;
    next_delay_incr = 1'b0;
    next_delay_reset = 1'b0;
    next_bcnt_repeat = bcnt_repeat;
    next_bcnt = bcnt;
    next_initial_delay = initial_delay;
    case (bstate)
      BUTTON_IDLE: begin
        next_bcnt_repeat = 8'd200;
        next_bcnt = 8'd0;
        next_initial_delay = 10'd0;
        if (sw1_pressed & sw2_released) begin
          next_bstate = BUTTON_DOWN;
          next_delay_decr = 1'b1;
        end else if (sw2_pressed & sw1_released) begin
          next_bstate = BUTTON_UP;
          next_delay_incr = 1'b1;
        end else if (sw1_pressed & sw2_pressed) begin
          next_bstate = BUTTON_RESET;
        end
      end
      BUTTON_DOWN: begin
        if (sw1_pressed & sw2_released) begin
          if (initial_delay == 10'd200) begin // 200 mS before repeat starts
            if (bcnt == bcnt_repeat) begin
              next_delay_decr = 1'b1;
              next_bcnt = 8'd0;
              if (bcnt_repeat > 5)
                next_bcnt_repeat = bcnt_repeat - 3'd5;
            end else begin
              next_bcnt = bcnt + 1'b1;
            end
          end else begin
            next_initial_delay = initial_delay + 1'b1;
          end
        end else begin
          next_bstate = BUTTON_IDLE;
        end
      end
      BUTTON_UP: begin
        if (sw2_pressed & sw1_released) begin
          if (initial_delay == 10'd200) begin // 200 mS before repeat starts
            if (bcnt == bcnt_repeat) begin
              next_delay_incr = 1'b1;
              next_bcnt = 8'd0;
              if (bcnt_repeat > 5)
                next_bcnt_repeat = bcnt_repeat - 3'd5;
            end else begin
              next_bcnt = bcnt + 1'b1;
            end
          end else begin
            next_initial_delay = initial_delay + 1'b1;
          end
        end else begin
          next_bstate = BUTTON_IDLE;
        end
      end
      BUTTON_RESET: begin
        if (sw1_pressed & sw2_pressed) begin
          if (initial_delay == 10'd1000) begin // 1000 mS before reset
            next_delay_reset = 1'b1;
            next_bcnt = 8'd0;
          end else begin
            next_initial_delay = initial_delay + 1'b1;
          end
        end else begin
          next_bstate = BUTTON_IDLE;
        end
      end
      default: begin
        next_bstate = BUTTON_IDLE;
      end
    endcase
  end



// output pulse generation state machine
  always @ (posedge clk200 or negedge reset) begin
    if (!reset) begin
      state <= IDLE;
      pulse <= 1'b0;
      counter <= 14'd0;
      pcounter <= 12'd0;
      delay <= 14'd0;
    end else begin
      state <= next_state;
      pulse <= next_pulse;
      counter <= next_counter;
      pcounter <= next_pcounter;
      delay <= delay_value + width;
    end
  end

  always @ * begin
    next_state = state;
    next_pulse = 1'b0;
    next_counter = 0;
    next_pcounter = 0;
    
    case (state)
      IDLE: begin
        if (pos_edge_detect)
          next_state = DELAY;
      end
      DELAY: begin
        next_counter = counter + 1'b1;
        if (counter >= delay) begin
          next_state = PULSE;
        end
      end
      PULSE: begin
        next_pulse = 1'b1;
        next_pcounter = pcounter + 1'b1;
        if (pcounter >= 2000) begin // 2000 * 5nS = 10uS
          next_state = IDLE;
        end
      end
      default: begin
        next_state = IDLE;
      end
    endcase
    
  end
        
// breating LED :)
  always @(posedge clk or negedge reset) begin
    if(!reset) begin
      cnt <= 28'b0;
      PWM_width <= 7'b0;
      redLED <= 1'b0;
      PWM_adj <= 6'b0;
    end else begin
      cnt <= cnt + 1'b1;
      PWM_width <= PWM_width[5:0] + PWM_adj;
      if(cnt[27])
        PWM_adj <= cnt[26:21];
      else 
        PWM_adj <= ~ cnt[26:21];
      redLED <= PWM_width[6];
    end
  end


endmodule
