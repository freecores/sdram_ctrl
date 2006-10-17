-- megafunction wizard: %ALTPLL%
-- GENERATION: STANDARD
-- VERSION: WM1.0
-- MODULE: altpll 

-- ============================================================
-- File Name: pll.vhd
-- Megafunction Name(s):
-- 			altpll
-- ============================================================
-- ************************************************************
-- THIS IS A WIZARD-GENERATED FILE. DO NOT EDIT THIS FILE!
--
-- 5.1 Build 213 01/19/2006 SP 1 SJ Full Version
-- ************************************************************


--Copyright (C) 1991-2006 Altera Corporation
--Your use of Altera Corporation's design tools, logic functions 
--and other software and tools, and its AMPP partner logic 
--functions, and any output files any of the foregoing 
--(including device programming or simulation files), and any 
--associated documentation or information are expressly subject 
--to the terms and conditions of the Altera Program License 
--Subscription Agreement, Altera MegaCore Function License 
--Agreement, or other applicable license agreement, including, 
--without limitation, that your use is for the sole purpose of 
--programming logic devices manufactured by Altera and sold by 
--Altera or its authorized distributors.  Please refer to the 
--applicable agreement for further details.


LIBRARY ieee;
USE ieee.std_logic_1164.all;

LIBRARY altera_mf;
USE altera_mf.altera_mf_components.all;

ENTITY pll IS
	PORT
	(
		inclk0		: IN STD_LOGIC  := '0';
		c0		: OUT STD_LOGIC ;
		e0		: OUT STD_LOGIC ;
		locked		: OUT STD_LOGIC 
	);
END pll;


ARCHITECTURE SYN OF pll IS

	SIGNAL sub_wire0	: STD_LOGIC_VECTOR (5 DOWNTO 0);
	SIGNAL sub_wire1	: STD_LOGIC ;
	SIGNAL sub_wire2	: STD_LOGIC ;
	SIGNAL sub_wire3	: STD_LOGIC_VECTOR (3 DOWNTO 0);
	SIGNAL sub_wire4	: STD_LOGIC ;
	SIGNAL sub_wire5	: STD_LOGIC ;
	SIGNAL sub_wire6	: STD_LOGIC_VECTOR (1 DOWNTO 0);
	SIGNAL sub_wire7_bv	: BIT_VECTOR (0 DOWNTO 0);
	SIGNAL sub_wire7	: STD_LOGIC_VECTOR (0 DOWNTO 0);



	COMPONENT altpll
	GENERIC (
		clk0_divide_by		: NATURAL;
		clk0_duty_cycle		: NATURAL;
		clk0_multiply_by		: NATURAL;
		clk0_phase_shift		: STRING;
		compensate_clock		: STRING;
		extclk0_divide_by		: NATURAL;
		extclk0_duty_cycle		: NATURAL;
		extclk0_multiply_by		: NATURAL;
		extclk0_phase_shift		: STRING;
		inclk0_input_frequency		: NATURAL;
		intended_device_family		: STRING;
		invalid_lock_multiplier		: NATURAL;
		lpm_type		: STRING;
		operation_mode		: STRING;
		pll_type		: STRING;
		valid_lock_multiplier		: NATURAL
	);
	PORT (
			extclk	: OUT STD_LOGIC_VECTOR (3 DOWNTO 0);
			inclk	: IN STD_LOGIC_VECTOR (1 DOWNTO 0);
			locked	: OUT STD_LOGIC ;
			clk	: OUT STD_LOGIC_VECTOR (5 DOWNTO 0)
	);
	END COMPONENT;

BEGIN
	sub_wire7_bv(0 DOWNTO 0) <= "0";
	sub_wire7    <= To_stdlogicvector(sub_wire7_bv);
	sub_wire1    <= sub_wire0(0);
	c0    <= sub_wire1;
	locked    <= sub_wire2;
	sub_wire4    <= sub_wire3(0);
	e0    <= sub_wire4;
	sub_wire5    <= inclk0;
	sub_wire6    <= sub_wire7(0 DOWNTO 0) & sub_wire5;

	altpll_component : altpll
	GENERIC MAP (
		clk0_divide_by => 2,
		clk0_duty_cycle => 50,
		clk0_multiply_by => 11,
		clk0_phase_shift => "0",
		compensate_clock => "CLK0",
		extclk0_divide_by => 2,
		extclk0_duty_cycle => 50,
		extclk0_multiply_by => 11,
		extclk0_phase_shift => "5682",
		inclk0_input_frequency => 41666,
		intended_device_family => "Cyclone",
		invalid_lock_multiplier => 5,
		lpm_type => "altpll",
		operation_mode => "NORMAL",
		pll_type => "AUTO",
		valid_lock_multiplier => 1
	)
	PORT MAP (
		inclk => sub_wire6,
		clk => sub_wire0,
		locked => sub_wire2,
		extclk => sub_wire3
	);



END SYN;

-- ============================================================
-- CNX file retrieval info
-- ============================================================
-- Retrieval info: PRIVATE: ACTIVECLK_CHECK STRING "0"
-- Retrieval info: PRIVATE: BANDWIDTH STRING "1.000"
-- Retrieval info: PRIVATE: BANDWIDTH_FEATURE_ENABLED STRING "0"
-- Retrieval info: PRIVATE: BANDWIDTH_FREQ_UNIT STRING "MHz"
-- Retrieval info: PRIVATE: BANDWIDTH_PRESET STRING "Low"
-- Retrieval info: PRIVATE: BANDWIDTH_USE_AUTO STRING "1"
-- Retrieval info: PRIVATE: BANDWIDTH_USE_CUSTOM STRING "0"
-- Retrieval info: PRIVATE: BANDWIDTH_USE_PRESET STRING "0"
-- Retrieval info: PRIVATE: CLKBAD_SWITCHOVER_CHECK STRING "0"
-- Retrieval info: PRIVATE: CLKLOSS_CHECK STRING "0"
-- Retrieval info: PRIVATE: CLKSWITCH_CHECK STRING "0"
-- Retrieval info: PRIVATE: CNX_NO_COMPENSATE_RADIO STRING "0"
-- Retrieval info: PRIVATE: CREATE_CLKBAD_CHECK STRING "0"
-- Retrieval info: PRIVATE: CREATE_INCLK1_CHECK STRING "0"
-- Retrieval info: PRIVATE: CUR_DEDICATED_CLK STRING "c0"
-- Retrieval info: PRIVATE: CUR_FBIN_CLK STRING "e0"
-- Retrieval info: PRIVATE: DEVICE_FAMILY NUMERIC "11"
-- Retrieval info: PRIVATE: DEVICE_SPEED_GRADE STRING "Any"
-- Retrieval info: PRIVATE: DEV_FAMILY STRING "Cyclone"
-- Retrieval info: PRIVATE: DIV_FACTOR0 NUMERIC "1"
-- Retrieval info: PRIVATE: DIV_FACTOR6 NUMERIC "1"
-- Retrieval info: PRIVATE: DUTY_CYCLE0 STRING "50.00000000"
-- Retrieval info: PRIVATE: DUTY_CYCLE6 STRING "50.00000000"
-- Retrieval info: PRIVATE: EXT_FEEDBACK_RADIO STRING "0"
-- Retrieval info: PRIVATE: GLOCKED_COUNTER_EDIT_CHANGED STRING "1"
-- Retrieval info: PRIVATE: GLOCKED_FEATURE_ENABLED STRING "0"
-- Retrieval info: PRIVATE: GLOCKED_MODE_CHECK STRING "0"
-- Retrieval info: PRIVATE: GLOCK_COUNTER_EDIT NUMERIC "1048575"
-- Retrieval info: PRIVATE: HAS_MANUAL_SWITCHOVER STRING "1"
-- Retrieval info: PRIVATE: INCLK0_FREQ_EDIT STRING "24.000"
-- Retrieval info: PRIVATE: INCLK0_FREQ_UNIT_COMBO STRING "MHz"
-- Retrieval info: PRIVATE: INCLK1_FREQ_EDIT STRING "100.000"
-- Retrieval info: PRIVATE: INCLK1_FREQ_EDIT_CHANGED STRING "1"
-- Retrieval info: PRIVATE: INCLK1_FREQ_UNIT_CHANGED STRING "1"
-- Retrieval info: PRIVATE: INCLK1_FREQ_UNIT_COMBO STRING "MHz"
-- Retrieval info: PRIVATE: INT_FEEDBACK__MODE_RADIO STRING "1"
-- Retrieval info: PRIVATE: LOCKED_OUTPUT_CHECK STRING "1"
-- Retrieval info: PRIVATE: LOCK_LOSS_SWITCHOVER_CHECK STRING "0"
-- Retrieval info: PRIVATE: LONG_SCAN_RADIO STRING "1"
-- Retrieval info: PRIVATE: LVDS_MODE_DATA_RATE STRING "504.000"
-- Retrieval info: PRIVATE: LVDS_MODE_DATA_RATE_DIRTY NUMERIC "0"
-- Retrieval info: PRIVATE: LVDS_PHASE_SHIFT_UNIT0 STRING "deg"
-- Retrieval info: PRIVATE: LVDS_PHASE_SHIFT_UNIT6 STRING "deg"
-- Retrieval info: PRIVATE: MIRROR_CLK0 STRING "0"
-- Retrieval info: PRIVATE: MIRROR_CLK6 STRING "0"
-- Retrieval info: PRIVATE: MULT_FACTOR0 NUMERIC "1"
-- Retrieval info: PRIVATE: MULT_FACTOR6 NUMERIC "1"
-- Retrieval info: PRIVATE: NORMAL_MODE_RADIO STRING "1"
-- Retrieval info: PRIVATE: OUTPUT_FREQ0 STRING "132.000"
-- Retrieval info: PRIVATE: OUTPUT_FREQ6 STRING "100.000"
-- Retrieval info: PRIVATE: OUTPUT_FREQ_MODE0 STRING "1"
-- Retrieval info: PRIVATE: OUTPUT_FREQ_MODE6 STRING "0"
-- Retrieval info: PRIVATE: OUTPUT_FREQ_UNIT0 STRING "MHz"
-- Retrieval info: PRIVATE: OUTPUT_FREQ_UNIT6 STRING "MHz"
-- Retrieval info: PRIVATE: PHASE_SHIFT0 STRING "0.00000000"
-- Retrieval info: PRIVATE: PHASE_SHIFT6 STRING "0.00000000"
-- Retrieval info: PRIVATE: PHASE_SHIFT_UNIT0 STRING "deg"
-- Retrieval info: PRIVATE: PHASE_SHIFT_UNIT6 STRING "deg"
-- Retrieval info: PRIVATE: PLL_ADVANCED_PARAM_CHECK STRING "0"
-- Retrieval info: PRIVATE: PLL_ARESET_CHECK STRING "0"
-- Retrieval info: PRIVATE: PLL_AUTOPLL_CHECK NUMERIC "1"
-- Retrieval info: PRIVATE: PLL_ENA_CHECK STRING "0"
-- Retrieval info: PRIVATE: PLL_ENHPLL_CHECK NUMERIC "0"
-- Retrieval info: PRIVATE: PLL_FASTPLL_CHECK NUMERIC "0"
-- Retrieval info: PRIVATE: PLL_LVDS_PLL_CHECK NUMERIC "0"
-- Retrieval info: PRIVATE: PLL_PFDENA_CHECK STRING "0"
-- Retrieval info: PRIVATE: PLL_TARGET_HARCOPY_CHECK NUMERIC "0"
-- Retrieval info: PRIVATE: PRIMARY_CLK_COMBO STRING "inclk0"
-- Retrieval info: PRIVATE: SACN_INPUTS_CHECK STRING "0"
-- Retrieval info: PRIVATE: SCAN_FEATURE_ENABLED STRING "0"
-- Retrieval info: PRIVATE: SHORT_SCAN_RADIO STRING "0"
-- Retrieval info: PRIVATE: SPREAD_FEATURE_ENABLED STRING "0"
-- Retrieval info: PRIVATE: SPREAD_FREQ STRING "50.000"
-- Retrieval info: PRIVATE: SPREAD_FREQ_UNIT STRING "KHz"
-- Retrieval info: PRIVATE: SPREAD_PERCENT STRING "0.500"
-- Retrieval info: PRIVATE: SPREAD_USE STRING "0"
-- Retrieval info: PRIVATE: SRC_SYNCH_COMP_RADIO STRING "0"
-- Retrieval info: PRIVATE: STICKY_CLK0 STRING "1"
-- Retrieval info: PRIVATE: STICKY_CLK6 STRING "0"
-- Retrieval info: PRIVATE: SWITCHOVER_COUNT_EDIT NUMERIC "1"
-- Retrieval info: PRIVATE: SWITCHOVER_FEATURE_ENABLED STRING "0"
-- Retrieval info: PRIVATE: USE_CLK0 STRING "1"
-- Retrieval info: PRIVATE: USE_CLK6 STRING "0"
-- Retrieval info: PRIVATE: USE_CLKENA0 STRING "0"
-- Retrieval info: PRIVATE: USE_CLKENA6 STRING "0"
-- Retrieval info: PRIVATE: ZERO_DELAY_RADIO STRING "0"
-- Retrieval info: LIBRARY: altera_mf altera_mf.altera_mf_components.all
-- Retrieval info: CONSTANT: CLK0_DIVIDE_BY NUMERIC "2"
-- Retrieval info: CONSTANT: CLK0_DUTY_CYCLE NUMERIC "50"
-- Retrieval info: CONSTANT: CLK0_MULTIPLY_BY NUMERIC "11"
-- Retrieval info: CONSTANT: CLK0_PHASE_SHIFT STRING "0"
-- Retrieval info: CONSTANT: COMPENSATE_CLOCK STRING "CLK0"
-- Retrieval info: CONSTANT: EXTCLK0_DIVIDE_BY NUMERIC "2"
-- Retrieval info: CONSTANT: EXTCLK0_DUTY_CYCLE NUMERIC "50"
-- Retrieval info: CONSTANT: EXTCLK0_MULTIPLY_BY NUMERIC "11"
-- Retrieval info: CONSTANT: EXTCLK0_PHASE_SHIFT STRING "5682"
-- Retrieval info: CONSTANT: INCLK0_INPUT_FREQUENCY NUMERIC "41666"
-- Retrieval info: CONSTANT: INTENDED_DEVICE_FAMILY STRING "Cyclone"
-- Retrieval info: CONSTANT: INVALID_LOCK_MULTIPLIER NUMERIC "5"
-- Retrieval info: CONSTANT: LPM_TYPE STRING "altpll"
-- Retrieval info: CONSTANT: OPERATION_MODE STRING "NORMAL"
-- Retrieval info: CONSTANT: PLL_TYPE STRING "AUTO"
-- Retrieval info: CONSTANT: VALID_LOCK_MULTIPLIER NUMERIC "1"
-- Retrieval info: USED_PORT: @clk 0 0 6 0 OUTPUT VCC "@clk[5..0]"
-- Retrieval info: USED_PORT: @extclk 0 0 4 0 OUTPUT VCC "@extclk[3..0]"
-- Retrieval info: USED_PORT: @inclk 0 0 2 0 INPUT VCC "@inclk[1..0]"
-- Retrieval info: USED_PORT: c0 0 0 0 0 OUTPUT VCC "c0"
-- Retrieval info: USED_PORT: e0 0 0 0 0 OUTPUT VCC "e0"
-- Retrieval info: USED_PORT: inclk0 0 0 0 0 INPUT GND "inclk0"
-- Retrieval info: USED_PORT: locked 0 0 0 0 OUTPUT GND "locked"
-- Retrieval info: CONNECT: locked 0 0 0 0 @locked 0 0 0 0
-- Retrieval info: CONNECT: @inclk 0 0 1 0 inclk0 0 0 0 0
-- Retrieval info: CONNECT: c0 0 0 0 0 @clk 0 0 1 0
-- Retrieval info: CONNECT: @inclk 0 0 1 1 GND 0 0 0 0
-- Retrieval info: CONNECT: e0 0 0 0 0 @extclk 0 0 1 0
-- Retrieval info: GEN_FILE: TYPE_NORMAL pll.vhd TRUE FALSE
-- Retrieval info: GEN_FILE: TYPE_NORMAL pll.inc FALSE FALSE
-- Retrieval info: GEN_FILE: TYPE_NORMAL pll.cmp FALSE FALSE
-- Retrieval info: GEN_FILE: TYPE_NORMAL pll.bsf FALSE FALSE
-- Retrieval info: GEN_FILE: TYPE_NORMAL pll_inst.vhd FALSE FALSE
-- Retrieval info: GEN_FILE: TYPE_NORMAL pll_waveforms.html FALSE FALSE
-- Retrieval info: GEN_FILE: TYPE_NORMAL pll_wave*.jpg FALSE FALSE
