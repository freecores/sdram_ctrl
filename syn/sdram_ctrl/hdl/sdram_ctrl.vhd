------------------------------------------------------------------
--                                                              
-- sdram_ctrl.vhd                                          
--                                                                                          
-- Module Description:                                          
-- SDRAM small&fast controller
-- 
--                                                              
-- To Do:                                                       
-- configurable times 
-- nios simulation support
--                                                              
-- Author(s):                                                   
-- Aleksey Kuzmenok, ntpqa@opencores.org                      
--                                                              
------------------------------------------------------------------
--                                                              
-- Copyright (C) 2006 Aleksey Kuzmenok and OPENCORES.ORG        
--                                                              
-- This module is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This module is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this software; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA            
--                                                              
------------------------------------------------------------------
-- Test results																					
-- FPGA                     SDRAM			         CLK (not less than)
-- EP1C12XXXXC8             MT48LC4M32B2TG-7:G		 125 MHz
-- EP1C6XXXXC8              IS42S16100C1-7TL		 125 MHz   
--
------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
LIBRARY altera_mf;
USE altera_mf.altera_mf_components.all;

entity sdram_ctrl is 
	generic(  
		DATA_WIDTH: integer:=32;
		BANK_WIDTH: integer:=4;
		ROW_WIDTH: integer:=12;
		COLUMN_WIDTH: integer:=8; 
		
		clk_MHz: integer:=120
		);
	port(		
		signal clk : IN STD_LOGIC;
		signal reset : IN STD_LOGIC;
		
		-- IMPORTANT: for this Avalon(tm) interface 
		-- 'Minimum Arbitration Shares'=1 
		-- 'Max Pending Read Transactions'=9
		signal avs_nios_chipselect : IN STD_LOGIC;
		signal avs_nios_address : IN STD_LOGIC_VECTOR ((BANK_WIDTH+ROW_WIDTH+COLUMN_WIDTH-1) DOWNTO 0);
		signal avs_nios_byteenable : IN STD_LOGIC_VECTOR (((DATA_WIDTH/8)-1) DOWNTO 0);
		signal avs_nios_writedata : IN STD_LOGIC_VECTOR ((DATA_WIDTH-1) DOWNTO 0);
		signal avs_nios_write : IN STD_LOGIC;
		signal avs_nios_read : IN STD_LOGIC; 
		signal avs_nios_waitrequest : OUT STD_LOGIC;
		signal avs_nios_readdata : OUT STD_LOGIC_VECTOR ((DATA_WIDTH-1) DOWNTO 0);
		signal avs_nios_readdatavalid : OUT STD_LOGIC;	
		
		-- global export signals
		signal sdram_cke : OUT STD_LOGIC;                       -- This pin has fixed state '1'
		signal sdram_ba : OUT STD_LOGIC_VECTOR ((BANK_WIDTH-1) DOWNTO 0);
		signal sdram_addr : OUT STD_LOGIC_VECTOR ((ROW_WIDTH-1) DOWNTO 0);
		signal sdram_cs_n : OUT STD_LOGIC;                      -- This pin has fixed state '0'
		signal sdram_ras_n : OUT STD_LOGIC;
		signal sdram_cas_n : OUT STD_LOGIC;	 
		signal sdram_we_n : OUT STD_LOGIC;
		signal sdram_dq : INOUT STD_LOGIC_VECTOR ((DATA_WIDTH-1) DOWNTO 0);
		signal sdram_dqm : OUT STD_LOGIC_VECTOR (((DATA_WIDTH/8)-1) DOWNTO 0)
		);
end sdram_ctrl;

architecture behaviour of sdram_ctrl is
	
	CONSTANT FIFO_WIDTH: integer:=BANK_WIDTH+ROW_WIDTH+COLUMN_WIDTH+DATA_WIDTH+(DATA_WIDTH/8)+2;     
	
	CONSTANT MODE: std_logic_vector(11 downto 0):="000000110000";	-- 1 word burst, CAS latency=3
	-- Only two times are configurable
	-- tINIT delay between powerup and load mode register = 100 us 
	-- tREF refresh period = 15.625 us  (64ms/4096rows)
	CONSTANT INIT_PAUSE_CLOCKS: integer:=(clk_MHz*10000)/91;	-- 109.9 us just to be on the save side
	CONSTANT REFRESH_PERIOD_CLOCKS: integer:=(clk_MHz*1000)/65;	-- 15.384 us the same purpose
	CONSTANT CAS_LATENCY: integer:=3;                               -- other latencies weren't been tested!
	
	COMPONENT scfifo
		GENERIC (
			add_ram_output_register		: STRING;
			intended_device_family		: STRING;
			lpm_numwords		: NATURAL;
			lpm_showahead		: STRING;
			lpm_type		: STRING;
			lpm_width		: NATURAL;
			lpm_widthu		: NATURAL;
			overflow_checking		: STRING;
			underflow_checking		: STRING;
			use_eab		: STRING
			);
		PORT (
			rdreq	: IN STD_LOGIC ;
			empty	: OUT STD_LOGIC ;
			aclr	: IN STD_LOGIC ;
			clock	: IN STD_LOGIC ;
			q	: OUT STD_LOGIC_VECTOR ((FIFO_WIDTH-1) DOWNTO 0);
			wrreq	: IN STD_LOGIC ;
			data	: IN STD_LOGIC_VECTOR ((FIFO_WIDTH-1) DOWNTO 0);
			full	: OUT STD_LOGIC 
			);
	END COMPONENT;
	
	-- If you ask me why there are so many states, I'll answer that all times are fixed.
	-- The top speed for MT48LC4M32B2TG-7:G is 7 ns, therefore all times were based on it.
	-- tRP PRECHARGE command period = 3 clocks
	-- tRFC AUTO REFRESH period = 10 clocks 
	-- tMRD LOAD MODE REGISTER command to ACTIVE or REFRESH command = 2 clocks
	-- tRCD ACTIVE to READ or WRITE delay = 3 clocks
	-- tRAS ACTIVE to PRECHARGE command = 7 clocks 
	-- tRC ACTIVE to ACTIVE command period = 10 clocks
	-- tWR2 Write recovery time = 2 clocks
	type states is (
	INIT0,INIT1,INIT2,INIT3,INIT4,INIT5,INIT6,INIT7,                         
	INIT8,INIT9,INIT10,INIT11,INIT12,INIT13,INIT14,INIT15,
	INIT16,INIT17,INIT18,INIT19,INIT20,INIT21,INIT22,INIT23,INIT24,
	REFRESH0,REFRESH1,REFRESH2,REFRESH3,REFRESH4,REFRESH5,REFRESH6,REFRESH7,
	REFRESH8,REFRESH9,REFRESH10,REFRESH11,REFRESH12,REFRESH13,REFRESH14,
	ACTIVE0,ACTIVE1,ACTIVE2,ACTIVE3,ACTIVE4,
	IDLE,READ0,WRITE0);
	signal operation: states;	  
	
	signal fifo_q: std_logic_vector((FIFO_WIDTH-1) downto 0);
	
	signal init_counter: unsigned(15 downto 0):=to_unsigned(INIT_PAUSE_CLOCKS,16); 
	signal refresh_counter: unsigned(15 downto 0);
	signal active_counter: unsigned(2 downto 0);  	 	 
	signal active_address: unsigned((BANK_WIDTH+ROW_WIDTH-1) downto 0);
	
	signal bank: std_logic_vector((sdram_ba'length-1) downto 0); 
	signal row: std_logic_vector((sdram_addr'length-1) downto 0);
	signal column: std_logic_vector((COLUMN_WIDTH-1) downto 0);
	signal be: std_logic_vector((sdram_dqm'length-1) downto 0);  
	signal data: std_logic_vector((sdram_dq'length-1) downto 0);
	
	signal do_init,do_refresh,do_active,read_is_active,ready,tRCD_not_expired: std_logic:='0';
	
	signal fifo_rdreq,fifo_empty: std_logic;
	
	signal read_latency: std_logic_vector(CAS_LATENCY downto 0);	
	
	signal fifo_data: std_logic_vector((FIFO_WIDTH-1) downto 0);	
	signal fifo_wrreq,fifo_wrfull: std_logic;	
	
	signal i_command : STD_LOGIC_VECTOR(4 downto 0); 
	CONSTANT NOP: STD_LOGIC_VECTOR(4 downto 0):="10111";
	CONSTANT ACTIVE: STD_LOGIC_VECTOR(4 downto 0):="10011";
	CONSTANT READ: STD_LOGIC_VECTOR(4 downto 0):="10101";
	CONSTANT WRITE: STD_LOGIC_VECTOR(4 downto 0):="10100";
	CONSTANT PRECHARGE: STD_LOGIC_VECTOR(4 downto 0):="10010";
	CONSTANT AUTO_REFRESH: STD_LOGIC_VECTOR(4 downto 0):="10001";
	CONSTANT LOAD_MODE_REGISTER: STD_LOGIC_VECTOR(4 downto 0):="10000";
	
	signal i_address : STD_LOGIC_VECTOR((sdram_addr'length-1) DOWNTO 0);
	signal i_bank : STD_LOGIC_VECTOR((sdram_ba'length-1) DOWNTO 0);
	signal i_dqm : STD_LOGIC_VECTOR((sdram_dqm'length-1) DOWNTO 0);
	signal i_data : STD_LOGIC_VECTOR((sdram_dq'length-1) DOWNTO 0);
	attribute ALTERA_ATTRIBUTE : string;
	attribute ALTERA_ATTRIBUTE of i_command : signal is "FAST_OUTPUT_REGISTER=ON";
	attribute ALTERA_ATTRIBUTE of i_address : signal is "FAST_OUTPUT_REGISTER=ON";
	attribute ALTERA_ATTRIBUTE of i_bank : signal is "FAST_OUTPUT_REGISTER=ON";
	attribute ALTERA_ATTRIBUTE of i_dqm : signal is "FAST_OUTPUT_REGISTER=ON";
	attribute ALTERA_ATTRIBUTE of i_data : signal is "FAST_OUTPUT_REGISTER=ON";	 
	attribute ALTERA_ATTRIBUTE of avs_nios_readdata : signal is "FAST_INPUT_REGISTER=ON";
begin
	(sdram_cke,sdram_cs_n,sdram_ras_n,sdram_cas_n,sdram_we_n) <= i_command;
	sdram_addr <= i_address;
	sdram_ba <= i_bank;
	sdram_dqm <= i_dqm;
	sdram_dq <= i_data;
	
	fifo_data<=avs_nios_address & avs_nios_writedata & avs_nios_byteenable & avs_nios_write & avs_nios_read;	 
	fifo_wrreq<=(avs_nios_write or avs_nios_read) and avs_nios_chipselect and not fifo_wrfull;
	
	avs_nios_waitrequest<=fifo_wrfull;   
	
	fifo_rdreq<=not fifo_empty and not do_refresh and not do_active and not read_is_active and ready;
	
	do_active<='0' when active_address=unsigned((fifo_q((fifo_q'length-1) downto (fifo_q'length-bank'length-row'length)))) else '1';	
	read_is_active<='1' when read_latency(CAS_LATENCY-1 downto 0)>"000" and fifo_q(1)='1' else '0';
	ready<='1' when operation=IDLE or operation=READ0 or operation=WRITE0 else '0';
	
	operation_machine:process(reset,clk)
	begin
		if reset='1'
			then
			operation<=INIT0;  	
			active_address<=(others=>'1');
		elsif rising_edge(clk)
			then   
			
			bank<=fifo_q((fifo_q'length-1) downto (fifo_q'length-bank'length));
			row<=fifo_q((fifo_q'length-bank'length-1) downto (fifo_q'length-bank'length-row'length));
			column<=fifo_q((column'length+data'length+be'length+2-1) downto (data'length+be'length+2));
			data<=fifo_q((data'length+be'length+2-1) downto (be'length+2));
			be<=fifo_q((be'length+2-1) downto 2);
			
			case operation is 
				when INIT0=> 
				if do_init='1' 
					then operation<=INIT1;row(10)<='1';
				end if;   
				when INIT1=>operation<=INIT2;
				when INIT2=>operation<=INIT3;
				when INIT3=>operation<=INIT4;
				when INIT4=>operation<=INIT5;
				when INIT5=>operation<=INIT6;
				when INIT6=>operation<=INIT7;
				when INIT7=>operation<=INIT8;
				when INIT8=>operation<=INIT9;
				when INIT9=>operation<=INIT10;
				when INIT10=>operation<=INIT11;
				when INIT11=>operation<=INIT12;
				when INIT12=>operation<=INIT13;	
				when INIT13=>operation<=INIT14;
				when INIT14=>operation<=INIT15;
				when INIT15=>operation<=INIT16;
				when INIT16=>operation<=INIT17;
				when INIT17=>operation<=INIT18;
				when INIT18=>operation<=INIT19;
				when INIT19=>operation<=INIT20;
				when INIT20=>operation<=INIT21;
				when INIT21=>operation<=INIT22;
				when INIT22=>operation<=INIT23;
				when INIT23=>operation<=INIT24;row<=MODE((row'length-1) downto 0);
				when INIT24=>operation<=IDLE;	
				
				when REFRESH0=>operation<=REFRESH1;		 
				when REFRESH1=>operation<=REFRESH2;	 
				when REFRESH2=>operation<=REFRESH3; 
				when REFRESH3=>operation<=REFRESH4;	 
				when REFRESH4=>operation<=REFRESH5;	 
				when REFRESH5=>operation<=REFRESH6;	 
				when REFRESH6=>operation<=REFRESH7;	 
				when REFRESH7=>operation<=REFRESH8;	 
				when REFRESH8=>operation<=REFRESH9;
				when REFRESH9=>operation<=REFRESH10;	 
				when REFRESH10=>operation<=REFRESH11;
				when REFRESH11=>operation<=REFRESH12;
				when REFRESH12=>operation<=REFRESH13;
				active_address<=unsigned(fifo_q((fifo_q'length-1) downto (fifo_q'length-bank'length-row'length)));
				when REFRESH13=>operation<=REFRESH14;	
				when REFRESH14=>operation<=IDLE;          	
				
				when ACTIVE0=>operation<=ACTIVE1;	 
				when ACTIVE1=>operation<=ACTIVE2;	  
				when ACTIVE2=>operation<=ACTIVE3;	
				active_address<=unsigned(fifo_q((fifo_q'length-1) downto (fifo_q'length-bank'length-row'length)));
				when ACTIVE3=>operation<=ACTIVE4;
				when ACTIVE4=>operation<=IDLE;    	   
				
				when others=>  
				if do_refresh='1'
					then
					if tRCD_not_expired='0' and operation=IDLE
						then operation<=REFRESH0;row(10)<='1';
					else operation<=IDLE;
					end if;	
				elsif do_active='1'
					then
					if tRCD_not_expired='0' and operation=IDLE	
						then operation<=ACTIVE0;row(10)<='1';
					else operation<=IDLE;
					end if;	
				elsif fifo_empty='1'
					then
					operation<=IDLE;	
				elsif fifo_q(1)='1' --write
					then  
					if read_latency(CAS_LATENCY-1 downto 0)>"000"
						then operation<=IDLE;
					else operation<=WRITE0; 
					end if;	
				elsif fifo_q(0)='1'	--read
					then
					operation<=READ0;
				end if;	
			end case;
		end if;	
	end process;  			
	
	control_latency:process(reset,clk)
	begin
		if reset='1'
			then 
			read_latency<=(others=>'0');
		elsif rising_edge(clk)
			then 
			read_latency<=std_logic_vector(unsigned(read_latency) SLL 1);
			if operation=READ0
				then read_latency(0)<='1';
			else read_latency(0)<='0';	
			end if;	
		end if;	
	end process; 	 
	latch_readdata:process(reset,clk)
	begin
		if reset='1'
			then
			avs_nios_readdata<=(others=>'0');
			avs_nios_readdatavalid<='0';
		elsif rising_edge(clk)
			then
			avs_nios_readdata<=sdram_dq;
			avs_nios_readdatavalid<=read_latency(CAS_LATENCY);
		end if;	
	end process;
	initialization:process(reset,clk)
	begin
		if rising_edge(clk)
			then	
			if init_counter>0
				then
				init_counter<=init_counter-1; 
			else do_init<='1';	
			end if;
		end if;	
	end process;	
	refreshing:process(clk,reset)
	begin	 
		if reset='1'
			then
			refresh_counter<=to_unsigned(REFRESH_PERIOD_CLOCKS,16);	
			do_refresh<='0';
		elsif rising_edge(clk)
			then						 
			if refresh_counter=to_unsigned(0,refresh_counter'length)
				then refresh_counter<=to_unsigned(REFRESH_PERIOD_CLOCKS,16);	
				do_refresh<='1';
			else refresh_counter<=refresh_counter-1;
			end if;
			if operation=REFRESH0 or operation=REFRESH5
				then do_refresh<='0';
			end if;
		end if;	
	end process;
	active_period:process(reset,clk)
	begin
		if reset='1'
			then
			active_counter<=(others=>'0');	
			tRCD_not_expired<='0';
		elsif rising_edge(clk)
			then	
			if operation=ACTIVE3 or operation=REFRESH13
				then active_counter<=to_unsigned(5,active_counter'length);
			elsif active_counter>0
				then active_counter<=active_counter-1;
			end if;	
		end if;
		if active_counter>0
			then tRCD_not_expired<='1';
		else tRCD_not_expired<='0';
		end if;	
	end process;  
	latch_controls:process(clk,reset)
	begin	 			
		if reset='1'
			then	 
			i_command<=NOP;
			i_address<=(others=>'0');
			i_bank<=(others=>'0');
			i_dqm<=(others=>'0');
			i_data<=(others=>'Z');
		elsif rising_edge(clk)
			then
			i_command<=NOP;
			i_bank<=bank;
			i_address<=(others=>'0');
			i_address((column'length-1) downto 0)<=column;
			i_data<=(others=>'Z');
			i_dqm<=(others=>'0'); 	   
			
			case operation is	
				when INIT1|REFRESH0|ACTIVE0 =>  
				i_command<=PRECHARGE;
				i_address<=row;
				when INIT4|INIT14|REFRESH3 =>   
				i_command<=AUTO_REFRESH;
				when INIT24=> 
				i_command<=LOAD_MODE_REGISTER;
				i_address<=row;
				when ACTIVE3|REFRESH13 => 
				i_command<=ACTIVE;
				i_address<=row;
				when READ0 =>
				i_command<=READ;
				when WRITE0 => 	
				i_command<=WRITE;
				i_dqm<=not be; 
				i_data<=data;
				when OTHERS => 
			end case;
		end if;
	end process;
	
	fifo: scfifo
	GENERIC MAP (
		add_ram_output_register => "ON",
		intended_device_family => "Cyclone",
		lpm_numwords => 4,
		lpm_showahead => "ON",
		lpm_type => "scfifo",
		lpm_width => FIFO_WIDTH,
		lpm_widthu => 2,
		overflow_checking => "ON",
		underflow_checking => "ON",
		use_eab => "ON"
		)
	PORT MAP (
		rdreq => fifo_rdreq,
		aclr => reset,
		clock => clk,
		wrreq => fifo_wrreq,
		data => fifo_data,
		empty => fifo_empty,
		q => fifo_q,
		full => fifo_wrfull
		);
end behaviour;
