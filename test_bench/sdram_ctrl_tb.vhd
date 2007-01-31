LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

entity sdram_ctrl_tb is   
end sdram_ctrl_tb;

architecture structure of sdram_ctrl_tb is	  
	component pll
		port (
			inclk0		: IN STD_LOGIC  := '0';
			c0		: OUT STD_LOGIC ;
			e0		: OUT STD_LOGIC ;
			locked		: OUT STD_LOGIC
			) ;
	end component ;	
	component sdram_ctrl is 
		port(		
			signal clk : IN STD_LOGIC;
			signal reset : IN STD_LOGIC;
			
			signal avs_nios_chipselect : IN STD_LOGIC;
			signal avs_nios_address : IN STD_LOGIC_VECTOR (21 DOWNTO 0);
			signal avs_nios_byteenable : IN STD_LOGIC_VECTOR (3 DOWNTO 0);
			signal avs_nios_writedata : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
			signal avs_nios_write : IN STD_LOGIC;
			signal avs_nios_read : IN STD_LOGIC;
			signal avs_nios_readdata : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
			signal avs_nios_readdatavalid : OUT STD_LOGIC;
			signal avs_nios_waitrequest : OUT STD_LOGIC;	
			
			signal sdram_addr : OUT STD_LOGIC_VECTOR (11 DOWNTO 0);
			signal sdram_ba : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
			signal sdram_cas_n : OUT STD_LOGIC;
			signal sdram_cke : OUT STD_LOGIC;
			signal sdram_cs_n : OUT STD_LOGIC_VECTOR(0 downto 0);
			signal sdram_dq : INOUT STD_LOGIC_VECTOR (31 DOWNTO 0);
			signal sdram_dqm : OUT STD_LOGIC_VECTOR (3 DOWNTO 0);
			signal sdram_ras_n : OUT STD_LOGIC;
			signal sdram_we_n : OUT STD_LOGIC
			);
	end component;
	
	component mt48lc4m32b2 IS
		PORT (
			Dq    : INOUT STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => 'Z');
			Addr  : IN    STD_LOGIC_VECTOR (11 DOWNTO 0) := (OTHERS => '0');
			Ba    : IN    STD_LOGIC_VECTOR (1 DOWNTO 0) := "00";
			Clk   : IN    STD_LOGIC := '0';
			Cke   : IN    STD_LOGIC := '1';
			Cs_n  : IN    STD_LOGIC := '1';
			Ras_n : IN    STD_LOGIC := '1';
			Cas_n : IN    STD_LOGIC := '1';
			We_n  : IN    STD_LOGIC := '1';
			Dqm   : IN    STD_LOGIC_VECTOR (3 DOWNTO 0) := "0000"
			);
	END component; 
	
	component cpu_simulator IS
		PORT(clk, reset: IN std_logic;
			address : OUT std_logic_vector(21 downto 0):=(others=>'0');
			writedata : out std_logic_vector(31 downto 0):=(others=>'0');
			byteenable : out std_logic_vector(3 downto 0):=(others=>'0');
			write : out std_logic:='0';
			read : out std_logic:='0';
			readdata : in std_logic_vector(31 downto 0);
			waitrequest : in std_logic;
			readdatavalid : in std_logic
			);
	END component;	
	
	signal reset, clk_ok: std_logic;
	signal run_nios_n : std_logic:='1';
	signal clk,i_clk: std_logic:='0';
	
	signal SDRAM_CLK :  std_logic;
	signal SDRAM_CKE :  std_logic;
	signal SDRAM_NCS :  std_logic_vector(0 downto 0);
	signal SDRAM_NRAS :  std_logic;
	signal SDRAM_NCAS :  std_logic;
	signal SDRAM_NWE :  std_logic;
	signal SDRAM_ADDRESS :  std_logic_vector(11 downto 0);
	signal SDRAM_BANK :  std_logic_vector(1 downto 0);
	signal SDRAM_DQM :  std_logic_vector(3 downto 0);
	signal SDRAM_DATA :  std_logic_vector(31 downto 0);		
	
	signal address : std_logic_vector(21 downto 0);
	signal writedata : std_logic_vector(31 downto 0):=(others=>'0');
	signal byteenable : std_logic_vector(3 downto 0):=(others=>'0');
	signal write : std_logic;
	signal read : std_logic; 
	signal chipselect : std_logic:='1';
	signal readdata : std_logic_vector(31 downto 0);
	signal waitrequest : std_logic;
	signal readdatavalid : std_logic;  
	
begin  	 
	
	reset<= not clk_ok;
	run_nios_n<= reset after 140 us;		
	
	UUT: sdram_ctrl
	port map(		
		clk => i_clk,
		reset => reset,
		
		avs_nios_chipselect => chipselect,
		avs_nios_address => address,
		avs_nios_byteenable => byteenable,
		avs_nios_writedata => writedata,
		avs_nios_write => write,
		avs_nios_read => read,
		avs_nios_readdata => readdata,
		avs_nios_readdatavalid => readdatavalid,
		avs_nios_waitrequest => waitrequest,
		
		sdram_addr => SDRAM_ADDRESS,
		sdram_ba => SDRAM_BANK,
		sdram_cas_n => SDRAM_NCAS,
		sdram_cke => SDRAM_CKE,
		sdram_cs_n => SDRAM_NCS,
		sdram_dq => SDRAM_DATA,
		sdram_dqm => SDRAM_DQM,
		sdram_ras_n => SDRAM_NRAS,
		sdram_we_n => SDRAM_NWE
		);
	
	sdram:mt48lc4m32b2 
	PORT MAP(
		Dq => SDRAM_DATA,
		Addr => SDRAM_ADDRESS,
		Ba  => SDRAM_BANK,
		Clk  => SDRAM_CLK,
		Cke => SDRAM_CKE,
		Cs_n => SDRAM_NCS(0),
		Ras_n => SDRAM_NRAS,
		Cas_n  => SDRAM_NCAS,
		We_n => SDRAM_NWE,
		Dqm  => SDRAM_DQM
		);	 
	
	cpu: cpu_simulator
	PORT MAP(
		clk	=> i_clk,
		reset => run_nios_n,
		address	=> address,
		writedata => writedata,
		byteenable => byteenable,
		write => write,
		read => read,
		readdata => readdata,
		waitrequest	=> waitrequest,
		readdatavalid	=> readdatavalid
		);
	
	U1 : pll
	port map(
		inclk0 => clk,
		c0 => i_clk,
		e0 => SDRAM_CLK,
		locked =>clk_ok
		);
	
	clock_generator:process
	begin
		wait for 21 ns;
		clk<= clk xor '1';
	end process;
end structure;
