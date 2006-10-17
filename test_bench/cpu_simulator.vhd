LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL; 

ENTITY cpu_simulator IS
	PORT(clk, reset: in std_logic;
		
		address : out std_logic_vector(21 downto 0);
		writedata : out std_logic_vector(31 downto 0);
		byteenable : out std_logic_vector(3 downto 0);
		write : out std_logic;
		read : out std_logic;
		readdata : in std_logic_vector(31 downto 0);
		waitrequest : in std_logic;
		readdatavalid : in std_logic
		);
END cpu_simulator;	

ARCHITECTURE behaviour OF cpu_simulator IS 
	signal counter: unsigned(3 downto 0);
	type vector is
	record
		address: integer;
		writedata: integer;
		byteenable: std_logic_vector(3 downto 0);
		write: std_logic;
		read: std_logic;
	end record;	  
	type script is array(0 to (2**counter'length)-1) of vector;
	signal wave_form: script:=(
		(0,0,"1111",'1','0'),
		(1,1,"1111",'1','0'),
		(2,2,"1111",'1','0'),
		(3,3,"1111",'1','0'),
		(1024,1024,"1111",'0','1'),
		(1025,1025,"1111",'0','1'),
		(1026,1026,"1111",'0','1'),
		(1027,1027,"1111",'0','1'),  
		(1024,1024,"1111",'1','0'),
		(1025,1025,"1111",'1','0'),
		(1026,1026,"1111",'1','0'),
		(1027,1027,"1111",'1','0'),
		(0,0,"1111",'0','1'),
		(1,1,"1111",'0','1'),
		(2,2,"1111",'0','1'),
		(3,3,"1111",'0','1')
	);
BEGIN		   
	process(clk,reset) 
	begin
		if reset='1'
			then
			counter<=(others=>'0');	 
			address<=(others=>'0');	 
			writedata<=(others=>'0');	 
			byteenable<=(others=>'0');	 
			write<='0';	 
			read<='0';	 
		elsif rising_edge(clk)
			then
			if waitrequest='0'
				then  
				counter<=counter+1;	 
				address<=std_logic_vector(to_unsigned(wave_form(to_integer(counter)).address,address'length));	 
				writedata<=std_logic_vector(to_unsigned(wave_form(to_integer(counter)).writedata,writedata'length));	 
				byteenable<=wave_form(to_integer(counter)).byteenable;	 
				write<=wave_form(to_integer(counter)).write;	 
				read<=wave_form(to_integer(counter)).read;	 
			end if;	
		end if;	
	end process;
END behaviour;	