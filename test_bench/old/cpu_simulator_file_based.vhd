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
	signal address: std_logic_vector(21 downto 0);
	signal writedata: std_logic_vector(31 downto 0);
	signal byteenable: std_logic_vector(3 downto 0);
	signal write: std_logic;
	signal read: std_logic;
	
BEGIN		   
	process(clk,reset) 
		variable service: std_logic_vector(7 downto 0);
		variable b0,b1,b2,b3,b4,b5,b6,b7: BYTE;
	begin
		if reset='1'
			then
			file_close(NIOS);
			file_open(NIOS, FILES_PATH&"nios.dat", READ_MODE);
		elsif rising_edge(clk)
			then
			if waitrequest='0'
				then 
				if Endfile(NIOS)
					then	
					file_close(NIOS);
					file_open(NIOS, FILES_PATH&"nios.dat", READ_MODE);	
				else	
					Read(NIOS,b0);
					Read(NIOS,b1);
					Read(NIOS,b2);
					Read(NIOS,b3);
					Read(NIOS,b4);
					Read(NIOS,b5);
					Read(NIOS,b6);	
					Read(NIOS,b7);	
					service:=char2std_logic_vector(b0);
					address<=service(5 downto 0)&char2std_logic_vector(b1)&char2std_logic_vector(b2);
					writedata<=char2std_logic_vector(b3)&char2std_logic_vector(b4)&char2std_logic_vector(b5)&char2std_logic_vector(b6);
					service:=char2std_logic_vector(b7);
					byteenable<=not service(7 downto 4);
					write<=service(3);
					read<=service(2); 
				end if;
			end if;	
		end if;	
	end process;
	
	process(clk,reset)
	begin
		if reset='1'
			then
			readed<=(others=>'0');
			new_data<='0';
		elsif rising_edge(clk)
			then
			new_data<=readdatavalid;
			if readdatavalid='1'
				then
				readed<=readdata;
			end if;	
		end if;	
	end process;	
END behaviour;	