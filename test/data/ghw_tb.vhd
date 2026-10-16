library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ghw_leaf is
  port (clk : in std_logic; q : out std_logic_vector(1 downto 0));
end;

architecture sim of ghw_leaf is
begin
  process (clk)
    variable n : unsigned(1 downto 0) := "00";
  begin
    if rising_edge(clk) then
      n := n + 1;
      q <= std_logic_vector(n);
    end if;
  end process;
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ghw_tb is end;

architecture sim of ghw_tb is
  type state_t is (IDLE, LOAD, SHIFT, DONE);
  type inner_t is record
    flag : std_logic;
    word : std_logic_vector(3 downto 0);
  end record;
  type outer_t is record
    count : unsigned(2 downto 0);
    inner : inner_t;
    ok    : boolean;
  end record;
  type mem_t is array (0 to 3) of std_logic_vector(7 downto 0);

  signal clk   : std_logic := '0';
  signal sl    : std_logic := 'U';
  signal slv_d : std_logic_vector(3 downto 0) := "0000";
  signal slv_t : std_logic_vector(0 to 3) := "0001";
  signal u8    : unsigned(7 downto 0) := x"00";
  signal s8    : signed(7 downto 0) := x"00";
  signal int   : integer := 0;
  signal nat   : natural range 0 to 15 := 0;
  signal re    : real := 0.0;
  signal flag  : boolean := false;
  signal b     : bit := '0';
  signal state : state_t := IDLE;
  signal rec   : outer_t := (count => "000", inner => (flag => '0', word => "0000"), ok => false);
  signal mem   : mem_t := (others => x"00");
  signal tm    : time := 0 ns;
  signal q     : std_logic_vector(1 downto 0);
begin
  clk <= not clk after 5 ns;

  leaf : entity work.ghw_leaf port map (clk => clk, q => q);

  gen : for i in 0 to 1 generate
    signal g : std_logic := '0';
  begin
    g <= '1' after (10 + 10 * i) * 1 ns;
  end generate;

  stim : process
  begin
    wait for 10 ns;
    sl <= '1'; slv_d <= "1010"; slv_t <= "1000"; u8 <= x"A5"; s8 <= x"F0";
    int <= -3; nat <= 7; re <= 1.5; flag <= true; b <= '1'; state <= LOAD;
    rec.count <= "101"; rec.inner.flag <= '1'; rec.inner.word <= "1100"; rec.ok <= true;
    mem(2) <= x"5A"; tm <= 3 ns;
    wait for 10 ns;
    sl <= 'Z'; slv_d <= "10Z1"; int <= 100000; re <= -0.25; state <= SHIFT;
    mem(0) <= x"01"; mem(2) <= x"FF"; rec.inner.word <= "0011";
    wait for 0 ns;
    sl <= 'X';
    wait for 10 ns;
    int <= 2147483647; state <= DONE; s8 <= x"80"; nat <= 15; flag <= false;
    wait for 10 ns;
    int <= -2147483648; slv_d <= "0000"; state <= IDLE; sl <= '0';
    wait for 10 ns;
    wait;
  end process;
end;
