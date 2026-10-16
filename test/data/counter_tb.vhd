library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is
  port (clk : in std_logic; rst : in std_logic; cnt : out unsigned(7 downto 0));
end entity;

architecture rtl of counter is
  signal r : unsigned(7 downto 0) := (others => '0');
begin
  process (clk) begin
    if rising_edge(clk) then
      if rst = '1' then r <= (others => '0'); else r <= r + 1; end if;
    end if;
  end process;
  cnt <= r;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter_tb is end entity;

architecture sim of counter_tb is
  signal clk  : std_logic := '0';
  signal rst  : std_logic := '1';
  signal cnt  : unsigned(7 downto 0);
  signal bus4 : std_logic_vector(3 downto 0) := "ZZZZ";
  signal flag : boolean := false;
  signal ana  : real := 0.0;
  signal done : std_logic := 'U';
begin
  clk <= not clk after 5 ns when done /= '1' else clk;
  dut : entity work.counter port map (clk => clk, rst => rst, cnt => cnt);
  stim : process begin
    wait for 12 ns; rst <= '0'; done <= '0';
    bus4 <= "10X1"; wait for 20 ns;
    bus4 <= "0101"; flag <= true; wait for 20 ns;
    for i in 0 to 10 loop ana <= real(i) * 0.25 - 1.0; wait for 7 ns; end loop;
    bus4 <= "ZZZZ"; wait for 30 ns;
    done <= '1'; wait;
  end process;
end architecture;
