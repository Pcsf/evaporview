library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
entity rec_tb is end;
architecture sim of rec_tb is
  type reg_t is record cnt : unsigned(3 downto 0); busy : std_logic; end record;
  signal clk : std_logic := '0';
  signal rst : std_logic := '1';
  signal r, rin : reg_t := (cnt => (others => '0'), busy => '0');
begin
  clk <= not clk after 5 ns;
  rst <= '0' after 12 ns;
  comb : process (r) begin rin <= r; rin.cnt <= r.cnt + 1; rin.busy <= not r.busy; end process;
  seq : process (clk) begin if rising_edge(clk) then if rst = '1' then r.cnt <= (others => '0'); else r <= rin; end if; end if; end process;
end;
