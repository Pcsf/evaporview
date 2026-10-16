library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use ieee.float_pkg.all;

entity nco_tb is
end entity;

architecture sim of nco_tb is
  constant STEP : natural := 1311;
  signal clk     : std_logic := '0';
  signal phase   : unsigned(15 downto 0) := (others => '0');
  signal amp     : real := 0.25;
  signal sine    : real := 0.0;
  signal sample  : signed(11 downto 0) := (others => '0');
  signal sample_f32 : std_logic_vector(31 downto 0) := (others => '0');
  signal done    : boolean := false;
begin
  clk <= not clk after 5 ns when not done;

  process (clk)
    variable s : real;
  begin
    if rising_edge(clk) then
      phase <= phase + STEP;
      s := amp * sin(MATH_2_PI * real(to_integer(phase)) / 65536.0);
      sine   <= s;
      sample <= to_signed(integer(round(s * 2047.0)), 12);
      sample_f32 <= to_slv(to_float(s, 8, 23));
    end if;
  end process;

  process
  begin
    for k in 1 to 3 loop
      wait for 1000 ns;
      amp <= amp + 0.25;
    end loop;
    wait for 1000 ns;
    done <= true;
    wait;
  end process;
end architecture;
