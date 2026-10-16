library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity uart_tb is
end entity;

architecture sim of uart_tb is
  constant BIT_T : time := 80 ns;
  constant MSG   : string := "Hi!";
  signal tx      : std_logic := '1';
  signal busy    : std_logic := '0';
  signal tx_data : std_logic_vector(7 downto 0) := (others => '0');
  signal bit_idx : unsigned(3 downto 0) := (others => '0');
begin
  stim : process
  begin
    wait for 200 ns;
    for c in MSG'range loop
      tx_data <= std_logic_vector(to_unsigned(character'pos(MSG(c)), 8));
      busy    <= '1';
      bit_idx <= x"0";
      tx      <= '0';
      wait for BIT_T;
      for i in 0 to 7 loop
        bit_idx <= to_unsigned(i + 1, 4);
        tx      <= tx_data(i);
        wait for BIT_T;
      end loop;
      bit_idx <= x"9";
      tx      <= '1';
      wait for BIT_T;
      busy <= '0';
      wait for 2 * BIT_T;
    end loop;
    wait;
  end process;
end architecture;
