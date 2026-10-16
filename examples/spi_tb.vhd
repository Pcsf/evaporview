library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity spi_tb is
end entity;

architecture sim of spi_tb is
  signal clk     : std_logic := '0';
  signal cs_n    : std_logic := '1';
  signal sclk    : std_logic := '0';
  signal mosi    : std_logic := 'Z';
  signal miso    : std_logic;
  signal busy    : std_logic := '0';
  signal bit_cnt : unsigned(2 downto 0) := (others => '0');
  signal tx_byte : std_logic_vector(7 downto 0) := (others => '0');
  signal rx_byte : std_logic_vector(7 downto 0) := (others => 'U');
  signal shreg   : std_logic_vector(7 downto 0) := x"3C";
  signal din     : std_logic := '0';
  signal done    : boolean := false;
begin
  clk <= not clk after 5 ns when not done;

  -- Mode 0 slave: samples MOSI on the rising edge, shifts on the falling edge.
  miso <= shreg(7) when cs_n = '0' else 'Z';
  slave : process (sclk) begin
    if rising_edge(sclk) and cs_n = '0' then
      din <= mosi;
    end if;
    if falling_edge(sclk) and cs_n = '0' then
      shreg <= shreg(6 downto 0) & din;
    end if;
  end process;

  master : process
    procedure transfer (b : std_logic_vector(7 downto 0)) is
    begin
      tx_byte <= b;
      busy    <= '1';
      for i in 7 downto 0 loop
        bit_cnt <= to_unsigned(i, 3);
        mosi    <= b(i);
        wait for 20 ns;
        sclk    <= '1';
        rx_byte <= rx_byte(6 downto 0) & miso;
        wait for 20 ns;
        sclk    <= '0';
      end loop;
      busy <= '0';
    end procedure;
  begin
    wait for 30 ns;
    cs_n <= '0';
    wait for 10 ns;
    transfer(x"A5");
    transfer(x"5A");
    wait for 10 ns;
    cs_n <= '1';
    mosi <= 'Z';
    wait for 50 ns;
    done <= true;
    wait;
  end process;
end architecture;
