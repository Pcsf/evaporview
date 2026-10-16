library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity blink_ctrl is
  port (clk   : in  std_logic;
        rst   : in  std_logic;
        start : in  std_logic;
        led   : out std_logic;
        done  : out std_logic);
end entity;

architecture rtl of blink_ctrl is
  type state_t is (IDLE, LED_ON, LED_OFF, FINISHED);
  type reg_t is record
    state  : state_t;
    count  : unsigned(7 downto 0);
    blinks : natural range 0 to 7;
    led    : std_logic;
  end record;
  constant REG_RESET : reg_t := (state => IDLE, count => (others => '0'), blinks => 0, led => '0');
  signal r, rin : reg_t := REG_RESET;
begin
  comb : process (r, start)
    variable v : reg_t;
  begin
    v := r;
    case r.state is
      when IDLE =>
        if start = '1' then
          v.state := LED_ON;
          v.led   := '1';
        end if;
      when LED_ON =>
        v.count := r.count + 1;
        if r.count = 9 then
          v.count := (others => '0');
          v.state := LED_OFF;
          v.led   := '0';
        end if;
      when LED_OFF =>
        v.count := r.count + 1;
        if r.count = 9 then
          v.count := (others => '0');
          if r.blinks = 2 then
            v.state := FINISHED;
          else
            v.blinks := r.blinks + 1;
            v.state  := LED_ON;
            v.led    := '1';
          end if;
        end if;
      when FINISHED =>
        null;
    end case;
    rin <= v;
  end process;

  seq : process (clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        r <= REG_RESET;
      else
        r <= rin;
      end if;
    end if;
  end process;

  led  <= r.led;
  done <= '1' when r.state = FINISHED else '0';
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity fsm_tb is
end entity;

architecture sim of fsm_tb is
  signal clk, led, done : std_logic := '0';
  signal rst   : std_logic := '1';
  signal start : std_logic := '0';
  signal stop  : boolean := false;
begin
  clk <= not clk after 5 ns when not stop;
  dut : entity work.blink_ctrl port map (clk => clk, rst => rst, start => start, led => led, done => done);
  stim : process
  begin
    wait for 25 ns;
    rst <= '0';
    wait for 30 ns;
    start <= '1';
    wait for 10 ns;
    start <= '0';
    wait until done = '1';
    wait for 50 ns;
    stop <= true;
    wait;
  end process;
end architecture;
