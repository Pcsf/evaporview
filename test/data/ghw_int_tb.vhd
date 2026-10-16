entity ghw_int_tb is
end entity;

architecture sim of ghw_int_tb is
  signal n : integer := 0;
begin
  process
  begin
    for k in 1 to 6 loop
      wait for 10 ns;
      case k is
        when 1 => n <= 40;
        when 2 => n <= -70;
        when 3 => n <= 63;
        when 4 => n <= 64;
        when 5 => n <= -64;
        when others => n <= 8191;
      end case;
    end loop;
    wait;
  end process;
end architecture;
