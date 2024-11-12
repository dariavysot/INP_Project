-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2024 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Daria Vysotska <xvysot00>
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (1) / zapis (0)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_INV  : out std_logic;                      -- pozadavek na aktivaci inverzniho zobrazeni (1)
   OUT_WE   : out std_logic;                      -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'

   -- stavove signaly
   READY    : out std_logic;                      -- hodnota 1 znamena, ze byl procesor inicializovan a zacina vykonavat program
   DONE     : out std_logic                       -- hodnota 1 znamena, ze procesor ukoncil vykonavani programu (narazil na instrukci halt)
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is

    -- Program Counter (PC) signals
    signal PC_DOUT  :  std_logic_vector(12 downto 0); 
    signal PC_INC  :  std_logic; -- Increment PC
    signal PC_DEC :  std_logic; -- Decrement PC
 
    -- Pointer (PTR) signals
    signal PTR_DOUT  :  std_logic_vector(12 downto 0);
    signal PTR_INC :  std_logic; -- Increment PTR
    signal PTR_DEC :  std_logic; -- Decrement PTR
 
    -- Counter (CNT) signals
    signal CNT_DOUT  :  std_logic_vector(12 downto 0);
    signal CNT_INC :  std_logic; -- Increment CNT
    signal CNT_DEC  :  std_logic; -- Decrement CNT
 
    -- Multiplexer control signals
    signal MUX1_SELECT :  std_logic; -- Select input for MUX1
    signal MUX2_SELECT :  std_logic_vector(1 downto 0); -- Select input for MUX2
 
    -- Temporary register (TMP) signals
    signal TMP_OUT : std_logic_vector(7 downto 0);
    signal TMP_KEEP : std_logic; -- Control signal to save data into TMP

    -- FSM state types
    type FSM_STATE is (
        SReset,
        SSearch1,  SSearch2,
        SDone,
        Fetch,
        SDecode,
        MoveToR,  MoveToL, 
        SWrite_Char1, SWrite_Char2,
        SRead_Char1, SRead_Char2,  SRead_Char3,
        SIncrement1, SIncrement2,  SIncrement3,
        SDecrement1, SDecrement2,  SDecrement3,
        While_S_1, While_S_2, While_S_3, While_S_4,  While_S_5,
        While_End_1,  While_End_2,  While_End_3,  While_End_4,  While_End_5, While_End_6,
        SStoreTmp1,  SStoreTmp2,
        SLoadTmp,
        SHalt
    );

    signal currentState  : FSM_STATE;
    signal nextState  :  FSM_STATE;
    
  begin
   
  -- State transition logic based on clock and reset
   stateLogic: process (CLK, RESET, EN) is 
   begin
      if RESET = '1' then
        currentState <= SReset;
      elsif (CLK'event) and (CLK = '1') then 
         if EN = '1' then 
         currentState <= nextState;
         end if;
      end if;
   end process;

    -- Finite State Machine (FSM) definition
    FSM : process (currentState, OUT_BUSY, IN_VLD, DATA_RDATA, CNT_DOUT, PTR_DOUT, PC_DOUT) IS
    begin

        -- Default signal values
        PC_INC <= '0';
        PC_DEC  <= '0';
        PTR_INC <= '0';
        PTR_DEC <= '0';
        CNT_INC <= '0';
        CNT_DEC  <= '0';
        DATA_EN <= '0';
        IN_REQ <= '0';
        OUT_WE <= '0';
        DATA_RDWR <= '1';
        OUT_DATA <= DATA_RDATA;
        MUX2_SELECT<= "00";
        TMP_KEEP <= '0';

        case currentState is
           -- Reset state
            when SReset =>
                READY <= '0';
                DONE <= '0';
                nextState <= SSearch1;

          -- Search for '@' to set the pointer --
            when SSearch1 =>
              MUX1_SELECT  <= '1';
                DATA_EN <= '1';
                DATA_RDWR <= '1';
                nextState <= SSearch2;
            when SSearch2 =>
                if (DATA_RDATA = X"40") then
                  PTR_INC <= '1';
                  nextState <= SDone;
                else
                PTR_INC <= '1';
                nextState <= SSearch1;
                end if;

            -- Initialization done --
            when SDone =>
                READY <= '1';
                nextState <= Fetch;

            -- Fetch instruction
            when Fetch =>
              PC_INC <= '0';
              MUX1_SELECT  <= '0';
                DATA_RDWR <= '1';
                DATA_EN <= '1';
                nextState <= SDecode;
     
                when SDecode =>
                case DATA_RDATA is
                    when X"40" => nextState <= SHalt;  -- @ 0x40
                    when X"3E" => nextState <= MoveToR;      -- < 0x3E
                    when X"3C" => nextState <= MoveToL;      -- > 0x3C
                    when X"2B" => nextState <= SIncrement1;  -- + 0x2B
                    when X"2D" => nextState <= SDecrement1;  -- - 0x2D
                    when X"2E" => nextState <= SWrite_Char1; -- . 0x2E
                    when X"2C" => nextState <= SRead_Char1;  -- , 0x2C
                    when X"5B" => nextState <= While_S_1;    -- [ 0x5B
                    when X"5D" => nextState <= While_End_1;  -- ] 0x5D
                    when X"24" =>                            -- $ 0x24
                        DATA_EN <= '1';
                        MUX1_SELECT  <= '1';
                        nextState <= SStoreTmp1;             
                    when X"21" =>                            -- ! 0x21
                        DATA_EN <= '1';
                        DATA_RDWR <= '0';
                        MUX1_SELECT  <= '1';
                        MUX2_SELECT <= "01";
                        nextState <= SLoadTmp;
                    when others => 
                    nextState <= Fetch;
                end case;
             
            -- Pointer move instructions ('>' and '<') --
            when MoveToR =>
                PTR_INC <= '1';
                PC_INC <= '1';
                nextState <= Fetch;
            when MoveToL =>
              PTR_DEC <= '1';
                PC_INC <= '1';
                nextState <= Fetch;

            -- Increment memory ('+') --
            when SIncrement1 =>
                MUX1_SELECT  <= '1';
                DATA_EN <= '1';
                DATA_RDWR <= '1';
                nextState <= SIncrement2;
            when SIncrement2 =>
              MUX2_SELECT<= "11";
                nextState <= SIncrement3;
            when SIncrement3 =>
                DATA_EN <= '1';
                DATA_RDWR <= '0';
                PC_INC <= '1';
                nextState <= Fetch;
  
            -- Decrement memory ('-') --
            when SDecrement1 =>
              MUX1_SELECT <= '1';
                DATA_EN <= '1';
                DATA_RDWR <= '1';
                nextState <= SDecrement2;
            when SDecrement2 =>
              MUX2_SELECT <= "10";
                nextState <= SDecrement3;
            when SDecrement3 =>
                DATA_EN <= '1';
                DATA_RDWR <= '0';
                PC_INC <= '1';
                nextState <= Fetch;

            -- Write character to output ('.') --
            when SWrite_Char1 =>
                MUX1_SELECT  <= '1';
                DATA_EN <= '1';
                DATA_RDWR <= '1';
                nextState <= SWrite_Char2;
            when SWrite_Char2 =>
                if (OUT_BUSY = '1') then
                  nextState <= SWrite_Char1;
                else
                    OUT_WE <= '1';
                    OUT_DATA <= DATA_RDATA;
                    PC_INC <= '1';
                    nextState <= Fetch;
                end if;  

          -- Read character from input (',') --
            when SRead_Char1 =>
                MUX1_SELECT  <= '1';
                IN_REQ <= '1';
                DATA_EN <= '1';
                DATA_RDWR <= '1';
              nextState <= SRead_Char2;
            when SRead_Char2 =>  
                if (IN_VLD = '0') then
                  nextState <= SRead_Char1;
                    IN_REQ <= '1';
                else
                    DATA_EN <= '1';
                    DATA_RDWR <= '0';
                    IN_REQ <= '1';
                    MUX2_SELECT <= "00";
                    PC_INC <= '1';
                  nextState <= SRead_Char3;
                end if;
            when SRead_Char3 =>
                DATA_RDWR <= '1';
              nextState <= Fetch;
            
            -- Start of while loop ('[') --
            when While_S_1 =>
                DATA_EN <= '1';
                PC_INC <= '1';
                MUX1_SELECT  <= '1';
              nextState <= While_S_2;      
            when While_S_2 =>
                if (DATA_RDATA = 0)  then
                  CNT_INC <= '1';
                  nextState <= While_S_3;
                else
                nextState <= Fetch;
                end if;
            when While_S_3 =>
                if (CNT_DOUT = 0) then
                  nextState <=Fetch;
                else 
                nextState <=While_S_4;
                end if;
            when While_S_4 =>
                    DATA_EN <= '1';
                    MUX1_SELECT  <= '0';
                  nextState <= While_S_5;
            when While_S_5 =>
                if (DATA_RDATA = X"5D") then
                    CNT_DEC  <= '1';
                    PC_INC <= '1';
                  nextState <= While_S_3;
                elsif (DATA_RDATA = X"5B") then
                  CNT_INC <= '1';
                    PC_INC <= '1';
                    nextState <= While_S_3;
                end if ;

            -- End of while loop (']') --
            when  While_End_1=> 
                DATA_EN <= '1';
                MUX1_SELECT  <= '1';
                nextState <=  While_End_2;
            when  While_End_2 =>
                if (DATA_RDATA = 0) then
                  PC_INC <= '1';
                  nextState <= Fetch;
                else
                  CNT_INC <= '1';
                    PC_DEC  <= '1';
                    nextState <= While_End_3;
                end if;
            when While_End_3 =>
                if (CNT_DOUT = 0)then
                  nextState <= Fetch;
                else
                  nextState <= While_End_4;
                end if;
            when While_End_4 =>
                DATA_EN <= '1';
                MUX1_SELECT  <= '0';
                nextState <= While_End_5; 
            when While_End_5 =>
                if (DATA_RDATA = X"5B") then
                  CNT_DEC  <= '1';
                elsif (DATA_RDATA = X"5D") then
                  CNT_INC <= '1';
                end if;           
                nextState <= While_End_6;
            when While_End_6 =>
                if (CNT_DOUT = 0) then
                  PC_INC <= '1';
                else
                PC_DEC  <= '1';
                end if;
                nextState <= While_End_3;

              -- Store value into TMP ('$') --
              when SStoreTmp1 =>
                DATA_EN <= '1';
                DATA_RDWR <= '1';
                TMP_KEEP <= '1';
                MUX1_SELECT <= '1';
                nextState <= SStoreTmp2;

              when SStoreTmp2 => 
                DATA_RDWR <= '1';
                PC_INC <= '1';
                TMP_KEEP <= '0';
                nextState <= Fetch; 

              -- Load value from TMP ('!')
              when SLoadTmp =>
                DATA_EN <= '1'; 
                DATA_RDWR <= '0';
                PC_INC <= '1';
                nextState <= Fetch;
                
              -- Halt (end program)
              when SHalt =>
                DONE <= '1';
                nextState <= SHalt;
                
          when others => null;
        end case;
    end process;

   -- Counter (CNT) process
   CNT: process (RESET, CLK, CNT_DOUT, CNT_INC, CNT_DEC )
   begin
      if RESET = '1' then
        CNT_DOUT <= (others => '0');
      elsif (CLK'event) and (CLK = '1') then
         if CNT_DEC  = '1' then
          CNT_DOUT <= CNT_DOUT - 1;
         elsif CNT_INC = '1' then
          CNT_DOUT <= CNT_DOUT + 1;
         end if;
      end if;
   end process;

    -- Program Counter (PC) process
    PC: process (RESET, CLK, PC_DOUT, PC_INC, PC_DEC )
      begin
         if RESET = '1' then
          PC_DOUT <= (others => '0');
         elsif (CLK'event) and (CLK = '1') then
           if (PC_DEC  = '1') then
            PC_DOUT <= PC_DOUT - 1;
            elsif (PC_INC = '1') then
              PC_DOUT <= PC_DOUT + 1;  
           end if;
         end if;
      end process;

   -- Pointer (PTR) process
    PTR: process (RESET, CLK, PTR_DOUT, PTR_INC, PTR_DEC)
    begin
      if RESET = '1' then
        PTR_DOUT <= (others => '0');
      elsif (CLK'event) and (CLK = '1') then
         if (PTR_INC = '1') then
            if (PTR_DOUT = "1111111111111") then
              PTR_DOUT <= (others => '0');
            else
              PTR_DOUT <= PTR_DOUT + 1;
            end if;
         elsif (PTR_DEC = '1') then
            if (PTR_DOUT = "0000000000000") then
              PTR_DOUT <= (others => '1');
            else
              PTR_DOUT <= PTR_DOUT - 1;
            end if;
         end if;
      end if;
   end process;

   -- Temporary Register (TMP) process
   TMP: process (CLK, RESET, TMP_KEEP) 
   begin 
          if (RESET = '1') then
            TMP_OUT <= (others => '0');
          elsif (rising_edge(CLK)) then
            if (EN = '1' and TMP_KEEP = '1') then
              TMP_OUT <= DATA_RDATA;
            end if;
          end if;
    end process; 
 
   -- Multiplexer 1 (MUX1) process
   MUX1: process(PC_DOUT, PTR_DOUT, MUX1_SELECT ) 
   begin   
        if (MUX1_SELECT  = '0') then
            DATA_ADDR <= PC_DOUT;
         elsif (MUX1_SELECT  = '1') then
            DATA_ADDR <= PTR_DOUT;
         else 
            DATA_ADDR <= (others => '0');
         end if;
   end process;

  -- Multiplexer 2 (MUX2) process
   MUX2: process (RESET, CLK, MUX2_SELECT)
   begin
      if (CLK'event) and (CLK = '1') then
         if (MUX2_SELECT = "00") then
            DATA_WDATA <= IN_DATA;
        elsif (MUX2_SELECT = "01") then
          DATA_WDATA <= TMP_OUT;            
         elsif (MUX2_SELECT = "11") then
            DATA_WDATA <= DATA_RDATA + 1;
         elsif (MUX2_SELECT = "10") then
            DATA_WDATA <= DATA_RDATA - 1;
        else
         end if;
      end if;
   end process; 
   -- pri tvorbe kodu reflektujte rady ze cviceni INP, zejmena mejte na pameti, ze 
   --   - nelze z vice procesu ovladat stejny signal,
   --   - je vhodne mit jeden proces pro popis jedne hardwarove komponenty, protoze pak
   --      - u synchronnich komponent obsahuje sensitivity list pouze CLK a RESET a 
   --      - u kombinacnich komponent obsahuje sensitivity list vsechny ctene signaly.   
end behavioral;
