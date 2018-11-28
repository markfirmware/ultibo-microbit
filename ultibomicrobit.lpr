program UltiboMicroBit;

{$mode objfpc}{$H+}{$modeswitch advancedrecords}

uses
 {$ifdef BUILD_MODE_QEMU}  RaspberryPi,  {$endif}
 {$ifdef BUILD_MODE_RPI }  RaspberryPi,  {$endif}
 {$ifdef BUILD_MODE_RPI2}  RaspberryPi2, {$endif}
 {$ifdef BUILD_MODE_RPI3}  RaspberryPi3, {$endif}
 GlobalConfig,
 GlobalConst,
 GlobalTypes,
 Platform,
 Threads,
 SysUtils,
 Classes,
 Ultibo,
 Console,Logging,
 DWCOTG,Keyboard,
 FileSystem,MMC,FATFS,
 Shell,ShellFilesystem,RemoteShell,ConsoleShell,
 Serial,USBCDCACM;

type
 PMicroBit = ^TMicroBit;
 TMicroBit = record
  DeviceName:String;
  SerialDevice:PSerialDevice;
  ThreadHandle:TThreadHandle;
  constructor Create(Index:Integer);
  procedure Start;
  procedure Thread;
 end;

var
 MicroBits:Array[1 .. 4] of TMicroBit;
 Console1:TWindowHandle;
 RxChar:Char;
 RxCount:LongWord;
 RxBuffer:String;
 I:Integer;

procedure TMicroBit.Thread;
var
 StatusCode:LongWord;
begin
 ConsoleWindowWriteLn(Console1,Format('thread started %s',[DeviceName]));
 while True do
  begin
   SerialDevice:=Nil;
   while True do
    begin
     SerialDevice:=SerialDeviceFindByName(DeviceName);
     if SerialDevice <> Nil then
      begin
       //The last 2 parameters allow setting the size of the transmit and receive buffers,
       //passing 0 means use the default size.}
       StatusCode:=SerialDeviceOpen(SerialDevice,115200,SERIAL_DATA_8BIT,SERIAL_STOP_1BIT,SERIAL_PARITY_NONE,SERIAL_FLOW_NONE,0,0);
       if StatusCode = ERROR_SUCCESS then
        break
       else
        ConsoleWindowWriteLn(Console1,Format('SerialDeviceOpen %s %d',[DeviceName,StatusCode]));
      end;
     Sleep(500);
    end;
   ConsoleWindowWriteLn(Console1,Format('%s open done',[DeviceName]));
   RxBuffer:='';
   while True do
    begin
     StatusCode:=SerialDeviceRead(SerialDevice,@RxChar,SizeOf(RxChar),SERIAL_READ_NONE,RxCount);
     if StatusCode <> ERROR_SUCCESS then
      begin
       SerialDeviceClose(SerialDevice);
       ConsoleWindowWriteLn(Console1,Format('%s rx error %d',[DeviceName,StatusCode]));
       Sleep(1*1000);
       break;
      end;
     if RxCount <> 0 then
      begin
       if RxChar <> #10 then
        begin
         RxBuffer:=RxBuffer + RxChar;
        end
       else
        begin
         ConsoleWindowWriteLn(Console1,Format('%s %s',[DeviceName,RxBuffer]));
         RxBuffer:='';
        end;
      end;
    end;
  end;
end;

function MicroBitSerialThread(Parameter:Pointer):PtrInt;
var
 MicroBit:PMicroBit;
begin
 Result:=0;
 MicroBit:=Parameter;
 MicroBit^.Thread;
end;

constructor TMicroBit.Create(Index:Integer);
begin
 DeviceName:='Serial' + IntToStr(Index);
end;

procedure TMicroBit.Start;
begin
 BeginThread(@MicroBitSerialThread,@Self,Self.ThreadHandle,THREAD_STACK_DEFAULT_SIZE);
end;

procedure StartLogging;
begin
 LOGGING_INCLUDE_COUNTER:=False;
 LOGGING_INCLUDE_TICKCOUNT:=True;
 CONSOLE_REGISTER_LOGGING:=True;
 CONSOLE_LOGGING_POSITION:=CONSOLE_POSITION_BOTTOMRIGHT;
 LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));
end;

procedure Log(S:String);
begin
 LoggingOutput(S);
end;

procedure RestoreBootFile(Prefix,FileName:String);
var
 Source:String;
begin
 if BoardGetType <> BOARD_TYPE_QEMUVPB then
  begin
   Source:=Prefix + '-' + FileName;
   Log(Format('Restoring from %s ...',[Source]));
   while not DirectoryExists('C:\') do
    Sleep(500);
   if FileExists(Source) then
    CopyFile(PChar(Source),PChar(FileName),False);
   Log(Format('Restoring from %s done',[Source]));
  end;
end;

begin
 StartLogging;
 RestoreBootFile('default','config.txt');
 Console1:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,True);
 for I:=Low(MicroBits) to High(MicroBits) do
  begin
   MicroBits[I]:=TMicroBit.Create(I);
   MicroBits[I].Start;
  end;
 ThreadHalt(0);
end.

program MicroBitDemo;
{$mode objfpc}{$modeswitch advancedrecords}{$H+}

uses 
{$ifdef BUILD_QEMUVPB } QEMUVersatilePB,                  {$endif}
{$ifdef BUILD_RPI     } BCM2708,BCM2835,                  {$endif}
{$ifdef BUILD_RPI2    } BCM2709,BCM2836,                  {$endif}
{$ifdef BUILD_RPI3    } BCM2710,BCM2837,LAN78XX,SMSC95XX, {$endif}
GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,SysUtils,Classes,Console,Logging,Ultibo,
Serial,DWCOTG,FileSystem,MMC,FATFS,Keyboard,bcmfw,EncodedSpa,WinSock2,Http,WebStatus,Math,USBCDCACM,Pwm;

const 
 PwmRange                    = 1000;
 BoostDuration               = 20;
 ScanUnitsPerSecond          = 1600;
 ScanInterval                = 0.800;
 ScanWindow                  = 0.400;

 HCI_COMMAND_PKT             = $01;
 HCI_EVENT_PKT               = $04;
 OGF_HOST_CONTROL            = $03;
 OGF_LE_CONTROL              = $08;
 OGF_VENDOR                  = $3f;
 LL_SCAN_PASSIVE             = $00;
 LL_SCAN_ACTIVE              = $01;

 ADV_IND                     = $00; // Connectable undirected advertising(default)
 ADV_DIRECT_IND_HI           = $01; // Connectable high duty cycle directed advertising
 ADV_SCAN_IND                = $02; // Scannable undirected advertising
 ADV_NONCONN_IND             = $03; // Non connectable undirected advertising
 ADV_DIRECT_IND_LO           = $04; // Connectable low duty cycle directed advertising

 // Advertising Data Types
 ADT_FLAGS                   = $01; // Flags
 ADT_INCOMPLETE_UUID16       = $02; // Incomplete List of 16-bit Service Class UUIDs
 ADT_COMPLETE_UUID16         = $03; // Complete List of 16-bit Service Class UUIDs
 ADT_INCOMPLETE_UUID32       = $04; // Incomplete List of 32-bit Service Class UUIDs
 ADT_COMPLETE_UUID32         = $05; // Complete List of 32-bit Service Class UUIDs
 ADT_INCOMPLETE_UUID128      = $06; // Incomplete List of 128-bit Service Class UUIDs
 ADT_COMPLETE_UUDI128        = $07; // Complete List of 128-bit Service Class UUIDs
 ADT_SHORTENED_LOCAL_NAME    = $08; // Shortened Local name
 ADT_COMPLETE_LOCAL_NAME     = $09; // Complete Local name
 ADT_POWER_LEVEL             = $0A; // Tx Power Level
 ADT_DEVICE_CLASS            = $0D; // Class of Device
 ADT_SERVICE_DATA            = $16; // Service data, starts with service uuid followed by data
 ADT_DEVICE_APPEARANCE       = $19; // Device appearance
 ADT_MANUFACTURER_SPECIFIC   = $FF;

 ManufacturerApple           = $004c;
 ManufacturerEstimote        = $015d;
 ManufacturerFlic            = $030f;
 ManufacturerLogitech        = $01da;
 ManufacturerMicrosoft       = $0006;
 ManufacturerTesting         = $ffff;

 BDADDR_LEN                  = 6;

type 
 TSpaDocument = class(THTTPDocument)
  protected 
   function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 end;

 TJsonDocument = class(THTTPDocument)
  protected 
   function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 end;

type 
 TArrayOfByte = Array of Byte;
 PLocomotive = ^TLocomotive;
 PMicroBitPeripheral = ^TMicroBitPeripheral;
 TMicroBitPeripheral = record
  AddressString:String;
  ButtonCounter:LongWord;
  TimeAtLastReception:LongWord;
  CarrierLost:Boolean;
  AssignedLocomotive:PLocomotive;
  AsciiMessage:String;
  NewestAsciiMessage:String;
 end;
 TLocomotive = record
  Id:Integer;
  ButtonThrottleCounter:Integer;
  CommandedThrottle:Integer;
  Peripheral:PMicroBitPeripheral;
  PwmLoopHandle:TThreadHandle;
 end;

var 
 HTTPListener:THTTPListener;
 Console1:TWindowHandle;
 LedRequest:Integer;
 MicroBitPeripherals:Array of TMicroBitPeripheral;
 Locomotive:TLocomotive;
 ScanRxCount:Integer;
 BluetoothUartDeviceDescription:String;
 ScanCycleCounter:LongWord;
 ScanIdle:Boolean;
 ScanStartTime:LongWord;
 Margin:LongWord;
 ReadBackLog:Integer;
 LastDeviceStatus:LongWord;
 HciSequenceNumber:Integer = 0;
 ch:char;
 UART0:PSerialDevice = Nil;
 LedLoopHandle:TThreadHandle = INVALID_HANDLE_VALUE;
 ButtonThrottleLoopHandle:TThreadHandle = INVALID_HANDLE_VALUE;
 DemoLoopHandle:TThreadHandle = INVALID_HANDLE_VALUE;
 KeyboardLoopHandle:TThreadHandle = INVALID_HANDLE_VALUE;
 SelfTestLoopHandle:TThreadHandle = INVALID_HANDLE_VALUE;
 ReadByteCounter:Integer;

function ReadByte:Byte; forward;

procedure RestoreBootFile(Prefix,FileName:String);
var 
 Source:String;
begin
 if BoardGetType <> BOARD_TYPE_QEMUVPB then
  begin
   Source:=Prefix + '-' + FileName;
   Log(Format('Restoring from %s ...',[Source]));
   while not DirectoryExists('C:\') do
    Sleep(500);
   if FileExists(Source) then
    CopyFile(PChar(Source),PChar(FileName),False);
   Log(Format('Restoring from %s done',[Source]));
  end;
end;

function ogf(op:Word):byte;
begin
 Result:=(op shr 10) and $3f;
end;

function ocf(op:Word):Word;
begin
 Result:=op and $3ff;
end;

function ErrToStr(code:byte):string;
begin
 case code of 
  $00:Result:='Success';
  $01:Result:='Unknown HCI Command';
  $02:Result:='Unknown Connection Identifier';
  $03:Result:='Hardware Failure';
  $04:Result:='Page Timeout';
  $05:Result:='Authentication Failure';
  $06:Result:='PIN or Key Missing';
  $07:Result:='Memory Capacity Exceeded';
  $08:Result:='Connection Timeout';
  $09:Result:='Connection Limit Exceeded';
  $0A:Result:='Synchronous Connection Limit To A Device Exceeded';
  $0B:Result:='ACL Connection Already Exists';
  $0C:Result:='Command Disallowed';
  $0D:Result:='Connection Rejected due to Limited Resources';
  $0E:Result:='Connection Rejected due To Security Reasons';
  $0F:Result:='Connection Rejected due to Unacceptable BD_ADDR';
  $10:Result:='Connection Accept Timeout Exceeded';
  $11:Result:='Unsupported Feature or Parameter Value';
  $12:Result:='Invalid HCI Command Parameters';
  $13:Result:='Remote User Terminated Connection';
  $14:Result:='Remote Device Terminated Connection due to Low Resources';
  $15:Result:='Remote Device Terminated Connection due to Power Off';
  $16:Result:='Connection Terminated By Local Host';
  $17:Result:='Repeated Attempts';
  $18:Result:='Pairing Not Allowed';
  $19:Result:='Unknown LMP PDU';
  $1A:Result:='Unsupported Remote Feature / Unsupported LMP Feature';
  $1B:Result:='SCO Offset Rejected';
  $1C:Result:='SCO Interval Rejected';
  $1D:Result:='SCO Air Mode Rejected';
  $1E:Result:='Invalid LMP Parameters / Invalid LL Parameters';
  $1F:Result:='Unspecified Error';
  $20:Result:='Unsupported LMP Parameter Value / Unsupported LL Parameter Value';
  $21:Result:='Role Change Not Allowed';
  $22:Result:='LMP Response Timeout / LL Response Timeout';
  $23:Result:='LMP Error Transaction Collision';
  $24:Result:='LMP PDU Not Allowed';
  $25:Result:='Encryption Mode Not Acceptable';
  $26:Result:='Link Key cannot be Changed';
  $27:Result:='Requested QoS Not Supported';
  $28:Result:='Instant Passed';
  $29:Result:='Pairing With Unit Key Not Supported';
  $2A:Result:='Different Transaction Collision';
  $2B:Result:='Reserved';
  $2C:Result:='QoS Unacceptable Parameter';
  $2D:Result:='QoS Rejected';
  $2E:Result:='Channel Classification Not Supported';
  $2F:Result:='Insufficient Security';
  $30:Result:='Parameter Out Of Mandatory Range';
  $31:Result:='Reserved';
  $32:Result:='Role Switch Pending';
  $33:Result:='Reserved';
  $34:Result:='Reserved Slot Violation';
  $35:Result:='Role Switch Failed';
  $36:Result:='Extended Inquiry Response Too Large';
  $37:Result:='Secure Simple Pairing Not Supported By Host';
  $38:Result:='Host Busy - Pairing';
  $39:Result:='Connection Rejected due to No Suitable Channel Found';
  $3A:Result:='Controller Busy';
  $3B:Result:='Unacceptable Connection Parameters';
  $3C:Result:='Directed Advertising Timeout';
  $3D:Result:='Connection Terminated due to MIC Failure';
  $3E:Result:='Connection Failed to be Established';
  $3F:Result:='MAC Connection Failed';
  $40:Result:='Coarse Clock Adjustment Rejected but Will Try to Adjust Using Clock';
 end;
end;

procedure Fail(Message:String);
begin
 raise Exception.Create(Message);
end;

procedure HciCommand(OpCode:Word; Params:array of byte);
var 
 i:integer;
 Cmd:array of byte;
 res,count:LongWord;
 PacketType,EventCode,PacketLength,CanAcceptPackets,Status:Byte;
 Acknowledged:Boolean;
begin
 Inc(HciSequenceNumber);
 // if OpCode <> $fc4c then
 //  Log(Format('hci %d op %04.4x',[HciSequenceNumber,OpCode]));
 SetLength(Cmd,length(Params) + 4);
 Cmd[0]:=HCI_COMMAND_PKT;
 Cmd[1]:=lo(OpCode);
 Cmd[2]:=hi(OpCode);
 Cmd[3]:=length(Params);
 for i:=0 to length(Params) - 1 do
  Cmd[4 + i]:=Params[i];
 count:=0;
 res:=SerialDeviceWrite(UART0,@Cmd[0],length(Cmd),SERIAL_WRITE_NONE,count);
 if res = ERROR_SUCCESS then
  begin
   Acknowledged:=False;
   while not Acknowledged do
    begin
     PacketType:=ReadByte;
     if PacketType <> HCI_EVENT_PKT then
      Fail(Format('event type not hci event: %d',[PacketType]));
     EventCode:=ReadByte;
     if EventCode = $0E then
      begin
       PacketLength:=ReadByte;
       if PacketLength <> 4 then
        Fail(Format('packet length not 4: %d',[PacketLength]));
       CanAcceptPackets:=ReadByte;
       if CanAcceptPackets <> 1 then
        Fail(Format('can accept packets not 1: %d',[CanAcceptPackets]));
       ReadByte; // completed command low
       ReadByte; // completed command high
       Status:=ReadByte;
       Acknowledged:=True;
      end
     else if EventCode = $0F then
           begin
            PacketLength:=ReadByte;
            if PacketLength <> 4 then
             Fail(Format('packet length not 4: %d',[PacketLength]));
            Status:=ReadByte;
            CanAcceptPackets:=ReadByte;
            if CanAcceptPackets <> 1 then
             Fail(Format('can accept packets not 1: %d',[CanAcceptPackets]));
            ReadByte; // completed command low
            ReadByte; // completed command high
            Acknowledged:=True;
           end
     else
      begin
       PacketLength:=ReadByte;
       Log(Format('HciCommand discarding event %d length %d',[EventCode,PacketLength]));
       for I:=1 to PacketLength do
        ReadByte;
       Sleep(5*1000);
       // Fail(Format('event code not command completed nor status: %02.2x',[EventCode]));
      end;
    end;
   if Status <> 0 then
    Fail(Format('status not 0: %d',[Status]));
  end
 else
  Log('Error writing to BT.');
end;

procedure HciCommand(OGF:byte; OCF:Word; Params:array of byte);
begin
 HciCommand((OGF shl 10) or OCF,Params);
end;

procedure MicroBitPeripheralReset(MicroBitPeripheral:PMicroBitPeripheral;Message:String);
begin
 Log(Message);
 MicroBitPeripheral^.ButtonCounter:=0;
 if MicroBitPeripheral^.AssignedLocomotive <> nil then
  begin
   MicroBitPeripheral^.AssignedLocomotive^.Peripheral := Nil;
   MicroBitPeripheral^.AssignedLocomotive^.ButtonThrottleCounter := 0;
   MicroBitPeripheral^.AssignedLocomotive^.CommandedThrottle := 0;
   MicroBitPeripheral^.AssignedLocomotive:=Nil;
   MicroBitPeripheral^.AsciiMessage:='';
   MicroBitPeripheral^.NewestAsciiMessage:='';
  end;
end;

procedure CullMicroBitPeripherals;
var 
 I,J:Integer;
 Now:LongWord;
begin
 Now:=GetTickCount;
 for I:=High(MicroBitPeripherals) downto 0 do
  with MicroBitPeripherals[I] do
   begin
    if not CarrierLost and (LongWord(Now - TimeAtLastReception) >= 2*1000) then
     begin
      CarrierLost:=True;
      Log(Format('**** micro:bit ble %s carrier lost - warning',[AddressString]));
     end
    else if CarrierLost and (LongWord(Now - TimeAtLastReception) >= 2*1000) then
          begin
           MicroBitPeripheralReset(@MicroBitPeripherals[I],Format('**** micro:bit ble %s carrier lost - removed from active list',[AddressString]));
           if Locomotive.Peripheral = @MicroBitPeripherals[I] then
            Locomotive.Peripheral:=Nil;
           for J:=I + 1 to High(MicroBitPeripherals) do
            MicroBitPeripherals[J - 1]:=MicroBitPeripherals[J];
           SetLength(MicroBitPeripherals,Length(MicroBitPeripherals) - 1);
          end;
   end;
end;

procedure Console1WriteLn(Line:string);
begin
 ConsoleWindowWrite(Console1,Line);
 ConsoleWindowClearEx(Console1,ConsoleWindowGetX(Console1),ConsoleWindowGetY(Console1),ConsoleWindowGetMaxX(Console1),ConsoleWindowGetY(Console1),False);
 ConsoleWindowWriteLn(Console1,'');
end;

procedure Console1SetForecolor(Color:LongWord);
begin
 ConsoleWindowSetForecolor(Console1,Color);
end;

procedure EndOfScan;
var 
 I:Integer;
 Line:string;
 MotionString,OperatorString:string;
begin
 Console1SetForecolor(COLOR_ORANGE);
 CullMicroBitPeripherals;
 ConsoleWindowSetXY(Console1,1,1);
 Console1WriteLn(Format('%s',[BoardTypeToString(BoardGetType)]));
 with Locomotive do
  begin
   if CommandedThrottle = 0 then
    Console1SetForecolor(COLOR_WHITE)
   else if CommandedThrottle > 0 then
         Console1SetForecolor(COLOR_GREEN)
   else
    Console1SetForecolor(COLOR_RED);
   MotionString:=Format('%4d%% throttle',[CommandedThrottle]);
   if Peripheral = nil then
    begin
     Console1SetForecolor(COLOR_GRAY);
     OperatorString:=Format('Available (select %s)',['chord aaaa']);
    end
   else
    OperatorString:=Format('micro:bit operator %s',[Peripheral^.AddressString]);
   Console1WriteLn(Format('%02.2d %-19s %s',[Id,MotionString,OperatorString]))
  end;
 Console1SetForecolor(COLOR_YELLOW);
 Console1WriteLn(Format('',[]));
 Console1WriteLn(Format('micro:bit operators detected',[]));
 for I:=0 to High(MicroBitPeripherals) do
  begin
   // LineFormat:=Format('%%7s %%%dd %%s %%s',[CounterWidth]);
   // Line:=Format(LineFormat,[dBm(Last.Rssi),Count,Key,Last.Data]);
   with MicroBitPeripherals[I] do
    begin
     if AssignedLocomotive <> nil then
      Line:=Format('%s operating locomotive %02.2d',[AddressString,AssignedLocomotive^.Id])
     else
      //      Line:=Format('%s cannot engage - must restart micro:bit to engage with this device',[AddressString]);
      Line:=Format('%s ...',[AddressString]);
     Console1WriteLn(Line);
    end;
  end;
 Console1SetForecolor(COLOR_WHITE);
 Console1WriteLn(Format('',[]));
 Console1WriteLn(Format('screen refreshed after every ble scan interval (%5.3f seconds) - (scan window is %5.3f seconds)',[ScanInterval,ScanWindow]));
 ConsoleWindowClearEx(Console1,ConsoleWindowGetX(Console1),ConsoleWindowGetY(Console1),ConsoleWindowGetMaxX(Console1),ConsoleWindowGetMaxY(Console1),False);
end;

function EventReadFirstByte:Byte;
var 
 c:LongWord;
 b:Byte;
 res:Integer;
 Now:LongWord;
 EntryTime:LongWord;
begin
 Result:=0;
 EntryTime:=ClockGetCount;
 while LongWord(ClockGetCount - EntryTime) < 10*1000*1000 do
  begin
   Now:=ClockGetCount;
   c:=0;
   res:=SerialDeviceRead(UART0,@b,1,SERIAL_READ_NON_BLOCK,c);
   if (res = ERROR_SUCCESS) and (c = 1) then
    begin
     Result:=b;
     Inc(ReadByteCounter);
     if ScanIdle then
      begin
       ScanIdle:=False;
       ScanStartTime:=Now;
       ScanRxCount:=0;
       if (ScanCycleCounter >= 1) and (LongWord(Now - EntryTime) div 1000 < Margin) then
        begin
         Margin:=LongWord(Now - EntryTime) div 1000;
         LoggingOutput(Format('lowest available processing time between scans is now %5.3fs',[Margin / 1000]));
        end;
      end;
     Inc(ScanRxCount);
     exit;
    end
   else
    begin
     if (not ScanIdle) and (LongWord(Now - ScanStartTime)/(1*1000*1000)  > ScanWindow + 0.200)  then
      begin
       ScanIdle:=True;
       Inc(ScanCycleCounter);
       EndOfScan;
      end;
     ThreadYield;
    end;
  end;
 Fail('timeout waiting for serial read byte');
end;

function ReadByte:Byte;
var 
 c:LongWord;
 b:Byte;
 res:Integer;
 EntryTime:LongWord;
 SerialStatus:LongWord;
begin
 Result:=0;
 EntryTime:=ClockGetCount;
 while LongWord(ClockGetCount - EntryTime) < 1*1000*1000 do
  begin
   c:=0;
   res:=SerialDeviceRead(UART0,@b,1,SERIAL_READ_NON_BLOCK,c);
   if (res = ERROR_SUCCESS) and (c = 1) then
    begin
     Result:=b;
     Inc(ReadByteCounter);
     res:=SerialDeviceRead(UART0,@b,1,SERIAL_READ_PEEK_BUFFER,c);
     if c > ReadBackLog then
      begin
       ReadBackLog:=c;
       LoggingOutput(Format('highest SERIAL_READ_PEEK_BUFFER is now %d',[ReadBackLog]));
      end;
     SerialStatus:=SerialDeviceStatus(UART0);
     SerialStatus:=SerialStatus and not (SERIAL_STATUS_RX_EMPTY or SERIAL_STATUS_TX_EMPTY);
     if SerialStatus <> LastDeviceStatus then
      begin
       LastDeviceStatus:=SerialStatus;
       LoggingOutput(Format('SerialDeviceStatus changed %08.8x',[SerialStatus]));
      end;
     exit;
    end
   else
    ThreadYield;
  end;
 Fail('timeout waiting for serial read byte');
end;

function IsBlueToothAvailable:Boolean;
begin
 Result:=True;
 Log(Format('Board is %s',[BoardTypeToString(BoardGetType)]));
 case BoardGetType of 
  BOARD_TYPE_RPI3B:
                   begin
                    BluetoothUartDeviceDescription:='BCM2837 PL011 UART';
                    PrepareBcmFirmware(0);
                   end;
  BOARD_TYPE_RPI3B_PLUS:
                        begin
                         BluetoothUartDeviceDescription:='BCM2837 PL011 UART';
                         PrepareBcmFirmware(1);
                        end;
  BOARD_TYPE_RPI_ZERO_W:
                        begin
                         BluetoothUartDeviceDescription:='BCM2835 PL011 UART';
                         PrepareBcmFirmware(0);
                        end;
  else
   begin
    Log('');
    Log('');
    Log('Bluetooth is not available on this board');
    Result:=False;
   end;
 end;
end;

function OpenUART0:boolean;
var 
 res:LongWord;
begin
 Result:=False;
 UART0:=SerialDeviceFindByDescription(BluetoothUartDeviceDescription);
 if UART0 = nil then
  begin
   Log('Cannot find UART0');
   exit;
  end;
 if BoardGetType = BOARD_TYPE_RPI_ZERO_W then
  res:=SerialDeviceOpen(UART0,115200,SERIAL_DATA_8BIT,SERIAL_STOP_1BIT,SERIAL_PARITY_NONE,SERIAL_FLOW_RTS_CTS,0,0)
 else
  res:=SerialDeviceOpen(UART0,115200,SERIAL_DATA_8BIT,SERIAL_STOP_1BIT,SERIAL_PARITY_NONE,SERIAL_FLOW_NONE,0,0);
 if res = ERROR_SUCCESS then
  begin
   Result:=True;
   ReadBackLog:=0;
   LastDeviceStatus:=0;

   GPIOFunctionSelect(GPIO_PIN_14,GPIO_FUNCTION_IN);
   GPIOFunctionSelect(GPIO_PIN_15,GPIO_FUNCTION_IN);

   GPIOFunctionSelect(GPIO_PIN_32,GPIO_FUNCTION_ALT3);     // TXD0
   GPIOFunctionSelect(GPIO_PIN_33,GPIO_FUNCTION_ALT3);     // RXD0
   GPIOPullSelect(GPIO_PIN_32,GPIO_PULL_NONE);             //Added
   GPIOPullSelect(GPIO_PIN_33,GPIO_PULL_UP);               //Added

   if BoardGetType = BOARD_TYPE_RPI_ZERO_W then
    begin
     GPIOFunctionSelect(GPIO_PIN_30,GPIO_FUNCTION_ALT3);     // RTS
     GPIOFunctionSelect(GPIO_PIN_31,GPIO_FUNCTION_ALT3);     // CTS
     GPIOPullSelect(GPIO_PIN_30,GPIO_PULL_UP);
     GPIOPullSelect(GPIO_PIN_31,GPIO_PULL_NONE);
    end;

   Sleep(50);
  end;
end;

procedure ResetChip;
begin
 HciCommand(OGF_HOST_CONTROL,$03,[]);
end;

procedure CloseUART0;
begin
 SerialDeviceClose(UART0);
 UART0:=Nil;
end;

procedure BCMLoadFirmware;
var 
 Params:array of byte;
 len:integer;
 Op:Word;
 Index:Integer;
 I:Integer;
 P:Pointer;
function GetByte:Byte;
begin
 Result:=PByte(P)^;
 Inc(P);
 Inc(Index);
end;
begin
 Log('Firmware load ...');
 HciCommand(OGF_VENDOR,$2e,[]);
 Index:=0;
 P:=BcmFirmwarePointer;
 while Index < BcmFirmwareLength do
  begin
   Op:=GetByte;
   Op:=Op or (GetByte shl 8);
   Len:=GetByte;
   SetLength(Params,Len);
   for I:= 0 to Len - 1 do
    Params[I]:=GetByte;
   HciCommand(Op,Params);
  end;
 CloseUart0;
 Sleep(50);
 OpenUart0;
 Sleep(50);
 Log('Firmware load done');
end;

{$ifdef BUILD_QEMUVPB}

procedure StartLogging;
begin
 LOGGING_INCLUDE_COUNTER:=False;
 LOGGING_INCLUDE_TICKCOUNT:=True;
 SERIAL_REGISTER_LOGGING:=True;
 SerialLoggingDeviceAdd(SerialDeviceGetDefault);
 SERIAL_REGISTER_LOGGING:=False;
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_SERIAL));
end;

{$else}

procedure StartLogging;
begin
 LOGGING_INCLUDE_COUNTER:=False;
 LOGGING_INCLUDE_TICKCOUNT:=True;
 CONSOLE_REGISTER_LOGGING:=True;
 CONSOLE_LOGGING_POSITION:=CONSOLE_POSITION_BOTTOM;
 LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));
end;

{$endif}

procedure SetLEScanParameters(Type_:byte;Interval,Window:Word;OwnAddressType,FilterPolicy:byte);
begin
 HciCommand(OGF_LE_CONTROL,$0b,[Type_,lo(Interval),hi(Interval),lo(Window),hi(Window),OwnAddressType,FilterPolicy]);
end;

procedure SetLEScanEnable(State,Duplicates:boolean);
var 
 Params:Array of Byte;
begin
 SetLength(Params,2);
 if State then
  Params[0]:=$01
 else
  Params[0]:=$00;
 if Duplicates then
  Params[1]:=$01
 else
  Params[1]:=$00;
 HciCommand(OGF_LE_CONTROL,$0c,Params);
end;

procedure StartPassiveScanning;
begin
 SetLEScanParameters(LL_SCAN_PASSIVE,Round(ScanInterval*ScanUnitsPerSecond),Round(ScanWindow*ScanUnitsPerSecond),$00,$00);
 SetLEScanEnable(True,False);
end;

procedure StartActiveScanning;
begin
 SetLEScanParameters(LL_SCAN_ACTIVE,Round(ScanInterval*ScanUnitsPerSecond),Round(ScanWindow*ScanUnitsPerSecond),$00,$00);
 SetLEScanEnable(True,False);
end;

procedure StopScanning;
begin
 SetLEScanEnable(False,False);
end;

// le control
procedure SetLEEventMask(Mask:QWord);
var 
 Params:array of byte;
 MaskHi,MaskLo:DWord;
begin
 MaskHi:=(Mask shr 32) and $FFFFFFFF;
 MaskLo:=Mask and $FFFFFFFF;
 SetLength(Params,8);
 Params[0]:=MaskLo and $ff;   // lsb
 Params[1]:=(MaskLo shr 8) and $ff;
 Params[2]:=(MaskLo shr 16) and $ff;
 Params[3]:=(MaskLo shr 24) and $ff;
 Params[4]:=MaskHi and $ff;   // lsb
 Params[5]:=(MaskHi shr 8) and $ff;
 Params[6]:=(MaskHi shr 16) and $ff;
 Params[7]:=(MaskHi shr 24) and $ff;
 HciCommand(OGF_LE_CONTROL,$01,Params);
end;

procedure Help;
begin
 Log('');
 Log('H - Help - display this help message');
 Log('Q - Quit - use default-config.txt');
 Log('R - Restart - use bluetooth-dev-bluetoothtest-config.txt');
 Log('');
 Log('Legend');
 Log('R/P/? Random/Public/Other MAC Address');
 Log('C/D/S/N/R Connectable/Directed/Scannable/Non-connectable/Response Ad Event Type');
 Log('');
end;

procedure SetThrottle(X:Integer);
begin
 Locomotive.CommandedThrottle:=X;
end;

procedure SleepPhase(RequestedPhase:Integer);
//var 
// Phase:Integer;
begin
 //Phase:=(ClockGetCount div 1000) Mod 10;
 // while (Phase < RequestedPhase) or (Phase > RequestedPhase + 1) do
 //  Sleep(1);
end;

function ButtonThrottleLoop(Parameter:Pointer):PtrInt;
var 
 BoostCounter:Integer;
begin
 Result:=0;
 SleepPhase(0);
 with Locomotive do
  while True do
   begin
    while ButtonThrottleCounter = 0 do
     begin
      SetThrottle(0);
      Sleep(10);
     end;
    BoostCounter:= 0;
    while (Abs(ButtonThrottleCounter) = 1) and (BoostCounter < BoostDuration) do
     begin
      SetThrottle(Sign(ButtonThrottleCounter)*Trunc(0.14*PwmRange));
      Inc(BoostCounter);
      Sleep(10);
     end;
    while ButtonThrottleCounter <> 0 do
     begin
      SetThrottle(Trunc(0.0005*ButtonThrottleCounter*PwmRange));
      Sleep(10);
     end;
   end;
end;

function DemoLoop(Parameter:Pointer):PtrInt;
const 
 CycleDutyPercent = 90;
 Accel = 0.00005;
var 
 CyclesCompletedCounter:Integer;
 CycleActive:Boolean;
 CyclePeriodSeconds:Integer;
 PhaseSeconds:Integer;
 EndActiveSeconds,EndActiveSteps:Integer;
 StepCounter:Integer;
 NewThrottle:Integer;
 ClockNow:Integer;
begin
 Result:=0;
 CyclesCompletedCounter:=0;
 CycleActive:=False;
 CyclePeriodSeconds:=3*60;
 SleepPhase(0);
 while True do
  begin
   ClockNow:=ClockGetCount div (1000*1000) - 10;
   EndActiveSeconds:=(CycleDutyPercent * CyclePeriodSeconds) div 100;
   EndActiveSteps:=EndActiveSeconds*100;
   PhaseSeconds:=ClockNow Mod CyclePeriodSeconds;
   if not CycleActive and (PhaseSeconds = 0) then
    begin
     CycleActive:=True;
     StepCounter:=0;
    end;
   if CycleActive then
    begin
     if PhaseSeconds <= EndActiveSeconds then
      begin
       if StepCounter < BoostDuration then
        SetThrottle(Trunc(0.14*PwmRange))
       else
        begin
         NewThrottle:=Min(Trunc(Accel*PwmRange*StepCounter),Trunc(0.90*PwmRange));
         NewThrottle:=Min(NewThrottle,Trunc(Accel*PwmRange*(EndActiveSteps - StepCounter)));
         NewThrottle:=Max(NewThrottle,0);
         SetThrottle(NewThrottle);
        end;
      end
     else
      begin
       CycleActive:=False;
       SetThrottle(0);
       Inc(CyclesCompletedCounter);
      end;
     Inc(StepCounter);
    end;
   Sleep(10);
  end;
end;

function LedLoop(Parameter:Pointer):PtrInt;
var 
 InterruptLatched:Boolean;
 IdleCounter:Integer;
const 
 Activity = 0;
 Power = 1;

function Interrupted:Boolean;
begin
 InterruptLatched:=InterruptLatched or (LedRequest <> 0);
 Result:=InterruptLatched;
end;

procedure Led(Which:Integer;State:Integer;Cycles:Integer);
begin
 if not Interrupted then
  begin
   case which of 
    Activity:
             begin
              if State = 1 then
               ActivityLedOn
              else
               ActivityLedOff;
             end;
    Power:
          begin
           if State = 1 then
            PowerLedOn
           else
            PowerLedOff;
          end;
   end;
   while not Interrupted and (Cycles > 0) do
    begin
     Sleep(100);
     Dec(Cycles);
    end;
  end;
end;

begin
 Result:=0;
 PowerLedEnable;
 ActivityLedEnable;
 while True do
  begin
   while not Interrupted do
    begin
     Led(Power,1,1);
     Led(Power,0,1);
     Led(Activity,1,1);
     Led(Activity,0,7);
    end;
   IdleCounter:=0;
   while Interrupted do
    begin
     if LedRequest <> 0 then
      begin
       IdleCounter:=0;
       if (LedRequest and $1) <> 0 then
        ActivityLedOn
       else
        ActivityLedOff;
       if (LedRequest and $2) <> 0 then
        PowerLedOn
       else
        PowerLedOff;
      end
     else
      begin
       ActivityLedOff;
       PowerLedOff;
       Inc(IdleCounter);
       if IdleCounter > 5 then
        InterruptLatched:=False;
      end;
     Sleep(100);
    end;
  end;
end;

function KeyboardLoop(Parameter:Pointer):PtrInt;
begin
 Result:=0;
 while True do
  begin
   if ConsoleGetKey(ch,nil) then
    case uppercase(ch) of 
     'H' : Help;
     'Q' : SystemRestart(0);
     'R' :
          begin
           RestoreBootFile('microbitdemo','config.txt');
           SystemRestart(0);
          end;
    end;
  end;
end;

function MacAddressTypeToStr(MacAddressType:Byte):String;
begin
 case MacAddressType of 
  $00:Result:='P';
  $01:Result:='R';
  else
   Result:='?';
 end;
end;

function AdEventTypeToStr(AdEventType:Byte):String;
begin
 case AdEventType of 
  $00:Result:='C';
  $01:Result:='D';
  $02:Result:='S';
  $03:Result:='N';
  $04:Result:='R';
  else
   Result:='?';
 end;
end;

function AsWord(Hi,Lo:Integer):Word;
begin
 Result:=(Hi shl 8) or Lo;
end;

function FindOrMakeMicroBitPeripheral(NewAddressString:String):PMicroBitPeripheral;
var 
 I:Integer;
begin
 Result:=Nil;
 for I:= 0 to High(MicroBitPeripherals) do
  if MicroBitPeripherals[I].AddressString = NewAddressString then
   Result:=@MicroBitPeripherals[I];
 if Result = nil then
  begin
   SetLength(MicroBitPeripherals,Length(MicroBitPeripherals) + 1);
   Result:=@MicroBitPeripherals[High(MicroBitPeripherals)];
   with Result^ do
    begin
     AddressString:=NewAddressString;
     ButtonCounter:=0;
     CarrierLost:=False;
     AssignedLocomotive:=Nil;
     AsciiMessage:='';
     NewestAsciiMessage:='';
    end;
   Log(Format('**** micro:bit ble %s newly detected',[NewAddressString]));
  end;
end;

procedure AssignOperator(ThisPeripheral:PMicroBitPeripheral);
begin
 if Locomotive.Peripheral = nil then
  begin
   Locomotive.Peripheral:=ThisPeripheral;
   ThisPeripheral^.AssignedLocomotive:=@Locomotive
  end;
end;

function SelfTestLoop(Parameter:Pointer):PtrInt;
var 
 Pwm,Pin1,Pin2:Integer;
 LastPin1,LastPin2:Integer;
 PwmAccumulator,LastPwmAccumulator:Integer;
 TimeCounter:Integer;
begin
 Result:=0;
 LastPin1:=-1;
 LastPin2:=-1;
 PwmAccumulator:=0;
 LastPwmAccumulator:=-1;
 TimeCounter:=0;
 while True do
  begin
   Pwm:=GPIOInputGet(GPIO_PIN_13);
   Inc(PwmAccumulator,Pwm);
   if TimeCounter >= 1000 then
    begin
     if PwmAccumulator <> LastPwmAccumulator then
      begin
       Log(Format('>>>> self test pwm accumulator %d',[PwmAccumulator]));
       LastPwmAccumulator:=PwmAccumulator;
      end;
     PwmAccumulator:=0;
     TimeCounter:=0;
    end;
   Pin1:=GPIOInputGet(GPIO_PIN_19);
   Pin2:=GPIOInputGet(GPIO_PIN_26);
   if (Pin1 <> LastPin1) or (Pin2 <> LastPin2) then
    begin
     Log(Format('>>>> self test changed pin1 %d pin2 %d',[Pin1,Pin2]));
     LastPin1:=Pin1;
     LastPin2:=Pin2;
    end;
   sleep(2);
   Inc(TimeCounter,2);
  end;
end;

function LocomotivePwmLoop(Locomotive:Pointer):PtrInt;
var 
 LastThrottle:Integer;
 PwmBias,Pwm,LastPwm:Integer;
 Pin1,Pin2:Integer;
 PWM0Device:PPwmDevice;
const 
 PulseFrequency = 1000;
begin
 Result:=0;
 try
  LastThrottle:=0;
  LastPwm:=-1;
  //GPIOPullSelect(GPIO_PIN_12,GPIO_PULL_NONE);
  //GPIOFunctionSelect(GPIO_PIN_12,GPIO_FUNCTION_ALT0);
  GPIOPullSelect(GPIO_PIN_6,GPIO_PULL_NONE);
  GPIOFunctionSelect(GPIO_PIN_6,GPIO_FUNCTION_OUT);
  GPIOPullSelect(GPIO_PIN_16,GPIO_PULL_NONE);
  GPIOFunctionSelect(GPIO_PIN_16,GPIO_FUNCTION_OUT);
  //GPIOPullSelect(GPIO_PIN_13,GPIO_PULL_NONE);
  //GPIOFunctionSelect(GPIO_PIN_13,GPIO_FUNCTION_IN);
  //GPIOPullSelect(GPIO_PIN_19,GPIO_PULL_NONE);
  //GPIOFunctionSelect(GPIO_PIN_19,GPIO_FUNCTION_IN);
  //GPIOPullSelect(GPIO_PIN_26,GPIO_PULL_NONE);
  //GPIOFunctionSelect(GPIO_PIN_26,GPIO_FUNCTION_IN);
  PWM0Device:=PWMDeviceFindByName('PWM0');
  PWMDeviceSetGPIO(PWM0Device,GPIO_PIN_12);
  PWMDeviceSetRange(PWM0Device,PwmRange);
  PWMDeviceSetMode(PWM0Device,PWM_MODE_MARKSPACE);
  PWMDeviceSetFrequency(PWM0Device,PwmRange*PulseFrequency);
  PWMDeviceStart(PWM0Device);
  SleepPhase(5);
  with PLocomotive(Locomotive)^ do
   while True do
    begin
     if CommandedThrottle <> LastThrottle then
      begin
       if CommandedThrottle = 0 then
        Pwm:=0
       else
        begin
         if CommandedThrottle > 0 then
          PwmBias:=Trunc(0.23*PwmRange)
         else
          PwmBias:=Trunc(0.26*PwmRange);
         Pwm:=Min(Abs(CommandedThrottle) + PwmBias,PwmRange);
        end;
       Pin1:=GPIO_LEVEL_LOW;
       Pin2:=GPIO_LEVEL_LOW;
       if CommandedThrottle > 0 then
        Pin1:=GPIO_LEVEL_HIGH
       else if CommandedThrottle < 0 then
             Pin2:=GPIO_LEVEL_HIGH;
       Log(Format('locomotive %d throttle %d range %d pin1 %d pin2 %d',[Id,CommandedThrottle,Pwm,Pin1,Pin2]));
       GPIOOutputSet(GPIO_PIN_6,Pin1);
       GPIOOutputSet(GPIO_PIN_16,Pin2);
       if Pwm <> LastPwm then
        begin
         PWMDeviceWrite(PWM0Device,Pwm);
         LastPwm:=Pwm;
        end;
       LastThrottle:=CommandedThrottle;
      end;
     Sleep(10);
    end;
 except
  on E:Exception do
       begin
        Log(Format('locomotive %d pwm exception %s',[E.Message]));
       end;
end;
ThreadHalt(0);
end;

var 
 HighWater:Integer;

procedure ParseEvent;
var 
 I:Integer;
 EventType,EventSubtype,EventLength:Byte;
 AdEventType,AddressType:Byte;
 Event:array of Byte;
 S:string;
 GetByteIndex:Integer;
 AddressString:string;
 MainType,MfrLo,MfrHi,SignatureLo,SignatureHi,SignatureAnother:Byte;
 AddressBytes:array[0 .. 5] of Byte;
 LeEventType:Byte;
 NewButtonCounter:LongWord;
 ButtonMessage:String;
 CounterByte:Byte;
 MicroEventIndex:Integer;
 MicroBitPeripheral:PMicroBitPeripheral;
 LogMarker:String;
 EventByte:Byte;
 CurrentButtons:Integer;
 Chord:LongWord;
 ChordDiscarded:Boolean;
 ChordDiscardedString:String;
 Mask,Keep:LongWord;
 PeripheralBindId:LongWord;
 PeripheralFlags:Byte;
 NewAsciiMessage:String;
 AsciiOffset:Integer;
function GetByte:Byte;
begin
 Result:=Event[GetByteIndex];
 Inc(GetByteIndex);
end;
const 
 A = 1;
 B = 2;
function IsChord(Pattern:Array of Byte):Boolean;
var 
 I:Integer;
 X:LongWord;
function Bits:Byte;
begin
 Result:=(X shr 30) and $03;
end;
begin
 // LoggingOutput(Format('IsChord',[]));
 Result:=True;
 X:=Chord;
 if Bits <> 0 then
  Result:=False;
 X:=X shl 2;
 for I:=0 to High(Pattern) do
  begin
   if Bits <> Pattern[I] then
    Result:=False;
   X:=X shl 2;
  end;
 if Bits <> 0 then
  Result:=False;
end;
begin
 NewButtonCounter:=0;
 EventType:=EventReadFirstByte;
 EventSubtype:=Readbyte;
 EventLength:=ReadByte;
 SetLength(Event,0);
 S:='';
 for I:=1 to EventLength - 1 do
  begin
   SetLength(Event,Length(Event) + 1);
   Event[I - 1]:=ReadByte;
   if I > 11 then
    begin
     S:=S + Event[I - 1].ToHexString(2);
     if I mod 4 = 3 then
      S:=S + ' ';
    end;
  end;
 ReadByte;
 if EventSubType <> $3e then
  begin
   Log(Format('ParseEvent type %02.2x subtype %02.2x length %d discarded',[EventType,EventSubType,EventLength]));
   Sleep(5*1000);
   exit;
  end;
 if S = '' then
  begin
   S:='(no data)';
   exit;
  end;
 GetByteIndex:=0;
 LeEventType:=GetByte;
 GetByte;
 if LeEventType <> $02 then
  begin
   Log(Format('ParseEvent le event type %02.2x length %d discarded',[LeEventType,EventLength]));
   Sleep(5*1000);
   exit;
  end;
 AdEventType:=GetByte;
 AddressType:=GetByte;
 AddressString:='';
 for I:=0 to 5 do
  begin
   AddressBytes[I]:=GetByte;
   AddressString:=AddressBytes[I].ToHexString(2) + AddressString;
  end;
 AddressString:=AddressString + MacAddressTypeToStr(AddressType) + AdEventTypeToStr(AdEventType);
 GetByte;
 GetByte;
 GetByte;
 GetByte;
 GetByte;
 MainType:=GetByte;
 MfrLo:=GetByte;
 MfrHi:=GetByte;
 SignatureLo:=GetByte;
 SignatureHi:=GetByte;
 SignatureAnother:=GetByte;
 if (MainType = $ff) and (AsWord(MfrHi,MfrLo) = Word(ManufacturerTesting)) and (AsWord(SignatureHi,SignatureLo) = $9755) then
  begin
  end
 else if (MainType = $ff) and (AsWord(MfrHi,MfrLo) = Word(ManufacturerTesting)) and (AsWord(SignatureHi,SignatureLo) = $9855) then
       begin
       end
 else if (MainType = $ff) and (AsWord(MfrHi,MfrLo) = Word(ManufacturerTesting)) and (SignatureLo = $78) and (SignatureHi = $f3) and (SignatureAnother = $c7) then
       begin
       end
 else if (MainType = $ff) and (AsWord(MfrHi,MfrLo) = Word(ManufacturerTesting)) and (SignatureLo = $78) and (SignatureHi = $f3) and (SignatureAnother = $c8) then
       begin
        MicroBitPeripheral:=FindOrMakeMicroBitPeripheral(AddressString);
        MicroBitPeripheral^.TimeAtLastReception:=GetTickCount;
        if MicroBitPeripheral^.CarrierLost then
         begin
          MicroBitPeripheral^.CarrierLost:=False;
          Log(Format('**** micro:bit ble %s carrier restored',[AddressString]));
         end;
        GetByteIndex:=21;
        PeripheralBindId:=GetByte;
        PeripheralBindId:=(PeripheralBindId shl 8) or GetByte;
        PeripheralBindId:=(PeripheralBindId shl 8) or GetByte;
        PeripheralFlags:=GetByte;
        CounterByte:=GetByte;
        if (CounterByte and $80) = 0 then
         begin
          NewButtonCounter:=CounterByte;
          AsciiOffset:=GetByteIndex + (CounterByte + 3) div 4;
          while AsciiOffset <= High(Event) do
           begin
            if Event[AsciiOffset] = 0 then
             break;
            NewAsciiMessage:=NewAsciiMessage + Char(Event[AsciiOffset]);
            Inc(AsciiOffset);
           end;
          if (NewAsciiMessage <> '') and ( MicroBitPeripheral^.AsciiMessage = '') then
           begin
            MicroBitPeripheral^.NewestAsciiMessage:=NewAsciiMessage;
            MicroBitPeripheral^.AsciiMessage:=NewAsciiMessage;
            Log(Format('AsciiMessage <%s>',[NewAsciiMessage]));
           end
          else if NewAsciiMessage <> MicroBitPeripheral^.NewestAsciiMessage then
                begin
                 MicroBitPeripheral^.NewestAsciiMessage:=NewAsciiMessage;
                 Log(Format('NewestAsciiMessage <%s>',[NewAsciiMessage]));
                end;
         end
        else
         begin
          NewbuttonCounter:=(MicroBitPeripheral^.ButtonCounter and $ffffff80) or (CounterByte and $7f);
          if NewButtonCounter < MicroBitPeripheral^.ButtonCounter then
           Inc(NewButtonCounter,128);
         end;
        if NewButtonCounter < MicroBitPeripheral^.ButtonCounter then
         begin
          MicroBitPeripheralReset(MicroBitPeripheral,Format('**** micro:bit ble %s seems to have restarted',[Addressstring]));
         end
        else
         begin
          S:='';
          while GetByteIndex <= High(Event) do
           begin
            EventByte:=GetByte;
            S:=S + Char(Ord('0') + ((EventByte shr 6) and $03));
            S:=S + Char(Ord('0') + ((EventByte shr 4) and $03));
            S:=S + Char(Ord('0') + ((EventByte shr 2) and $03));
            S:=S + Char(Ord('0') + ((EventByte shr 0) and $03));
           end;
          while MicroBitPeripheral^.ButtonCounter <> NewButtonCounter do
           begin
            Inc(MicroBitPeripheral^.ButtonCounter);
            LogMarker:='    ';
            MicroEventIndex:=NewButtonCounter - MicroBitPeripheral^.ButtonCounter;
            if MicroEventIndex > HighWater then
             begin
              HighWater:=MicroEventIndex;
              Log(Format('high water now %d',[HighWater]));
             end;
            //Log(Format('counter %d new %d index %d %s',[MicroBitPeripheral^.ButtonCounter,NewButtonCounter,MicroEventIndex,S]));
            if MicroEventIndex >= 80 then
             begin
              Log(Format('new button counter %d current index %d %s',[NewButtonCounter,MicroBitPeripheral^.ButtonCounter,MicroEventIndex,S]));
              MicroBitPeripheralReset(MicroBitPeripheral,Format('**** micro:bit ble %s unable to determine event %d',[MicroBitPeripheral^.AddressString,MicroBitPeripheral^.ButtonCounter]));
              Sleep(60*1000);
             end
            else
             begin
              GetByteIndex:=22 + 4 + MicroEventIndex div 4;
              Chord:=GetByte;
              Chord:=(Chord shl 8) or GetByte;
              Chord:=(Chord shl 8) or GetByte;
              Chord:=(Chord shl 8) or GetByte;
              case MicroEventIndex mod 4 of 
               0: Chord:=Chord shl 0;
               1: Chord:=Chord shl 2;
               2: Chord:=Chord shl 4;
               3: Chord:=Chord shl 6;
              end;
              CurrentButtons:=(Chord shr 30) and $03;
              if CurrentButtons = $01 then
               LedRequest:=$02
              else if CurrentButtons = $02 then
                    LedRequest:=$01
              else
               LedRequest:=Currentbuttons;
              case CurrentButtons of 
               0:
                 ButtonMessage:='Released    ';
               1:
                 ButtonMessage:='A down      ';
               2:
                 ButtonMessage:='B down      ';
               else
                ButtonMessage:='????????????';
              end;
              LoggingOutput(Format('%s micro:bit ble %s event number %03.3d %s',[LogMarker,AddressString,MicroBitPeripheral^.ButtonCounter,ButtonMessage]));
              if ((Chord shr 30) and $03) = 0 then
               begin
                Mask:=$30000000;
                Keep:=$c0000000;
                while Mask <> 0 do
                 begin
                  if (Chord and Mask) = 0 then
                   begin
                    Chord:=Chord and Keep;
                    break;
                   end
                  else
                   begin
                    Mask:=Mask shr 2;
                    Keep:=(Keep shr 2) or $c0000000;
                   end;
                 end;
                ChordDiscarded:=True;
                begin
                 LoggingOutput(Format('analyzing chord %08.8x',[Chord]));
                 if IsChord([A,A,A,A]) then
                  begin
                   if MicroBitPeripheral^.AssignedLocomotive = nil then
                    begin
                     AssignOperator(MicroBitPeripheral);
                     ChordDiscarded:=False;
                    end;
                  end;
                 if IsChord([A,B]) then
                  begin
                   if MicroBitPeripheral^.AssignedLocomotive <> nil then
                    MicroBitPeripheral^.AssignedLocomotive^.ButtonThrottleCounter:=0;
                   ChordDiscarded:=False;
                  end;
                 if IsChord([B,A]) then
                  begin
                   if MicroBitPeripheral^.AssignedLocomotive <> nil then
                    MicroBitPeripheral^.AssignedLocomotive^.ButtonThrottleCounter:=0;
                   ChordDiscarded:=False;
                   SystemRestart(0);
                  end;
                 if IsChord([B]) then
                  begin
                   if MicroBitPeripheral^.AssignedLocomotive <> nil then
                    Inc(MicroBitPeripheral^.AssignedLocomotive^.ButtonThrottleCounter,1);
                   ChordDiscarded:=False;
                  end;
                 if IsChord([A]) then
                  begin
                   if MicroBitPeripheral^.AssignedLocomotive <> nil then
                    Dec(MicroBitPeripheral^.AssignedLocomotive^.ButtonThrottleCounter,1);
                   ChordDiscarded:=False;
                  end;
                end;
                if ChordDiscarded then
                 ChordDiscardedString:='discarded'
                else
                 ChordDiscardedString:='ok';
                Log(Format('**** micro:bit ble %s chord %04.4x %s high water %d',[Addressstring,Chord,ChordDiscardedString,HighWater]));
               end;
             end;
           end;
         end;
       end
end;

function GetIpAddress:String;
var 
 Winsock2TCPClient:TWinsock2TCPClient;
begin
 Log('Obtaining IP address ...');
 Winsock2TCPClient:=TWinsock2TCPClient.Create;
 Result:=Winsock2TCPClient.LocalAddress;
 while (Result = '') or (Result = '0.0.0.0') or (Result = '255.255.255.255') do
  begin
   Sleep(100);
   Result:=Winsock2TCPClient.LocalAddress;
  end;
 Winsock2TCPClient.Free;
 Log(Format('Obtaining IP address done %s',[Result]));
end;

function TSpaDocument.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
begin
 Result:=False;
 if AHost = nil then Exit;
 if ARequest = nil then Exit;
 if AResponse = nil then Exit;
 AResponse.Version:=HTTP_VERSION;
 AResponse.Status:=HTTP_STATUS_OK;
 AResponse.Reason:=HTTP_REASON_200;
 AResponse.ContentString:=SpaBuffer.DataString;
 Result:=True;
end;

function TJsonDocument.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
begin
 Result:=False;
 if AHost = nil then Exit;
 if ARequest = nil then Exit;
 if AResponse = nil then Exit;
 AResponse.Version:=HTTP_VERSION;
 AResponse.Status:=HTTP_STATUS_OK;
 AResponse.Reason:=HTTP_REASON_200;
 AResponse.ContentString:=AResponse.ContentString + '{"message":"json status works"}' + HTTP_LINE_END;
 Result:=True;
end;

var 
 SpaDocument:TSpaDocument;
 JsonDocument:TJsonDocument;

procedure StartHttpServer;
begin
 HTTPListener:=THTTPListener.Create;
 WebStatusRegister(HTTPListener,'','',True);

 SpaDocument:=TSpaDocument.Create;
 SpaDocument.Name:='/spa';
 HTTPListener.RegisterDocument('',SpaDocument);;

 JsonDocument:=TJsonDocument.Create;
 JsonDocument.Name:='/json';
 HTTPListener.RegisterDocument('',JsonDocument);;

 HTTPListener.Active:=True;
end;

var 
 I:Integer;

begin
 Console1 := ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOP,False);
 ConsoleWindowSetBackcolor(Console1,COLOR_BLACK);
 ConsoleWindowSetForecolor(Console1,COLOR_YELLOW);
 ConsoleWindowClear(Console1);

 RestoreBootFile('default','config.txt');
 StartLogging;
 Log('prototype-microbit-as-ultibo-peripheral');

 Sleep(2*1000);
 Log('building spa buffer ...');
 BuildSpaBuffer;
 Log('building spa buffer done');
 GetIpAddress;
 StartHttpServer;

 BeginThread(@LedLoop,Nil,LedLoopHandle,THREAD_STACK_DEFAULT_SIZE);
 BeginThread(@KeyboardLoop,Nil,KeyboardLoopHandle,THREAD_STACK_DEFAULT_SIZE);
 // BeginThread(@SelfTestLoop,Nil,SelfTestLoopHandle,THREAD_STACK_DEFAULT_SIZE);
 Help;

 SetLength(MicroBitPeripherals,0);
 with Locomotive do
  begin
   Id:=1;
   CommandedThrottle:=0;
   ButtonThrottleCounter:=0;
   Peripheral:=Nil;
   PwmLoopHandle:=INVALID_HANDLE_VALUE;
  end;
 BeginThread(@LocomotivePwmLoop,@Locomotive,Locomotive.PwmLoopHandle,THREAD_STACK_DEFAULT_SIZE);
 EndOfScan;

 if IsBlueToothAvailable then
  begin
   ReadByteCounter:=0;
   OpenUart0;
   ResetChip;
   try
    BCMLoadFirmware;
   except
    on E:Exception do
         begin
          LoggingOutput(Format('load exception %s',[E.Message]));
         end;
  end;
 SetLEEventMask($ff);
 Log('Init complete');
 ScanCycleCounter:=0;
 ReadByteCounter:=0;
 HighWater:=-1;
 if SysUtils.GetEnvironmentVariable('RUN_DEMO_OPERATIONS') = '1' then
  BeginThread(@DemoLoop,Nil,DemoLoopHandle,THREAD_STACK_DEFAULT_SIZE)
 else
  BeginThread(@ButtonThrottleLoop,Nil,ButtonThrottleLoopHandle,THREAD_STACK_DEFAULT_SIZE);
 while True do
  begin
   ReadBackLog:=0;
   Margin:=High(Margin);
   StartActiveScanning;
   Log('Receiving scan data');
   ScanIdle:=True;
   ScanRxCount:=0;
   while True do
    ParseEvent;
  end;
end;
ThreadHalt(0);
end.
