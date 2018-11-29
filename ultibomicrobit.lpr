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
 Serial,USBCDCACM,
 Http,WebStatus,
 USB,
 Storage;

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
 Console1,Console2:TWindowHandle;
 RxChar:Char;
 RxCount:LongWord;
 RxBuffer:String;
 I:Integer;
 HTTPListener:THTTPListener;
 UsbMonitorThreadHandle:TThreadHandle;

procedure Log(S:String);
begin
 LoggingOutput(S);
end;

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

function UsbDeviceGetEstimatedBoardConnectorNumber(Device:PUSBDevice;var ConnectorNumber:Integer):LongWord;
var
 UsbHub:PUsbHub;
 UsbPort:PUsbPort;
 HubNumber,PortNumber:Integer;
begin
 ConnectorNumber:=0;

 Result:=ERROR_INVALID_PARAMETER;
 if Device = Nil then
  exit;

 UsbHub:=UsbDeviceGetHub(Device);
 HubNumber:=-1;
 if UsbHub <> Nil then
  HubNumber:=UsbHub^.HubId;
 UsbPort:=UsbDeviceGetPort(Device);
 PortNumber:=-1;
 if UsbPort <> Nil then
  PortNumber:=UsbPort^.Number;

 case BoardGetType of
  BOARD_TYPE_RPI3B:
   begin
    if (HubNumber = 1) and (PortNumber = 2) then
     ConnectorNumber:=1
    else if (HubNumber = 1) and (PortNumber = 3) then
     ConnectorNumber:=2
    else if (HubNumber = 1) and (PortNumber = 4) then
     ConnectorNumber:=3
    else if (HubNumber = 1) and (PortNumber = 5) then
     ConnectorNumber:=4
   end;
  BOARD_TYPE_RPI3B_PLUS:
   begin
    if (HubNumber = 2) and (PortNumber = 2) then
     ConnectorNumber:=1
    else if (HubNumber = 2) and (PortNumber = 3) then
     ConnectorNumber:=2
    else if (HubNumber = 1) and (PortNumber = 3) then
     ConnectorNumber:=3
    else if (HubNumber = 1) and (PortNumber = 2) then
     ConnectorNumber:=4
   end;
 end;

 if ConnectorNumber <> 0 then
  Result:=ERROR_SUCCESS
 else
  Result:=ERROR_NOT_VALID;
end;

var
 NoMicroBitsAttached:Boolean;

function UsbDeviceEnumeration(UsbDevice:PUSBDevice;Data:Pointer):LongWord;
var
 ConnectorNumber:Integer;
 MassStorageInterface:PUsbInterface;
 MassStorageDevice:PStorageDevice;
 DiskDevice:TDiskDevice;
 DiskDrive:TDiskDrive;
 DriveLetter:String;
 SerialInterface:PUsbInterface;
 SerialDevice:PSerialDevice;
 SerialDeviceName:String;
begin
 UsbDeviceGetEstimatedBoardConnectorNumber(UsbDevice,ConnectorNumber);
 with UsbDevice^ do
  begin
   if (Descriptor^.idVendor = $0D28) and (Descriptor^.idProduct = $0204) then
    begin
     NoMicroBitsAttached:=False;
     DriveLetter:='(none)';
     MassStorageInterface:=USBDeviceFindInterfaceByClass(UsbDevice,USB_CLASS_CODE_MASS_STORAGE,USB_SUBCLASS_MASS_STORAGE_SCSI,USB_PROTOCOL_MASS_STORAGE_BBB);
     if MassStorageInterface <> Nil then
      begin
       MassStorageDevice:=StorageDeviceCheck(MassStorageInterface^.DriverData);
       if MassStorageDevice <> Nil then
        begin
         DiskDevice:=FileSysDriver.GetDeviceByStorage(MassStorageDevice,False,FILESYS_LOCK_NONE);
         if DiskDevice <> Nil then
          begin
           DiskDrive:=FileSysDriver.GetDriveByDevice(DiskDevice,False,FILESYS_LOCK_NONE);
           if DiskDrive <> Nil then
            DriveLetter:=DiskDrive.Name;
          end;
        end;
      end;
     SerialDeviceName:='(none)';
     SerialInterface:=USBDeviceFindInterfaceByClass(UsbDevice,USB_CLASS_CODE_CDC_DATA);
     if SerialInterface <> Nil then
      begin
       SerialDevice:=SerialDeviceCheck(SerialInterface^.DriverData);
       if SerialDevice <> Nil then
        SerialDeviceName:=SerialDevice^.Device.DeviceName;
      end;
     ConsoleWindowWriteLn(Console2,Format('Micro:Bit on connector %d disk drive %s serial device %s',[ConnectorNumber,DriveLetter,SerialDeviceName]));
     ConsoleWindowWriteLn(Console2,Format('    copy \hex\peripheral.hex %s',[DriveLetter]));
     ConsoleWindowWriteLn(Console2,'');
    end;
// ConsoleWindowWriteLn(Console2,Format('[%d] %04.4x:%04.4x %s %d.%d %s %s %s %s',[ConnectorNumber,Descriptor^.idVendor,Descriptor^.idProduct,Host^.Device.DeviceName,Depth,PortNumber,Device.DeviceName,Product,Manufacturer,SerialNumber]));
  end;
 Result:=ERROR_SUCCESS;
end;

function UsbMonitorThread(Parameter:Pointer):PtrInt;
var
 Device:PUSBDevice;
begin
 Result:=0;
 while True do
  begin
   ConsoleWindowClear(Console2);
   ConsoleWindowWriteLn(Console2,'The shell is at the right');
   ConsoleWindowWriteLn(Console2,'    dir \hex');
   ConsoleWindowWriteLn(Console2,'');
   NoMicroBitsAttached:=True;
   USBDeviceEnumerate(@UsbDeviceEnumeration,Nil);
   if NoMicroBitsAttached then
    ConsoleWindowWriteLn(Console2,'There are no Micro:Bits attached');
   Sleep(1*1000);
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

procedure StartHttpServer;
begin
 HTTPListener:=THTTPListener.Create;
 WebStatusRegister(HTTPListener,'','',False);
 HTTPListener.Active:=True;
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
 Console1:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOMLEFT,True);
 Console2:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPLEFT,True);
 for I:=Low(MicroBits) to High(MicroBits) do
  begin
   MicroBits[I]:=TMicroBit.Create(I);
   MicroBits[I].Start;
  end;
 BeginThread(@UsbMonitorThread,Nil,UsbMonitorThreadHandle,THREAD_STACK_DEFAULT_SIZE);
 StartHttpServer;
 ThreadHalt(0);
end.
