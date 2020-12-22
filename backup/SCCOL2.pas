{$APPTYPE CONSOLE}
{$MODE OBJFPC}
program SCCOL2;
uses SCCOL{$IFDEF WINDOWS}, Windows{$ELSE}, Crt{$ENDIF}, SysUtils;

type
  EventManager = class
    procedure Get(const KEY: boolean);
    procedure Print(const INSTR: ansistring);
  end;

const
  Libraries: array[0..3] of ansistring = ('SYSTEM', 'FILES', 'MATH', 'STRINGS');

var
  Manager: EventManager;
  ParamNum: byte;

{$IFDEF WINDOWS}
function ReadKey: char;
var
  Mode:DWORD;
begin
  if GetConsoleMode(TextRec(Input).Handle, Mode) then
    SetConsoleMode(TextRec(Input).Handle, Mode and not ENABLE_LINE_INPUT);
  Read(Result);
  SetConsoleMode(TextRec(INPUT).Handle, Mode);
end;
{$ENDIF}

procedure EventManager.Get(const KEY: boolean);
begin
  if KEY then SCCOLInput := ReadKey else ReadLn(SCCOLInput);
end;
procedure EventManager.Print(const INSTR: ansistring);
begin
  WriteLn(INSTR);
end;

procedure CheckLibraries;
var
  Check: boolean;
  LibStr, SearchStr: ansistring;
  I, Found, NotFound: word;
begin
  Found := 0;
  NotFound := 0;
  for LibStr in Libraries do begin
    Write(LibStr);
    SearchStr := 'LIB'+DirectorySeparator+LibStr+'.SCCOL';;
    if not FileExists(SearchStr) then begin
      SearchStr := LibStr;
      if not FileExists(SearchStr) then begin
        SearchStr := LibStr+'.SCCOL';
        if not FileExists(SearchStr) then begin
          SearchStr := '';
          for I := 10 downto Length(LibStr) do SearchStr += #32;
          WriteLn(SearchStr+': NOT FOUND');
          NotFound += 1;
          Continue;
        end;
      end;
    end;
    SearchStr := '';
    for I := 10 downto Length(LibStr) do SearchStr += #32;
    WriteLn(SearchStr+': FOUND');
    Found += 1;
    CompileBatch('<INCLUDE '+LibStr+'>');
  end;
  WriteLn('Found ',Found,'/',Found+NotFound,' libraries.');
  WriteLn('All found libraries have been loaded.');
  WriteLn('Heap size   : ',GetFPCHeapStatus.CurrHeapSize);
  WriteLn('Used heap size   : ',GetFPCHeapStatus.CurrHeapUsed);
  WriteLn;
  WriteLn('=== Press any key to exit. ===');
  ReadKey;
end;
procedure ShowHelp;
begin
  WriteLn('S-COM Command Oriented Language version '+SCCOLVersion+' Debug Pre-release Build');
  WriteLn('Copyright (c) GreatCorn 2020');
  WriteLn('Usage: SCCOL [OPTION/FILE]');
  WriteLn('Options (case-insensitive):');
  WriteLn('-L     : check for and return all included libraries');
  WriteLn('-H     : print this help message');
  WriteLn('-?     : print this help message');
  WriteLn;
  WriteLn('FILE   : load and execute SCCOL program file');
end;

begin
  Manager := EventManager.Create;
  Shared.Get := @Manager.Get;
  Shared.Print := @Manager.Print;

  if ParamCount > 0 then
  begin
    ParamNum := 1;
    while ParamNum <= ParamCount do
    begin
      case UpCase(ParamStr(paramNum)) of
        '-L': begin
          CheckLibraries;
          ExitOnFinish := true;
        end;
        '-H': begin
          ShowHelp;
          ExitOnFinish := true;
        end;
        '-?': begin
          ShowHelp;
          ExitOnFinish := true;
        end;
        else begin
          ExitOnFinish := true;
          LoadFile(ParamStr(paramNum));
        end;
      end;
      ParamNum += 1;
    end;
  end;
  if not ExitOnFinish then begin
    {$IFDEF WINDOWS}SetConsoleTitle('SCCOL');{$ENDIF}
    WriteLn('Welcome to the S-COM Command Oriented Language (SCCOL) console.');
    WriteLn('SCCOL v'+SCCOLVersion+' Debug Pre-release Build');
    WriteLn('Copyright (c) GreatCorn 2020');
    WriteLn('To exit the console type in <EXIT>.');
  end;
  while not ExitOnFinish do begin
    if ShowInput then begin
      Write('SCCOL>>>');
      ReadLn(InputCommand);
      CompileBatch(InputCommand);
    end;
  end;
end.

