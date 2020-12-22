program SCCOL2;
uses SharedObject, DynLibs, Math, SysUtils;

type
  TSharedListener = class
      procedure Compile(const INSTR: ansistring);
      procedure CompileAsync(const ASYNC:TAsync);
      procedure CompileFunc(const INSTR: ansistring; const ARGUMENTS: array of pchar);
      procedure Get(const KEY: boolean);
      procedure Print(const INSTR: ansistring);
      procedure SendError(const CODE: byte);
      procedure SetMBB(const NUM: longword; const VALUE: pchar);
  end;
  TDynamicFunction = function(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
  TParamFunc = function(var SH: TSharedObject):pchar;
  TParameter = record
    ID: ansistring;
    PascalFunction: TParamFunc;
  end;
  TSCCOLFunction = record
    ID: ansistring;
    PascalProcedure: TDynamicFunction;
  end;
  PSCCOLFunction = ^TSCCOLFunction;
  TSCCOLFunctions = array of TSCCOLFunction;
  PSCCOLFunctions = ^TSCCOLFunctions;

var
  CodeLog: ansistring;
  Constants: TMapArray;
  Errors: TMapArray;
  ExitOnFinish: boolean = false;
  ExternalLibraries: array of TLibHandle;
  ExternalFunctions: TSCCOLFunctions;
  HighFunctions: TSCCOLFunctions;
  InputCommand: ansistring;
  LogCode: boolean = false;
  MBB: TStringArray;
  Operators: TSCCOLFunctions;
  ParamNum: byte;
  Parameters: array of TParameter;
  SCCOLInput: ansistring;
  Shared: TSharedObject;
  SharedListener: TSharedListener;
  ShowInput: boolean = true;

procedure Print(const INSTR: ansistring);
begin
  WriteLn(INSTR);
end;
procedure CompileBatch(INSTR: ansistring); forward;
function IsComment(const INSTR: string):boolean;
begin
  IsComment:=false;
  if Length(INSTR)>1 then
  begin
    if (INSTR[1] = '/') and (INSTR[2] = '/') then IsComment:=true;
  end;
end;
procedure SendError(const CODE: byte);
var
  CodeString: shortstring;
  ErrorString: ansistring;
  I: word;
begin
  {case CODE of
    //SYNTAX ERRORS
    0: ErrorString := 'UNSPECIFIED ERROR';
    1: ErrorString := 'UNEXPECTED TYPE FOUND WHEN USING AN OPERATOR';
    2: ErrorString := '"(" OR " " EXPECTED';
    3: ErrorString := '")" OR ";" EXPECTED';
    4: ErrorString := 'INVALID IDENTIFIER STRING GIVEN';
    5: ErrorString := 'INVALID VALUE GIVEN';
    6: ErrorString := 'UNEXPECTED VALUE TYPE GIVEN';
    7: ErrorString := 'UNKNOWN PARAMETER';
    8: ErrorString := 'INVALID FORMAT';
    //MEMORY ERRORS
    16: ErrorString := 'FUNCTION NOT FOUND';
    17: ErrorString := 'PROCEDURE NOT FOUND';
    18: ErrorString := 'UNDEFINED MEMORY BUFFER BLOCK';
    19: ErrorString := 'FILE OR LIBRARY NOT FOUND';
    20: ErrorString := 'DIRECTORY NOT FOUND';
    21: ErrorString := 'COMMAND NOT FOUND';
    22: ErrorString := 'PARAMETER NOT FOUND';
    23: ErrorString := 'READING/WRITING FILE FAILED';
    24: ErrorString := 'DIRECTORY CREATING FAILED';
    //NETWORK ERRORS
    32: ErrorString := 'CONNECTION FAILED';
    33: ErrorString := 'INVALID ADRESS GIVEN';
    34: ErrorString := 'DATA SENDING FAILED';
    35: ErrorString := 'HOST FAILED';
    36: ErrorString := 'ACCEPTING CONNECTION FAILED';
    //SECURITY ERRORS
    48: ErrorString := 'UNAUTHORIZED ACCESS ATTEMPT';
    49: ErrorString := 'SECURITY ACCESS LEVEL INVALID';

    else ErrorString := 'UNDEFINED ERROR';
  end;}
  Str(CODE, CodeString);
  for I := 1 to Length(Errors) do
    if Errors[I-1].ID = CodeString then begin
      ErrorString := Errors[I-1].Value;
      Break;
    end;
  if ErrorString = '' then ErrorString := 'UNDEFINED ERROR';
  Print('Error code '+CodeString+': "'+ErrorString+'".');
end;

function CompileAsync(p: pointer):ptrint;
var
  ArgNum, CompileString: ansistring;
  AsyncFunc: TAsync;
  I: word;
begin
  //WriteLn('WELL NO SHIT BITCH CUNT');
  AsyncFunc := TAsync(p^);
  CompileString := AsyncFunc.Code;
  for I := 1 to Length(AsyncFunc.Arguments) do begin
    Str(I-1, ArgNum);
    //Print('%^A'+ArgNum);
    CompileString := StringReplace(CompileString, '%^A'+ArgNum, AsyncFunc.Arguments[I-1], [rfReplaceAll]);
  end;
  CompileBatch(CompileString);
  Dispose(PAsync(P));
end;

procedure TSharedListener.Compile(const INSTR: ansistring);
begin
  SCCOL2.CompileBatch(INSTR);
end;
procedure TSharedListener.CompileAsync(const ASYNC:TAsync);
var
  ASYNCMem: PAsync;
  ID: longword;
begin
  New(ASYNCMem);
  ASYNCMem^ := ASYNC;
  BeginThread(@SCCOL2.CompileAsync, ASYNCMem);
  //SCCOL2.CompileAsync(@ASYNC);
end;
procedure TSharedListener.CompileFunc(const INSTR: ansistring; const ARGUMENTS: array of pchar);
var
  ArgNum, CompileString: ansistring;
  I: word;
begin
  CompileString := INSTR;
  for I := 1 to Length(ARGUMENTS) do begin
    Str(I-1, ArgNum);
    //SCCOL2.Print('%^A'+ArgNum);
    CompileString := StringReplace(CompileString, '%^A'+ArgNum, ARGUMENTS[I-1], [rfReplaceAll]);
  end;
  SCCOL2.CompileBatch(CompileString);
end;
procedure TSharedListener.Get(const KEY: boolean);
begin
  if KEY then begin

  end;
end;
procedure TSharedListener.Print(const INSTR: ansistring);
begin
  SCCOL2.Print(INSTR);
end;
procedure TSharedListener.SendError(const CODE: byte);
begin
  SCCOL2.SendError(CODE);
end;
procedure TSharedListener.SetMBB(const NUM: longword; const VALUE: pchar);
begin
  SetLength(SCCOL2.MBB, Max(Length(SCCOL2.MBB), NUM+1));
  SCCOL2.MBB[NUM] := VALUE;
end;

function IsNaN(const INSTR: shortstring):boolean;
var
  Bool: boolean;
  I: Word;
begin
  if (INSTR = ' ') or (INSTR = '') then begin
    IsNaN := true;
    Exit;
  end;
  for I := 1 to Length(INSTR) do begin
    Bool := INSTR[I] in ['0'..'9','-','.',' '];
    if not Bool then begin
      IsNaN := true;
      Exit;
    end;
  end;
  IsNaN := false;
end;
function Split(const INSTR: ansistring; const DELIM: char):TStringArray;
var
  I: integer;
  Parse: ansistring = '';
  StringAmount: integer = 0;
begin
  for I:=1 to Length(INSTR) do
  begin
    Parse := Parse+INSTR[I];
    if (INSTR[i] = DELIM) or (I = Length(INSTR)) then
    begin
      SetLength(Split, stringAmount+1);
      if Parse[Length(Parse)] = DELIM then Delete(Parse, Length(Parse), 1);
      Split[stringAmount] := Parse;
      inc(stringAmount);
      Parse := '';
    end;
  end;
end;

function MBBEvaluate(const NUM: shortstring):plongword;
var
  NumReal: double;
  NumWord: longword;
begin
  MBBEvaluate := nil;
  if NUM = '' then begin
    SendError(5);
    Exit;
  end;
  if IsNaN(NUM) then begin
    SendError(6);
    Exit;
  end;
  Val(NUM, NumReal);
  if NumReal < 0 then begin
    SendError(5);
    Exit;
  end;
  NumWord := Trunc(NumReal);
  MBBEvaluate := @NumWord;
end;

function LoadCommand(var LIB: TLibHandle; const ID, SCCOLID: ansistring; const HIGH: PSCCOLFunctions):boolean;
var
  FuncHandle: TDynamicFunction;
  I: word;
begin
  FuncHandle := TDynamicFunction(GetProcedureAddress(LIB, ID));
  if FuncHandle = nil then begin
    SendError(16);
    LoadCommand := false;
    Exit;
  end;
  for I := 1 to Length(HIGH^) do
    if HIGH^[I-1].ID = SCCOLID then Exit;
  SetLength(HIGH^, Length(HIGH^)+1);
  with HIGH^[Length(HIGH^)-1] do begin
    PascalProcedure := FuncHandle;
    ID := SCCOLID;
  end;
  LoadCommand := true;
end;
function LoadExternal(const PATH: ansistring):boolean;
var
  LibHandle: TLibHandle;
begin
  LibHandle := LoadLibrary('LIB'+DirectorySeparator+PATH+'.'+SharedSuffix);
  if LibHandle = 0 then begin
    LibHandle := LoadLibrary(PATH);
    if LibHandle = 0 then begin
      LibHandle := LoadLibrary(PATH+'.'+SharedSuffix);
      if LibHandle = 0 then begin
        SendError(19);
        LoadExternal := false;
        Exit;
      end;
    end;
  end;
  SetLength(ExternalLibraries, Length(ExternalLibraries)+1);
  ExternalLibraries[Length(ExternalLibraries)-1] := LibHandle;
  LoadExternal := true;
end;
function LoadFile(const PATH: ansistring):boolean;
var
  InputFile: TextFile;
  InputPath, InputString, FileString: ansistring;
begin
  InputPath := 'LIB'+DirectorySeparator+PATH+'.SCCOL';
  if not FileExists(InputPath) then begin
    InputPath := PATH;
    if not FileExists(InputPath) then begin
      InputPath := PATH+'.SCCOL';
      if not FileExists(InputPath) then begin
        SendError(19);
        LoadFile := false;
        Exit;
      end;
    end;
  end;
  AssignFile(InputFile, InputPath);
  InputString := '';
  try
    Reset(InputFile);
    while not EOF(InputFile) do begin
      ReadLn(InputFile, FileString);
      InputString += FileString+sLineBreak;
    end;
    CloseFile(InputFile);
    CompileBatch(InputString);
    LoadFile := true;
  except
    SendError(23);
    LoadFile := false;
  end;
end;
function LoadParameter(var LIB: TLibHandle; const ID, SCCOLID: ansistring):boolean;
var
  ParamHandle: TParamFunc;
  I: word;
begin
  ParamHandle := TParamFunc(GetProcedureAddress(LIB, ID));
  if ParamHandle = nil then begin
    SendError(22);
    LoadParameter := false;
    Exit;
  end;
  for I := 1 to Length(Parameters) do
    if Parameters[I-1].ID = SCCOLID then Exit;
  SetLength(Parameters, Length(Parameters)+1);
  with Parameters[Length(Parameters)-1] do begin
    ID := SCCOLID;
    PascalFunction := ParamHandle;
  end;
  LoadParameter := true;
end;

//COMPILING
procedure ReadArguments(const INSTR: ansistring; var ARGUMENTS: TStringArray; I: word);
var
  QuoteBlock: boolean = false;
begin
  while I <= Length(INSTR) do begin
    case INSTR[I] of
      ' ': begin
        if not QuoteBlock then begin
          I += 1;
          SetLength(ARGUMENTS, Length(ARGUMENTS)+1);
          Continue;
        end;
      end;
      '"': begin
        //WriteLn('QUOTE BLOCK');
        I += 1;
        QuoteBlock := not QuoteBlock;
        Continue;
      end;
      '>': begin
        if not QuoteBlock then Break;
      end;
    end;
    if INSTR[I] = '>' then
      if not QuoteBlock then Break;
    ARGUMENTS[Length(ARGUMENTS)-1] += INSTR[I];
    I += 1;
  end;
  if Length(ARGUMENTS) = 1 then
    if ARGUMENTS[0] = '' then SetLength(ARGUMENTS, 0);
end;
procedure Operate(var INSTR: ansistring);
var
  LeftSide, RightSide, Op: ansistring;
  Operating: word;
  I, J: word;
begin
  for Operating := 1 to Length(Operators) do begin
    Op := Operators[Operating-1].ID;
    I := Pos('$'+Op, INSTR);
    if I = 0 then Continue;
    if (I = 1) or (Length(INSTR) = Length(Op)+1) or (I+Length(Op) = Length(INSTR)) then begin
      SendError(5);
      Continue;
    end;
    while I <> 0 do begin
      LeftSide := ''; RightSide := '';
          //PARSING LEFT SIDE
      for J := I-1 downto 0 do begin
        if INSTR[J] in [' ', '"'] then Break;
        LeftSide := INSTR[J]+LeftSide;
      end;
          //PARSING RIGHT SIDE
      for J := I+1+Length(Op) to Length(INSTR) do begin
        if INSTR[J] in [' ', '"'] then Break;
        RightSide += INSTR[J];
      end;
      //Print('"'+LeftSide+'" operated by "'+Op+'" with "'+RightSide+'".');
      INSTR := StringReplace(INSTR, LeftSide+'$'+Op+RightSide, Operators[Operating-1].PascalProcedure(Shared, [PChar(LeftSide), PChar(RightSide)]), [rfReplaceAll]);
      I := Pos('$'+Op, INSTR);
    end;
  end;
end;
procedure Compile(INSTR, ORIGINAL: ansistring);
var
  ArgumentCount: byte;
  Arguments: TStringArray;
  PArguments: array of pchar;
  Command, Partition: ansistring;
  FuncResult: ansistring;
  HasBrackets: boolean;
  I, J: word;
  MBBPointer: plongword;
begin
     //HIGH COMPILE LEVEL COMMANDS
  Command := '';
  for J := 1 to Length(INSTR) do begin
    if INSTR[J] = ' ' then Break;
    Command += INSTR[J];
  end;
  for J := 1 to Length(HighFunctions) do begin
    if HighFunctions[J-1].ID = UpCase(Command) then begin
      HighFunctions[J-1].PascalProcedure(Shared, []);
      Exit;
    end;
  end;
      //CODE LOG
  if LogCode then begin
    CodeLog += Original+';'+sLineBreak;
    Exit;
  end;
  if (IsComment(INSTR)) or (INSTR = '') or (INSTR = sLineBreak) then Exit;
     //PARSING CONSTANTS
  for I := 1 to Length(Constants) do
    INSTR := StringReplace(INSTR, '%^'+Constants[I-1].ID, Constants[I-1].Value, [rfReplaceAll]);
     //PARSING PARAMETERS
  for I := 1 to Length(Parameters) do
    INSTR := StringReplace(INSTR, '%^'+Parameters[I-1].ID, Parameters[I-1].PascalFunction(Shared), [rfReplaceAll]);
     //PARSING MBB
  I := Pos('%^', INSTR);
  while I <> 0 do begin
    Command := '';
    for J := I+2 to Length(INSTR) do begin
      if (IsNan(INSTR[J])) or (INSTR[J] = ' ') then Break;
      Command := Command+INSTR[J];
    end;
    Partition := '';
    MBBPointer := MBBEvaluate(Command);
    if MBBPointer <> nil then begin
      J := MBBPointer^;
      if J < Length(MBB) then
        Partition := MBB[J];
    end else Partition := '';
    INSTR := StringReplace(INSTR, '%^'+Command, Partition, [rfReplaceAll]);
    I := Pos('%^', INSTR);
  end;
      //OPERATORS
  Operate(INSTR);
      //SPECIAL
  INSTR := StringReplace(INSTR, '||FUNCTION||', '%^f', [rfReplaceAll]);
  INSTR := StringReplace(INSTR, '||SEMICOLON||', ';', [rfReplaceAll]);
      //FUNCTIONS
  I := Pos('%^f', INSTR);
  while I <> 0 do begin
    Command := '';
    Partition := '';
    FuncResult := '';
    HasBrackets := false;
    for J := I+3 to Length(INSTR) do begin
      if (INSTR[J] = '(') or (INSTR[J] = ' ') then Break;
      Command += INSTR[J];
    end;
    if INSTR[I+3+Length(Command)] = '(' then begin
      for J := I+4+Length(Command) to Length(INSTR) do begin
        if (INSTR[J] = ')') then begin
          HasBrackets := true;
          Break;
        end;
        Partition += INSTR[J];
      end;
      if not HasBrackets then begin
        SendError(3);
        Exit;
      end;
    end;
    SetLength(Arguments, 0);
    SetLength(Arguments, 1);
    ReadArguments(Partition, Arguments, 1);
    SetLength(PArguments, 0);
    SetLength(PArguments, Length(Arguments));
    for J := 1 to Length(Arguments) do
      PArguments[J-1] := PChar(Arguments[J-1]);
    for J := 1 to Length(ExternalFunctions) do begin
    if UpCase(ExternalFunctions[J-1].ID) = UpCase(Command) then begin
        FuncResult := ExternalFunctions[J-1].PascalProcedure(Shared, PArguments);
      end;
    end;
    //Print('"%^f'+Command+'('+Partition+')"');
    if HasBrackets then
      INSTR := StringReplace(INSTR, '%^f'+Command+'('+Partition+')', FuncResult, [rfReplaceAll])
    else
      INSTR := StringReplace(INSTR, '%^f'+Command, FuncResult, [rfReplaceAll]);
    I := Pos('%^f', INSTR);
  end;
  if Length(INSTR) = 0 then Exit;
      //PARSING COMPILER
  if INSTR[1] = '<' then begin
    ArgumentCount := 0;
    Command := '';
    for I := 2 to Length(INSTR) do begin
      if INSTR[I] in [' ', '>'] then Break;
      Command += INSTR[I];
    end;
    ReadArguments(INSTR, Arguments, Length(Command)+2);
    case UpCase(Command) of
      'CONST': begin
        if Length(Arguments) = 2 then begin
          SetLength(Constants, Length(Constants)+1);
          with Constants[Length(Constants)-1] do begin
            ID := Arguments[0];
            Value := Arguments[1];
          end;
        end else
          SendError(5);
      end;
      'DEFINE': begin
        if Length(Arguments) = 2 then
          LoadCommand(ExternalLibraries[Length(ExternalLibraries)-1], Arguments[0], Arguments[1], @ExternalFunctions)
        else
          SendError(5);
      end;
      'ERROR': begin
        if Length(Arguments) = 2 then begin
          SetLength(Errors, Length(Errors)+1);
          with Errors[Length(Errors)-1] do begin
            ID := Arguments[0];
            Value := Arguments[1];
          end;
        end else
          SendError(5);
      end;
      'EXIT': Halt;
      'EXTERNAL': begin
        if Length(Arguments) = 1 then
          LoadExternal(Arguments[0])
        else
          SendError(5);
      end;
      'HDEFINE': begin
        if Length(Arguments) = 2 then
          LoadCommand(ExternalLibraries[Length(ExternalLibraries)-1], Arguments[0], Arguments[1], @HighFunctions)
        else
          SendError(5);
      end;
      'INCLUDE': begin
        if Length(Arguments) = 1 then
          LoadFile(Arguments[0])
        else
          SendError(5);
      end;
      'INPUT': begin
        if Length(Arguments) = 1 then begin
          if Arguments[0] = '1' then ShowInput := true
          else ShowInput := false;
        end else
          SendError(5);
      end;
      'NOEXIT': ExitOnFinish := false;
      'OPERATOR': begin
        //Print(Arguments[1]);
        if Length(Arguments) = 2 then
          LoadCommand(ExternalLibraries[Length(ExternalLibraries)-1], Arguments[0], Arguments[1], @Operators)
        else
          SendError(5);
      end;
      'PARAM': begin
        if Length(Arguments) = 2 then
          LoadParameter(ExternalLibraries[Length(ExternalLibraries)-1], Arguments[0], Arguments[1])
        else
          SendError(5);
      end;
      else SendError(21);
    end;
    Exit;
  end;
      //PARSING COMMANDS
  Command := '';
  SetLength(Arguments, 0);
  SetLength(Arguments, 1);
  for I := 1 to Length(INSTR) do begin
    if INSTR[I] = ' ' then Break;
    Command += INSTR[I];
  end;
  ReadArguments(INSTR, Arguments, I+1);
  SetLength(PArguments, Length(Arguments));
  for I := 1 to Length(Arguments) do
    PArguments[I-1] := PChar(Arguments[I-1]);
  {Print('"'+UpCase(Command)+'"');
  Print(PArguments[1]);}
  for I := 1 to Length(ExternalFunctions) do begin
    if UpCase(ExternalFunctions[I-1].ID) = UpCase(Command) then begin
      ExternalFunctions[I-1].PascalProcedure(Shared, PArguments);
      Exit;
    end;
  end;
  SendError(21);
end;
procedure CompileBatch(INSTR: ansistring);
var
  Commands: TStringArray;
  I, J: word;
  Command, Original: ansistring;
begin
  INSTR := StringReplace(INSTR, '\(', '||BRACKETOPEN||', [rfReplaceAll]);
  INSTR := StringReplace(INSTR, '\)', '||BRACKETCLOSE||', [rfReplaceAll]);
  INSTR := StringReplace(INSTR, '\$', '||DOLLAR||', [rfReplaceAll]);
  INSTR := StringReplace(INSTR, '%^f', '||FUNCTION||', [rfReplaceAll]);
  INSTR := StringReplace(INSTR, '\"', '||QUOTES||', [rfReplaceAll]);
  INSTR := StringReplace(INSTR, '\;', '||SEMICOLON||', [rfReplaceAll]);
  Commands := Split(INSTR, ';');
  for I := 1 to Length(Commands) do begin
    Original := Commands[I-1];
    for J := 1 to Length(Commands[I-1]) do begin
      if not (Commands[I-1][J] in [#9, #10, #13, #32]) then begin
        Delete(Commands[I-1], 1, J-1);
        Break;
      end;
    end;
    Command := '';
    Compile(Commands[I-1], Original);
  end;
end;

begin
  Shared := TSharedObject.Create;
  SharedListener := TSharedListener.Create;
  Shared.CodeLog := @CodeLog;
  Shared.Compile := @SharedListener.Compile;
  Shared.CompileAsync := @SharedListener.CompileAsync;
  Shared.CompileFunc := @SharedListener.CompileFunc;
  Shared.Get := @SharedListener.Get;
  Shared.LogCode := @LogCode;
  Shared.MBB := @MBB;
  Shared.Print := @SharedListener.Print;
  Shared.SCCOLInput := @SCCOLInput;
  Shared.SendError := @SharedListener.SendError;
  Shared.SetMBB := @SharedListener.SetMBB;

  if ParamCount > 0 then
  begin
    ParamNum := 1;
    while ParamNum <= ParamCount do
    begin
      case UpCase(ParamStr(paramNum)) of
        '-H': begin
          WriteLn(AnsiString(#150));
        end;
        else begin
          ExitOnFinish := true;
          LoadFile(ParamStr(paramNum));
        end;
      end;
      ParamNum += 1;
    end;
  end;
  if not ExitOnFinish then while true do begin
    if ShowInput then begin
      Write('SCCOL>>>');
      ReadLn(InputCommand);
      CompileBatch(InputCommand);
    end;
  end;
end.

