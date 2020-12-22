unit SCCOL;

interface
uses SharedObject, DynLibs, Math, SysUtils;

type
  TSharedListener = class
      procedure Compile(const INSTR: ansistring);
      procedure CompileAsync(const ASYNC:TAsync);
      procedure CompileFunc(const INSTR: ansistring; const ARGUMENTS: array of pchar);
      procedure SendError(const CODE: word);
      procedure SetMBB(const NUM: longword; const VALUE: pchar);
  end;
  TDynamicFunction = function(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
  TSCCOLFunction = record
    ID: ansistring;
    PascalProcedure: TDynamicFunction;
  end;
  TSCCOLLibrary = record
    ID: ansistring;
    Handle: TLibHandle;
  end;
  PSCCOLFunction = ^TSCCOLFunction;
  TSCCOLFunctions = array of TSCCOLFunction;
  PSCCOLFunctions = ^TSCCOLFunctions;

const
  SCCOLVersion = '1.0.0.0';

var
  Aliases: TMapArray;
  CodeLog: ansistring;
  Constants: TMapArray;
  Errors: TMapArray;
  ExternalLibraries: array of TSCCOLLibrary;
  ExternalFunctions: TSCCOLFunctions;
  HighFunctions: TSCCOLFunctions;
  InputCommand: ansistring;
  LogCode: boolean = false;
  MBB: TStringArray;
  Operators: TSCCOLFunctions;
  Parameters: TParameters;
  SCCOLInput: ansistring;
  Shared: TSharedObject;
  SharedListener: TSharedListener;

      //PARAMETERS
  ExitOnFinish: boolean = true;
  ShowInput: boolean = true;

procedure CompileBatch(INSTR: ansistring);
function LoadFile(const PATH: ansistring):boolean;

implementation

function IsComment(const INSTR: string):boolean;
begin
  IsComment:=false;
  if Length(INSTR)>1 then
  begin
    if (INSTR[1] = '/') and (INSTR[2] = '/') then IsComment:=true;
  end;
end;

function CompileAsync(p: pointer):ptrint;
var
  ArgNum, CompileString: ansistring;
  AsyncFunc: TAsync;
  I: word;
begin
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
  SCCOL.CompileBatch(INSTR);
end;
procedure TSharedListener.CompileAsync(const ASYNC:TAsync);
var
  ASYNCMem: PAsync;
begin
  New(ASYNCMem);
  ASYNCMem^ := ASYNC;
  BeginThread(@SCCOL.CompileAsync, ASYNCMem);
end;
procedure TSharedListener.CompileFunc(const INSTR: ansistring; const ARGUMENTS: array of pchar);
var
  ArgNum, CompileString: ansistring;
  I: word;
begin
  CompileString := INSTR;
  for I := 1 to Length(ARGUMENTS) do begin
    Str(I-1, ArgNum);
    //SCCOL.Print('%^A'+ArgNum);
    CompileString := StringReplace(CompileString, '%^A'+ArgNum, ARGUMENTS[I-1], [rfReplaceAll]);
  end;
  SCCOL.CompileBatch(CompileString);
end;
procedure TSharedListener.SendError(const CODE: word);
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
  Shared.Print('Error code '+CodeString+': "'+ErrorString+'".');
end;
procedure TSharedListener.SetMBB(const NUM: longword; const VALUE: pchar);
begin
  SetLength(SCCOL.MBB, Max(Length(SCCOL.MBB), NUM+1));
  SCCOL.MBB[NUM] := VALUE;
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
    Shared.SendError(5);
    Exit;
  end;
  if IsNaN(NUM) then begin
    Shared.SendError(6);
    Exit;
  end;
  Val(NUM, NumReal);
  if NumReal < 0 then begin
    Shared.SendError(5);
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
  for I := 1 to Length(HIGH^) do
    if HIGH^[I-1].ID = SCCOLID then Exit;
  FuncHandle := TDynamicFunction(GetProcedureAddress(LIB, ID));
  if FuncHandle = nil then begin
    Shared.SendError(16);
    LoadCommand := false;
    Exit;
  end;
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
  I, J: word;
  ID: ansistring;
  TempLib: TSCCOLLibrary;
begin
  ID := ExtractFileName(PATH);
  for I := 1 to Length(ExternalLibraries) do
    if UpCase(ExternalLibraries[I-1].ID) = UpCase(ID) then begin
      Shared.Print('Detected override library, not importing.');
      TempLib := ExternalLibraries[I-1];
      ExternalLibraries[I-1] := ExternalLibraries[Length(ExternalLibraries)-1];
      ExternalLibraries[Length(ExternalLibraries)-1] := TempLib;
      Exit;
    end;
  LibHandle := LoadLibrary('LIB'+DirectorySeparator+PATH+'.'+SharedSuffix);
  if LibHandle = 0 then begin
    LibHandle := LoadLibrary(PATH);
    if LibHandle = 0 then begin
      LibHandle := LoadLibrary(PATH+'.'+SharedSuffix);
      if LibHandle = 0 then begin
        Shared.SendError(19);
        LoadExternal := false;
        Exit;
      end;
    end;
  end;
  SetLength(ExternalLibraries, Length(ExternalLibraries)+1);
  with ExternalLibraries[Length(ExternalLibraries)-1] do begin
    Handle := LibHandle;
  end;
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
        Shared.SendError(19);
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
    Shared.SendError(23);
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
    Shared.SendError(22);
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
      Shared.SendError(5);
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
  Arguments: TStringArray;
  PArguments: array of pchar;
  Command, Partition: ansistring;
  FuncResult: ansistring;
  HasBrackets: boolean;
  I, J: word;
  MBBPointer: plongword;
begin
     {HIGH COMPILE LEVEL COMMANDS}
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
      {CODE LOG}
  if LogCode then begin
    CodeLog += Original+';'+sLineBreak;
    Exit;
  end;
  if (IsComment(INSTR)) or (INSTR = '') or (INSTR = sLineBreak) then Exit;
     {CONSTANTS}
  for I := 1 to Length(Constants) do
    INSTR := StringReplace(INSTR, '%^'+Constants[I-1].ID, Constants[I-1].Value, [rfReplaceAll]);
     {PARAMETERS}
  for I := 1 to Length(Parameters) do
    if Pos('%^'+Parameters[I-1].ID, INSTR) <> 0 then
      INSTR := StringReplace(INSTR, '%^'+Parameters[I-1].ID, Parameters[I-1].PascalFunction(Shared, nil), [rfReplaceAll]);
     {MBB}
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
      {OPERATORS}
  Operate(INSTR);
      {SPECIAL}
  INSTR := StringReplace(INSTR, '||FUNCTION||', '%^f', [rfReplaceAll]);
  INSTR := StringReplace(INSTR, '||SEMICOLON||', ';', [rfReplaceAll]);
      {FUNCTIONS}
  I := Pos('%^f', INSTR);
  while I <> 0 do begin
    Command := '';
    Partition := '';
    FuncResult := '';
    HasBrackets := false;
    //PARSE COMMAND
    for J := I+3 to Length(INSTR) do begin
      if (INSTR[J] = '(') or (INSTR[J] = ' ') then Break;
      Command += INSTR[J];
    end;
    //PARSE ALIAS
    for J := 1 to Length(Aliases) do begin
      if Pos(Aliases[J-1].Value, Command) <> 0 then begin
        Delete(INSTR, I+3, Length(Command));
        Insert(Aliases[J-1].ID, INSTR, I+3);
        Command := Aliases[J-1].ID;
        WriteLn(INSTR);
        //Command := StringReplace(Command, Aliases[J-1].Value, Aliases[J-1].ID, [rfReplaceAll]);
      end;
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
        Shared.SendError(3);
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
      {COMPILER}
  if INSTR[1] = '<' then begin
    Command := '';
    for I := 2 to Length(INSTR) do begin
      if INSTR[I] in [' ', '>'] then Break;
      Command += INSTR[I];
    end;
    ReadArguments(INSTR, Arguments, Length(Command)+2);
    case UpCase(Command) of
      'ALIAS': begin
        if Length(Arguments) = 2 then begin
          SetLength(Aliases, Length(Aliases)+1);
          with Aliases[Length(Aliases)-1] do begin
            ID := Arguments[0];
            Value := Arguments[1];
          end;
        end
        else
          Shared.SendError(5);
      end;
      'CONST': begin
        if Length(Arguments) = 2 then begin
          SetLength(Constants, Length(Constants)+1);
          with Constants[Length(Constants)-1] do begin
            ID := Arguments[0];
            Value := Arguments[1];
          end;
        end else
          Shared.SendError(5);
      end;
      'DEFINE': begin
        if Length(Arguments) = 2 then
          LoadCommand(ExternalLibraries[Length(ExternalLibraries)-1].Handle, Arguments[0], Arguments[1], @ExternalFunctions)
        else
          Shared.SendError(5);
      end;
      'ERROR': begin
        if Length(Arguments) = 2 then begin
          SetLength(Errors, Length(Errors)+1);
          with Errors[Length(Errors)-1] do begin
            ID := Arguments[0];
            Value := Arguments[1];
          end;
        end else
          Shared.SendError(5);
      end;
      'EXIT': Halt;
      'EXTERNAL': begin
        if Length(Arguments) = 1 then
          LoadExternal(Arguments[0])
        else
          Shared.SendError(5);
      end;
      'HDEFINE': begin
        if Length(Arguments) = 2 then
          LoadCommand(ExternalLibraries[Length(ExternalLibraries)-1].Handle, Arguments[0], Arguments[1], @HighFunctions)
        else
          Shared.SendError(5);
      end;
      'INCLUDE': begin
        if Length(Arguments) = 1 then
          LoadFile(Arguments[0])
        else
          Shared.SendError(5);
      end;
      'OPERATOR': begin
        if Length(Arguments) = 2 then
          LoadCommand(ExternalLibraries[Length(ExternalLibraries)-1].Handle, Arguments[0], Arguments[1], @Operators)
        else
          Shared.SendError(5);
      end;
      'PARAM': begin
        if Length(Arguments) = 2 then
          LoadParameter(ExternalLibraries[Length(ExternalLibraries)-1].Handle, Arguments[0], Arguments[1])
        else
          Shared.SendError(5);
      end;
      else Shared.SendError(21);
    end;
    Exit;
  end;
      {COMMANDS}
  Command := '';
  SetLength(Arguments, 0);
  SetLength(Arguments, 1);
  for I := 1 to Length(INSTR) do begin
    if INSTR[I] = ' ' then Break;
    Command += INSTR[I];
  end;
      {ALIASES}
  for J := 1 to Length(Aliases) do begin
    Command := StringReplace(Command, Aliases[J-1].Value, Aliases[J-1].ID, [rfReplaceAll]);
    //Shared.Print('"'+Aliases[J-1].Value+'":"'+Aliases[J-1].ID+'" = "'+Command+'"');
  end;
  ReadArguments(INSTR, Arguments, I+1);
  SetLength(PArguments, Length(Arguments));
  for I := 1 to Length(Arguments) do
    PArguments[I-1] := PChar(Arguments[I-1]);
  for I := 1 to Length(ExternalFunctions) do begin
    if UpCase(ExternalFunctions[I-1].ID) = UpCase(Command) then begin
      ExternalFunctions[I-1].PascalProcedure(Shared, PArguments);
      Exit;
    end;
  end;
  Shared.SendError(21);
end;
procedure CompileBatch(INSTR: ansistring);
var
  Commands: TStringArray;
  I, J: word;
  Original: ansistring;
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
    if Pos('<BREAK>', Commands[I-1]) = 1 then Exit;
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
  Shared.LogCode := @LogCode;
  Shared.MBB := @MBB;
  Shared.Parameters := @Parameters;
  Shared.SCCOLInput := @SCCOLInput;
  Shared.SendError := @SharedListener.SendError;
  Shared.SetMBB := @SharedListener.SetMBB;

  Shared.ExitOnFinish := @ExitOnFinish;
  Shared.ShowInput := @ShowInput;
end.

