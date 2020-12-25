library SCSystem;
uses
  Math, SharedObject, SysUtils;

var
  DeclareID: ansistring;
  Functions: TStringArray;
  FunctionID: TStringArray;
  FunctionResult: ansistring;

function SCIsNaN(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar; forward;
function IndexFunction(const NAME: ansistring):word;
var
  I: word;
begin
  for I := 1 to Length(FunctionID) do begin
    if UpCase(FunctionID[I-1]) = UpCase(NAME) then begin
      IndexFunction := I;
      Exit;
    end;
  end;
  IndexFunction := 0;
end;
procedure CleanArray(var INARRAY: TStringArray);
var
  LastBlock: longword = 0;
  I: longword;
begin
  if Length(INARRAY) > 0 then
  begin
    for I:=1 to Length(INARRAY) do
    begin
      if INARRAY[I-1] <> '' then begin
        LastBlock := I-1;
      end;
    end;
    SetLength(INARRAY, LastBlock+1);
    if (Length(INARRAY) = 1) and (INARRAY[0] = '') then SetLength(INARRAY, 0);
    //WriteLn(Length(INARRAY));
  end;
end;

         { OPERATORS }
function NumberArguments(var SH: TSharedObject; var ARGUMENTS: array of pchar; var LeftSide, RightSide: double): boolean;
begin
  if Length(ARGUMENTS) < 2 then begin
    SH.SendError(5);
    NumberArguments := false;
    Exit;
  end;
  if (SCIsNaN(SH, ARGUMENTS) = 'TRUE') then begin
    SH.SendError(1);
    NumberArguments := false;
    Exit;
  end;
  Val(ARGUMENTS[0], LeftSide);
  Val(ARGUMENTS[1], RightSide);
  NumberArguments := true;
end;
function SCAdd(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  LeftSide, RightSide: double;
begin
  if Length(ARGUMENTS) < 2 then begin
    SH.SendError(5);
    Exit;
  end;
  //SH.Print('DLL receives: "'+ARGUMENTS[0]+'" and "'+ARGUMENTS[1]+'".');
  if SCIsNaN(SH, ARGUMENTS) = 'TRUE' then begin
    SCAdd := StrCat(ARGUMENTS[0], ARGUMENTS[1]);
    Exit;
  end;
  Val(ARGUMENTS[0], LeftSide);
  Val(ARGUMENTS[1], RightSide);
  LeftSide += RightSide;
  SCAdd := PChar(StringReplace(FloatToStr(LeftSide), ',', '.', [rfReplaceAll]));
end;
function SCSub(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  LeftSide, RightSide: double;
  EndResult: ansistring;
begin
  if Length(ARGUMENTS) < 2 then begin
    SH.SendError(5);
    Exit;
  end;
  if (SCIsNaN(SH, ARGUMENTS[0]) = 'TRUE') and (SCIsNaN(SH, ARGUMENTS[1]) = 'FALSE') then begin
    Val(ARGUMENTS[1], RightSide);
    EndResult := ARGUMENTS[0];
    Delete(EndResult, Length(EndResult)-Trunc(RightSide)+1, Trunc(RightSide));
    SCSub := PChar(EndResult);
    Exit;
  end
  else if (SCIsNaN(SH, ARGUMENTS[0]) = 'FALSE') and (SCIsNaN(SH, ARGUMENTS[1]) = 'TRUE') then begin
    Val(ARGUMENTS[0], LeftSide);
    EndResult := ARGUMENTS[1];
    Delete(EndResult, 1, Trunc(LeftSide));
    SCSub := PChar(EndResult);
    Exit;
  end
  else if (SCIsNaN(SH, ARGUMENTS) = 'TRUE') then begin
    SH.SendError(1);
    Exit;
  end;
  Val(ARGUMENTS[0], LeftSide);
  Val(ARGUMENTS[1], RightSide);
  LeftSide -= RightSide;
  SCSub := PChar(StringReplace(FloatToStr(LeftSide), ',', '.', [rfReplaceAll]));
end;
function SCMul(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  LeftSide, RightSide: double;
begin
  if not NumberArguments(SH, ARGUMENTS, LeftSide, RightSide) then Exit;
  LeftSide *= RightSide;
  SCMul := PChar(StringReplace(FloatToStr(LeftSide), ',', '.', [rfReplaceAll]));
end;
function SCDiv(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  LeftSide, RightSide: double;
begin
  if not NumberArguments(SH, ARGUMENTS, LeftSide, RightSide) then Exit;
  LeftSide /= RightSide;
  SCDiv := PChar(StringReplace(FloatToStr(LeftSide), ',', '.', [rfReplaceAll]));
end;
function SCPow(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  LeftSide, RightSide, Temp: double;
begin
  if not NumberArguments(SH, ARGUMENTS, LeftSide, RightSide) then Exit;
  if Abs(LeftSide) < 1E-15 then LeftSide := 0.0; if Abs(RightSide) < 1E-15 then RightSide := 0.0;
  if LeftSide = 0.0 then Temp := 0.0;
  if RightSide = 0.0 then Temp := 1.0;

  if (LeftSide < 0) and (RightSide < 0) then Temp := 1/Exp(-RightSide*Ln(-LeftSide))
  else if (LeftSide < 0) and (RightSide >= 0) then Temp := Exp(RightSide*Ln(-LeftSide))
  else if (LeftSide > 0) and (RightSide < 0) then Temp := 1/Exp(-RightSide*Ln(LeftSide))
  else if (LeftSide > 0) and (RightSide >= 0) then Temp := Exp(RightSide*Ln(LeftSide));

  if (LeftSide < 0) and (Frac(RightSide / 2.0) <> 0.0) then Temp := -Temp;
  SCPow := PChar(StringReplace(FloatToStr(Temp), ',', '.', [rfReplaceAll]));
end;
function SCMod(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  LeftSide, RightSide: double;
begin
  if not NumberArguments(SH, ARGUMENTS, LeftSide, RightSide) then Exit;
  LeftSide := LeftSide - RightSide * Trunc(LeftSide / RightSide);
  SCMod := PChar(StringReplace(FloatToStr(LeftSide), ',', '.', [rfReplaceAll]));
end;
function SCEqGr(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  LeftSide, RightSide: double;
  EndResult: ansistring = 'FALSE';
begin
  if not NumberArguments(SH, ARGUMENTS, LeftSide, RightSide) then Exit;
  if LeftSide >= RightSide then EndResult := 'TRUE';
  SCEqGr := PChar(EndResult);
end;
function SCEqLe(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  LeftSide, RightSide: double;
  EndResult: ansistring = 'FALSE';
begin
  if not NumberArguments(SH, ARGUMENTS, LeftSide, RightSide) then Exit;
  if LeftSide <= RightSide then EndResult := 'TRUE';
  SCEqLe := PChar(EndResult);
end;
function SCGr(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  LeftSide, RightSide: double;
  EndResult: ansistring = 'FALSE';
begin
  if not NumberArguments(SH, ARGUMENTS, LeftSide, RightSide) then Exit;
  if LeftSide > RightSide then EndResult := 'TRUE';
  SCGr := PChar(EndResult);
end;
function SCLe(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  LeftSide, RightSide: double;
  EndResult: ansistring = 'FALSE';
begin
  if not NumberArguments(SH, ARGUMENTS, LeftSide, RightSide) then Exit;
  if LeftSide < RightSide then EndResult := 'TRUE';
  SCLe := PChar(EndResult);
end;
function SCEq(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  LeftSide, RightSide: double;
  LeftStr, RightStr: ansistring;
  EndResult: ansistring = 'FALSE';
begin
  if Length(ARGUMENTS) < 2 then begin
    SH.SendError(5);
    Exit;
  end;
  if SCIsNaN(SH, ARGUMENTS) = 'TRUE' then begin
    LeftStr := ARGUMENTS[0]; RightStr := ARGUMENTS[1];
    if LeftStr = RightStr then EndResult := 'TRUE';
    SCEq := PChar(EndResult);
    Exit;
  end;
  Val(ARGUMENTS[0], LeftSide);
  Val(ARGUMENTS[1], RightSide);
  if LeftSide = RightSide then EndResult := 'TRUE';
  SCEq := PChar(EndResult);
end;
function SCNotEq(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  LeftSide, RightSide: double;
  LeftStr, RightStr: ansistring;
  EndResult: ansistring = 'FALSE';
begin
  if Length(ARGUMENTS) < 2 then begin
    SH.SendError(5);
    Exit;
  end;
  if SCIsNaN(SH, ARGUMENTS) = 'TRUE' then begin
    LeftStr := ARGUMENTS[0]; RightStr := ARGUMENTS[1];
    if LeftStr <> RightStr then EndResult := 'TRUE';
    SCNotEq := PChar(EndResult);
    Exit;
  end;
  Val(ARGUMENTS[0], LeftSide);
  Val(ARGUMENTS[1], RightSide);
  if LeftSide <> RightSide then EndResult := 'TRUE';
  SCNotEq := PChar(EndResult);
end;

         { PARAMETERS }
function SCExit(var SH: TSharedObject; VALUE: pchar):pchar;
begin
  if VALUE <> nil then
    if (UpCase(VALUE) = 'TRUE') or (VALUE = '1') then SH.ExitOnFinish^ := true else SH.ExitOnFinish^ := false
  else
    if SH.ExitOnFinish^ = true then SCExit := 'TRUE' else SCExit := 'FALSE';
end;
function SCInput(var SH: TSharedObject; VALUE: pchar):pchar;
begin
  if VALUE <> nil then
    if (UpCase(VALUE) = 'TRUE') or (VALUE = '1') then SH.ShowInput^ := true else SH.ShowInput^ := false
  else
    if SH.ShowInput^ = true then SCInput := 'TRUE' else SCInput := 'FALSE';
end;
function SCR:pchar;
begin
  SCR := PChar(StringReplace(FloatToStr(Random), ',', '.', [rfReplaceAll]))
end;

         { COMMANDS }

function SCACall(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  I, J: word;
  PArguments: array of pchar;
  AsyncFunc: TAsync;
begin
  if Length(ARGUMENTS) < 1 then begin
    SH.SendError(5);
    Exit;
  end;
  for I := 1 to Length(FunctionID) do
    if UpCase(FunctionID[I-1]) = UpCase(Arguments[0]) then begin
      if Length(ARGUMENTS) > 1 then begin
        SetLength(PArguments, Length(ARGUMENTS)-1);
        for J := 0 to Length(ARGUMENTS)-2 do
          PArguments[J] := PChar(ARGUMENTS[J+1]);
      end;
      AsyncFunc.Code := Functions[I-1];
      AsyncFunc.Arguments := PArguments;
      SH.CompileAsync(AsyncFunc);
      //BeginThread(@CompileAsync);
      Exit;
    end;
  SH.SendError(16);
end;

function SCCall(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  I, J: word;
  PArguments: array of pchar;
begin
  if Length(ARGUMENTS) < 1 then begin
    SH.SendError(5);
    Exit;
  end;
  for I := 1 to Length(FunctionID) do
    if UpCase(FunctionID[I-1]) = UpCase(Arguments[0]) then begin
      if Length(ARGUMENTS) > 1 then begin
        SetLength(PArguments, Length(ARGUMENTS)-1);
        for J := 0 to Length(ARGUMENTS)-2 do
          PArguments[J] := PChar(ARGUMENTS[J+1]);
      end;
      FunctionResult := '';
      SH.CompileFunc(Functions[I-1], PArguments);
      SCCall := PChar(FunctionResult);
      Exit;
    end;
  SH.SendError(16);
end;

function SCChar(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Value: double;
  ValStr: ansistring;
begin
  if Length(ARGUMENTS) < 1 then begin
    SH.SendError(5);
    Exit;
  end;
  if Length(ARGUMENTS) > 1 then begin
    if SCIsNaN(SH, ARGUMENTS[1]) = 'TRUE' then begin
      SH.SendError(6);
      Exit;
    end;
    Val(ARGUMENTS[1], Value);
    ValStr := ARGUMENTS[0][Trunc(Value)];
    SCChar := PChar(ValStr);
    Exit;
  end;
  Val(ARGUMENTS[0], Value);
  ValStr := Chr(Trunc(Value));
  SCChar := PChar(ValStr);
end;

function SCEnd(var SH: TSharedObject):pchar;
var
  I: word;
begin
  if not SH.LogCode^ then SH.Compile('<EXIT>');
  I := IndexFunction(DeclareID);
  if I > 0 then begin
    Functions[I-1] := SH.CodeLog^;
    Exit;
  end;
  SetLength(Functions, Length(Functions)+1);
  Functions[Length(Functions)-1] := SH.CodeLog^;
  SetLength(FunctionID, Length(FunctionID)+1);
  FunctionID[Length(FunctionID)-1] := DeclareID;
  SH.LogCode^ := false;
end;

function SCError(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Num: word;
begin
  if Length(ARGUMENTS) < 1 then begin
    SH.SendError(5);
    Exit;
  end;
  if SCIsNaN(SH, ARGUMENTS[0]) = 'TRUE' then begin
    SH.SendError(6);
    Exit;
  end;
  Val(ARGUMENTS[0], Num);
  SH.SendError(Num);
end;

function SCFree(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  BlockReal: double;
  I, J: longword;
begin
  if Length(ARGUMENTS) < 1 then begin
    SH.SendError(5);
    Exit;
  end;
  if SCIsNaN(SH, ARGUMENTS[0]) = 'TRUE' then begin
    I := IndexFunction(ARGUMENTS[0]);
    if I = 0 then begin
      SH.SendError(16);
      Exit;
    end;
    FunctionID[I-1] := '';
    Functions[I-1] := '';
    CleanArray(FunctionID);
    CleanArray(Functions);
    Exit;
  end;
  Val(ARGUMENTS[0], BlockReal);
  I := Trunc(BlockReal);
  if Length(ARGUMENTS) > 1 then begin
    if SCIsNaN(SH, ARGUMENTS[1]) = 'TRUE' then begin
      SH.SendError(6);
      Exit;
    end;
    Val(ARGUMENTS[1], BlockReal);
    J := Trunc(BlockReal);
  end;
  if J > Length(SH.MBB^)-1 then J := Length(SH.MBB^)-1;
  if Length(SH.MBB^) > 0 then
    for I := I to J do begin
      //Dispose(PAnsiString(@SH.MBB^[I]));
      SH.SetMBB(I, '');
    end;
  CleanArray(SH.MBB^);
end;

function SCFunc(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
begin
  if Length(ARGUMENTS) < 1 then begin
    SH.SendError(5);
    Exit;
  end;
  DeclareID := ARGUMENTS[0];
  SH.CodeLog^ := '';
  SH.LogCode^ := true;
end;

function SCGet(var SH: TSharedObject):pchar;
begin
  SH.Get(false);
  SCGet := PChar(SH.SCCOLInput^);
end;

function SCIf(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  I: word;
begin
  if Length(ARGUMENTS) < 2 then begin
    SH.SendError(5);
    Exit;
  end;
  if UpCase(ARGUMENTS[0]) = 'TRUE' then
    for I := 1 to Length(ARGUMENTS)-1 do begin
      SH.Compile(ARGUMENTS[I]);
    end;
end;

function SCInt(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Value: double;
begin
  if Length(ARGUMENTS) < 1 then begin
    SH.SendError(5);
    Exit;
  end;
  if SCIsNaN(SH, ARGUMENTS[0]) = 'TRUE' then begin
    SH.SendError(6);
    Exit;
  end;
  Val(ARGUMENTS[0], Value);
  SCInt := PChar(IntToStr(Trunc(Value)));
end;

function SCIsNaN(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Bool: boolean;
  Argument: pchar;
  I: Word;
begin
  if Length(ARGUMENTS) < 1 then begin
    SH.SendError(5);
    Exit;
  end;
  for Argument in ARGUMENTS do begin
    //SH.Print('IsNaN: "'+ARGUMENT+'".');
    if (Argument = ' ') or (Argument = '') then begin
      SCIsNaN := 'TRUE';
      Exit;
    end;
    //SH.Print('Testing for numbers:');
    for I := 0 to Length(Argument)-1 do begin
      //SH.Print(Argument[I]);
      Bool := Argument[I] in ['0'..'9','-','.',' '];
      if not Bool then begin
        SCIsNaN := 'TRUE';
        Exit;
      end;
    end;
    SCIsNaN := 'FALSE';
  end;
end;

function SCKey(var SH: TSharedObject):pchar;
begin
  SH.Get(true);
  SCKey := PChar(SH.SCCOLInput^);
end;

function SCLength(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Final, ValStr: ansistring;
begin
  if Length(ARGUMENTS) < 1 then
    Str(Length(SH.MBB^), ValStr)
  else if Length(ARGUMENTS) = 1 then
    Str(Length(ARGUMENTS[0]), ValStr);
  else
    Str(Length(ARGUMENTS), ValStr);
  SCLength := PChar(ValStr);
end;

function SCMem(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  I: longword;
  IStr: shortstring;
begin
  if Length(ARGUMENTS) > 0 then begin
    for I := 1 to Length(FunctionID) do
      if UpCase(FunctionID[I-1]) = UpCase(ARGUMENTS[0]) then begin
        SH.Print(FunctionID[I-1]+':');
        SH.Print(Functions[I-1]);
        Exit;
      end;
    SH.SendError(16);
    Exit;
  end;
  for I := 1 to Length(SH.MBB^) do
  begin
    Str(I-1, IStr);
    if not (SH.MBB^[I-1] = '') then SH.Print(IStr+': '+SH.MBB^[I-1]);
  end;
  for I := 1 to Length(FunctionID) do
    if not (FunctionID[I-1] = '') then SH.Print('f: '+FunctionID[I-1]);
end;

function SCOrd(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Value: double;
  ValStr: ansistring;
begin
  if Length(ARGUMENTS) < 1 then begin
    SH.SendError(5);
    Exit;
  end;
  Value := Ord(ARGUMENTS[0][0]);
  Str(Trunc(Value), ValStr);
  SCOrd := PChar(ValStr);
end;

function SCRand(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Seed: word;
  SeedReal: double;
  SeedStr: ansistring;
begin
  if Length(ARGUMENTS) < 1 then begin
    Randomize;
    Str(RandSeed, SeedStr);
    SCRand := PChar(SeedStr);
    Exit;
  end;
  if SCIsNaN(SH, ARGUMENTS[0]) = 'TRUE' then begin
    SH.SendError(6);
    Exit;
  end;
  Val(ARGUMENTS[0], SeedReal);
  Seed := Trunc(SeedReal);
  RandSeed := Seed;
  Str(RandSeed, SeedStr);
  SCRand := PChar(SeedStr);
end;

function SCResult(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  I: word;
  Final: ansistring = '';
begin
  for I := 1 to Length(ARGUMENTS) do
    Final += ARGUMENTS[I-1]+' ';
  FunctionResult := Final;
  SCResult := PChar(Final);
end;

function SCSay(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  I: word;
  Final: ansistring = '';
begin
  for I := 1 to Length(ARGUMENTS) do
    Final += ARGUMENTS[I-1]+' ';
  SH.Print(Final);
end;

function SCSet(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  BlockReal: double;
  BlockNum: longword;
  I: word;
  Final: ansistring;
begin
  if Length(ARGUMENTS) < 2 then begin
    SH.SendError(5);
    Exit;
  end;
  if SCIsNaN(SH, ARGUMENTS[0]) = 'TRUE' then begin
    for I := 1 to Length(SH.Parameters^) do
      if UpCase(ARGUMENTS[0]) = UpCase(SH.Parameters^[I-1].ID) then
        SH.Parameters^[I-1].PascalFunction(SH, ARGUMENTS[1]);
    Exit;
  end;
  Val(ARGUMENTS[0], BlockReal);
  BlockNum := Trunc(BlockReal);
  for I := 1 to Length(ARGUMENTS)-1 do
    Final += ARGUMENTS[I]+' ';
  SH.SetMBB(BlockNum, PChar(Final));
end;

function SCWait(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  WaitTime: longword;
begin
  if Length(ARGUMENTS) < 1 then begin
    SH.SendError(5);
    Exit;
  end;
  if SCIsNaN(SH, ARGUMENTS[0]) = 'TRUE' then begin
    SH.SendError(6);
    Exit;
  end;
  Val(ARGUMENTS[0], WaitTime);
  Sleep(WaitTime);
end;

exports
  SCAdd, SCSub, SCMul, SCDiv, SCPow, SCMod, SCEqGr, SCEqLe, SCGr, SCLe, SCEq, SCNotEq,
  SCExit, SCR, SCInput,
  SCACall, SCCall, SCChar, SCEnd, SCError, SCFree, SCFunc, SCGet, SCIf, SCInt, SCIsNaN, SCKey, SCLength, SCMem, SCOrd, SCRand, SCResult, SCSay, SCSet, SCWait;
end.

