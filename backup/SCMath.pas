library SCMath;
uses
  SharedObject, SysUtils;

var
  DeclareID: ansistring;
  Functions: TStringArray;
  FunctionID: TStringArray;
  FunctionResult: ansistring;

function SCIsNaN(var ARGUMENTS: array of pchar):boolean;
var
  Bool: boolean;
  Argument: pchar;
  I: Word;
begin
  for Argument in ARGUMENTS do begin
    if (Argument = ' ') or (Argument = '') then begin
      SCIsNaN := TRUE;
      Exit;
    end;
    for I := 0 to Length(Argument)-1 do begin
      Bool := Argument[I] in ['0'..'9','-','.',' '];
      if not Bool then begin
        SCIsNaN := TRUE;
        Exit;
      end;
    end;
    SCIsNaN := FALSE;
  end;
end;
function CorrectUsage(var SH: TSharedObject; var ARGUMENTS: array of pchar; NUM: byte):boolean;
begin
  CorrectUsage := false;
  if Length(ARGUMENTS) < NUM then begin
    SH.SendError(5);
    Exit;
  end;
  if SCIsNaN(ARGUMENTS) then begin
    SH.SendError(6);
    Exit;
  end;
  CorrectUsage := true;
end;

         { COMMANDS }

function SCAbs(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Value: double;
begin
  if not CorrectUsage(SH, ARGUMENTS, 1) then Exit;
  Val(ARGUMENTS[0], Value);
  SCAbs := PChar(StringReplace(FloatToStr(Abs(Value)), ',', '.', [rfReplaceAll]));
end;

function SCCeil(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Value: double;
  ValStr: ansistring;
begin
  if not CorrectUsage(SH, ARGUMENTS, 1) then Exit;
  Val(ARGUMENTS[0], Value);
  if Value > Trunc(Value) then Value += 1;
  Str(Trunc(Value), ValStr);
  SCCeil := PChar(ValStr);
end;

function SCClamp(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Value, Minimum, Maximum: double;
begin
  if not CorrectUsage(SH, ARGUMENTS, 3) then Exit;
  Val(ARGUMENTS[0], Value); Val(ARGUMENTS[1], Minimum); Val(ARGUMENTS[2], Maximum);
  if Value < Minimum then SCClamp := ARGUMENTS[1]
  else if Value > Maximum then SCClamp := ARGUMENTS[2]
  else SCClamp := ARGUMENTS[0];
end;

function SCCos(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Value: double;
begin
  if not CorrectUsage(SH, ARGUMENTS, 1) then Exit;
  Val(ARGUMENTS[0], Value);
  SCCos := PChar(StringReplace(FloatToStr(Cos(Value)), ',', '.', [rfReplaceAll]));
end;

function SCExp(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Value: double;
begin
  if not CorrectUsage(SH, ARGUMENTS, 1) then Exit;
  Val(ARGUMENTS[0], Value);
  SCExp := PChar(StringReplace(FloatToStr(Exp(Value)), ',', '.', [rfReplaceAll]));
end;

function SCFloor(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Value: double;
  ValStr: ansistring;
begin
  if not CorrectUsage(SH, ARGUMENTS, 1) then Exit;
  Val(ARGUMENTS[0], Value);
  if Value < Trunc(Value) then Value -= 1;
  Str(Trunc(Value), ValStr);
  SCFloor := PChar(ValStr);
end;

function SCFrac(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Value: double;
begin
  if not CorrectUsage(SH, ARGUMENTS, 1) then Exit;
  Val(ARGUMENTS[0], Value);
  SCFrac := PChar(StringReplace(FloatToStr(Frac(Value)), ',', '.', [rfReplaceAll]));
end;

function SCLerp(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  A, B, T: double;
begin
  if not CorrectUsage(SH, ARGUMENTS, 3) then Exit;
  Val(ARGUMENTS[0], A); Val(ARGUMENTS[1], B); Val(ARGUMENTS[2], T);
  SCLerp := PChar(StringReplace(FloatToStr(A + (B - A) * T), ',', '.', [rfReplaceAll]));
end;

function SCLog(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Value: double;
begin
  if not CorrectUsage(SH, ARGUMENTS, 1) then Exit;
  Val(ARGUMENTS[0], Value);
  SCLog := PChar(StringReplace(FloatToStr(Ln(Value)), ',', '.', [rfReplaceAll]));
end;

function SCOdd(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Value: double;
begin
  if not CorrectUsage(SH, ARGUMENTS, 1) then Exit;
  Val(ARGUMENTS[0], Value);
  if Odd(Trunc(Value)) then SCOdd := 'TRUE' else SCOdd := 'FALSE';
end;

function SCRandomRange(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Minimum, Maximum: double;
begin
  if not CorrectUsage(SH, ARGUMENTS, 2) then Exit;
  Val(ARGUMENTS[0], Minimum); Val(ARGUMENTS[1], Maximum);
  SCRandomRange := PChar(StringReplace(FloatToStr(Random*(Maximum - Minimum)+Minimum), ',', '.', [rfReplaceAll]));
end;

function SCRound(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Value: double;
begin
  if not CorrectUsage(SH, ARGUMENTS, 1) then Exit;
  Val(ARGUMENTS[0], Value);
  SCRound := PChar(StringReplace(FloatToStr(Round(Value)), ',', '.', [rfReplaceAll]));
end;

function SCSin(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Value: double;
begin
  if not CorrectUsage(SH, ARGUMENTS, 1) then Exit;
  Val(ARGUMENTS[0], Value);
  SCSin := PChar(StringReplace(FloatToStr(Sin(Value)), ',', '.', [rfReplaceAll]));
end;

function SCTan(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Value: double;
begin
  if not CorrectUsage(SH, ARGUMENTS, 1) then Exit;
  Val(ARGUMENTS[0], Value);
  SCTan := PChar(StringReplace(FloatToStr(Sin(Value)/Cos(Value)), ',', '.', [rfReplaceAll]));
end;

exports
  SCAbs, SCCeil, SCClamp, SCCos, SCExp, SCFloor, SCFrac, SCLerp, SCLog, SCOdd, SCRandomRange, SCRound, SCSin, SCTan;
end.

