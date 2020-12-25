library SCFiles;
uses
  SharedObject, SysUtils;

var
  DIR, WORKDIR, SCCOLDIR: ansistring;

         { PARAMETERS }

function SCPDir(var SH: TSharedObject; VALUE: pchar):pchar;
begin
  if VALUE <> nil then begin
    if (VALUE = '.') or (VALUE = '..') then begin
      SH.SendError(20);
      Exit;
    end;
    if DirectoryExists(VALUE) then
      DIR := VALUE
    else
      SH.SendError(20)
  end
  else
    SCPDir := PChar(DIR);
end;
function SCWorkDir(var SH: TSharedObject; VALUE: pchar):pchar;
begin
  if VALUE <> nil then begin
    if (VALUE = '.') or (VALUE = '..') then begin
      SH.SendError(20);
      Exit;
    end;
    if DirectoryExists(VALUE) then
      WORKDIR := VALUE
    else
      SH.SendError(20)
  end
  else
    SCWorkDir := PChar(WORKDIR);
end;
function SCSCCOLDir(var SH: TSharedObject; VALUE: pchar):pchar;
begin
  if VALUE <> nil then begin
    if (VALUE = '.') or (VALUE = '..') then begin
      SH.SendError(20);
      Exit;
    end;
    if DirectoryExists(VALUE) then
      SCCOLDIR := VALUE
    else
      SH.SendError(20)
  end
  else
    SCSCCOLDir := PChar(SCCOLDIR);
end;

         { COMMANDS }

function SCDir(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Final: ansistring;
  I: word;
begin
  for I := 1 to Length(ARGUMENTS) do
    Final += ARGUMENTS[I-1]+' ';
  SCDir := SCPDir(SH, PChar(Final));
  if Length(SCDir) > 0 then SH.Print(SCDir);
end;

function SCFile(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  OpenFile: TextFile;
  FileString, OutString: ansistring;
begin
  if Length(ARGUMENTS) < 1 then begin
    SH.SendError(5);
    Exit;
  end;
  if not FileExists(ARGUMENTS[0]) then begin
    SH.SendError(19);
    Exit;
  end;
  Assign(OpenFile, ARGUMENTS[0]);
  OutString := '';
  try
    Reset(OpenFile);
    while not EOF(OpenFile) do begin
      ReadLn(OpenFile, FileString);
      OutString += FileString+sLineBreak;
    end;
  finally
    Close(OpenFile);
    Delete(OutString, Length(OutString)-Length(sLineBreak)+1, Length(sLineBreak));
  end;
  SCFile := PChar(OutString);
end;

function SCList(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  Found: boolean = false;
  Directory, ArgStr: ansistring;
  Search: TSearchRec;
begin
  if Length(ARGUMENTS) < 1 then
    Directory := DIR
  else begin
    if not DirectoryExists(ARGUMENTS[0]) then begin
      SH.SendError(20);
      Exit;
    end;
    Directory := ARGUMENTS[0];
  end;
  SH.Print(Directory+' contents:');
  if FindFirst(Directory+'*', faDirectory, Search) = 0 then
    repeat
      with Search do begin
        if (Name = '.') or (Name = '..') or ((Attr and faDirectory) <> faDirectory) then Continue;
        Found := true;
        ArgStr := '';
        if (Attr and faReadOnly) = faReadOnly then ArgStr += 'R, ';
        if (Attr and $00000002) = $00000002 then ArgStr += 'H, '; //faHidden
        Delete(ArgStr, Length(ArgStr)-1, 2);
        if ArgStr <> '' then SH.Print(' [D; '+ArgStr+'] '+Name) else SH.Print(' [D] '+Name);
      end;
    until FindNext(Search)<>0;
  FindClose(Search);
  if not Found then SH.Print('Contains no subdirectories.');
  SH.Print('');
  if FindFirst(Directory+'*', faAnyFile, Search) = 0 then
    repeat
      with Search do begin
        if (Name = '.') or (Name = '..') or ((Attr and faDirectory) = faDirectory) then Continue;
        Found := true;
        ArgStr := '';
        if (Attr and faReadOnly) = faReadOnly then ArgStr += 'R, ';
        if (Attr and $00000002) = $00000002 then ArgStr += 'H, '; //faHidden
        Delete(ArgStr, Length(ArgStr)-1, 2);
        if ArgStr <> '' then SH.Print(' [F; '+ArgStr+'] '+Name) else SH.Print(' [F] '+Name);
      end;
    until FindNext(Search)<>0;
  FindClose(Search);
  if not Found then SH.Print('Contains no files.');
end;

function SCUp(var SH: TSharedObject):pchar;
var
  I: word;
begin
  if (DIR[Length(DIR)-1] <> ':') and (Pos(DirectorySeparator, DIR) <> 0) then
    for I := Length(DIR)-1 downto 1 do
      if DIR[I] = DirectorySeparator then begin
        Delete(DIR, I, Length(DIR)-I);
        Break;
      end;
end;

exports
  SCPDir, SCSCCOLDir, SCWorkDir,
  SCDir, SCFile, SCList, SCUp;

begin
  DIR := ExtractFilePath(ParamStr(0));
  WORKDIR := ExtractFilePath(ParamStr(1));
  if not DirectoryExists(WORKDIR) then WORKDIR := DIR;
  SCCOLDIR := DIR;
end.

