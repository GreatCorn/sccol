library SCFiles;
uses
  SharedObject, SysUtils;

var
  DIR: ansistring;

         { PARAMETERS }

function SCDir(var SH: TSharedObject; VALUE: pchar):pchar;
begin
  if VALUE <> nil then
    if DirectoryExists(VALUE) then
      DIR := VALUE
    else
      SH.SendError(20)
  else
    SCDir := PChar(DIR);
end;

         { COMMANDS }

function SCFile(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
begin

end;

function SCUp(var SH: TSharedObject; ARGUMENTS: array of pchar):pchar;
var
  I: word;
begin
  if DIR[Length(DIR)] <> ':' then
    for I := Length(DIR) downto 1 do
      if DIR[I] = DirectorySeparator then begin
        Delete(DIR, I, Length(DIR)-I);
        Break;
      end;
end;

exports
  SCDir,
  SCFile, SCUp;

begin
  DIR := ParamStr(0);
end.

