unit SharedObject;

interface

type
  PAsync = ^TAsync;
  PParameters = ^TParameters;
  PSharedObject = ^TSharedObject;
  PStringArray = ^TStringArray;

  TStringArray = array of ansistring;
  TMap = record
    ID: ansistring;
    Value: ansistring;
  end;
  TMapArray = array of TMap;
  TAsync = record
    Code: ansistring;
    Arguments: array of pchar;
  end;

  FAsyncEvent = procedure(const ASYNC: TAsync) of object;
  FFuncEvent = procedure(const INSTR: ansistring; const ARGUMENTS: array of pchar) of object;
  FGetEvent = procedure(const KEY: boolean) of object;
  FPrintEvent = procedure(const INSTR: ansistring) of object;
  FSendErrorEvent = procedure(const CODE: word) of object;
  FSetMBBEvent = procedure(const NUM: longword; const VALUE: pchar) of object;
  TSharedObject = class
    public
      CodeLog: pansistring;
      Compile: FPrintEvent;
      CompileAsync: FAsyncEvent;
      CompileFunc: FFuncEvent;
      Get: FGetEvent;
      MBB: PStringArray;
      LogCode: pboolean;
      Parameters: PParameters;
      Print: FPrintEvent;
      SCCOLInput: pansistring;
      SendError: FSendErrorEvent;
      SetMBB: FSetMBBEvent;

              {PARAMETERS}
      ExitOnFinish: pboolean;
      ShowInput: pboolean;
  end;
  TParamFunc = function(var SH: TSharedObject; VALUE: pchar):pchar;
  TParameter = record
    ID: ansistring;
    PascalFunction: TParamFunc;
  end;
  TParameters = array of TParameter;

implementation

end.
