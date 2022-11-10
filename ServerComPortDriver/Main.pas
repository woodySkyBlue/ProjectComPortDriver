unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, CPDrv;

type
  TFormMain = class(TForm)
    MemoMessage: TMemo;
    Button1: TButton;
    CommPortDriver1: TCommPortDriver;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CommPortDriver1ReceiveData(Sender: TObject; DataPtr: Pointer; DataSize: Cardinal);
  private
    FStream: TMemoryStream;
    IsWaitCommand: Boolean;
    FFileName: string;
    FFileSize: Int64;
    function ProcIsReceived(F: TStream; var S: string): Boolean;
    procedure ProcMemoMessage(S: string);
    procedure ProcSendLn(S: string);
    procedure ProcWriteString(F: TStream; S: String);
  public
    { Public 宣言 }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses UnitUtils;

const
  MAX_SIZE = 4096;

// *********************************************************************************************************************
{$REGION '// 共用 //'}
// *********************************************************************************************************************

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  FStream := TMemoryStream.Create;
  CommPortDriver1.Port := pnCOM6;
  CommPortDriver1.BaudRate := br256000;
  CommPortDriver1.OutBufSize := MAX_SIZE;
  CommPortDriver1.InBufSize := MAX_SIZE;
  CommPortDriver1.Connect;
  IsWaitCommand := True;
  FFileName := '';
  FFileSize := 0;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  CommPortDriver1.Disconnect;
  FStream.Free;
end;

function TFormMain.ProcIsReceived(F: TStream; var S: string): Boolean;
var
  FByte: Byte;
begin
  Result := False;
  F.Seek(-1, soEnd);
  F.ReadData(FByte);
  if FByte = 10 then begin
    F.Position := 0;
    SetLength(S, (F.Size-1) div 2);
    F.Read(Pointer(S)^, (F.Size-1));
    Result := True;
  end;
end;

procedure TFormMain.ProcMemoMessage(S: string);
begin
  MemoMessage.Lines.Add(FormatDateTime('hh:nn:ss.zzz ', Now) + S);
end;

procedure TFormMain.ProcSendLn(S: string);
begin
  var F := TMemoryStream.Create;
  try
    ProcWriteString(F, S);
    CommPortDriver1.SendData(F.Memory, F.Size);
  finally
    F.Free;
  end;
end;

procedure TFormMain.ProcWriteString(F: TStream; S: String);
begin
  F.Write(Pointer(S)^, Length(S) * SizeOf(Char));
  var Term: Byte := 10;
  F.Write(Term, SizeOf(Term));
end;

// *********************************************************************************************************************
{$ENDREGION}
// *********************************************************************************************************************

procedure TFormMain.CommPortDriver1ReceiveData(Sender: TObject; DataPtr: Pointer; DataSize: Cardinal);
var
  SData: string;
begin
  //ProcMemoMessage(Format('ReceiveData DataSize=%d', [DataSize]));
  FStream.WriteData(DataPtr, DataSize);
  if IsWaitCommand then begin
    // 受信データ：[ファイル名],[ファイルサイズ]<LF>
    // 返信データ：'Command Success'<LF>
    if ProcIsReceived(FStream, SData) then begin
      ProcMemoMessage(Format('ReceiveData = %s', [SData]));
      FFileName := Split(SData, 0);
      FFileSize := StrToIntDef(Split(SData, 1), 0);
      if (FFileName <> '') and (FFileSize > 0) then begin
        ProcSendLn('Command Success');
        FStream.Clear;
        IsWaitCommand := False;
      end;
    end;
  end
  else begin
    // 受信データ：[バイナリデータ]
    // 返信データ：'Data Success'<LF>
    if FStream.Size = FFileSize then begin
      ProcMemoMessage('データ受信完了');
      var S := 'C:\temp\buf\' + FFileName;
      if CheckBeforeSaveFile(S, True) then FStream.SaveToFile(S);
      ProcSendLn('Data Success');
      FStream.Clear;
      IsWaitCommand := True;
    end;
  end;
end;

end.
