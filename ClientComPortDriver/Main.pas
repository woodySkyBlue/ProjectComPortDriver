unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.IOUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  CPDrv;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    ButtonSendData: TButton;
    CheckBoxConnect: TCheckBox;
    CommPortDriver1: TCommPortDriver;
    OpenDialog1: TOpenDialog;
    procedure CheckBoxConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonSendDataClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CommPortDriver1ReceiveData(Sender: TObject; DataPtr: Pointer; DataSize: Cardinal);
    procedure CommPortDriver1ReceivePacket(Sender: TObject; Packet: Pointer; DataSize: Cardinal);
  private
    IsWaitCommand: Boolean;
    FFileName: string;
    FStream: TMemoryStream;
    function ProcIsReceived(F: TStream; var S: string): Boolean;
    procedure ProcMemoMessage(S: string);
    procedure ProcWriteString(F: TStream; S: String);
    procedure ProcSendLn(S: string);
    procedure ProcSendFileData;
  public
    { Public 宣言 }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses UnitUtils;

const
  MAX_SIZE = 4096;

// *********************************************************************************************************************
{$REGION '// 共用 //'}
// *********************************************************************************************************************

procedure TForm1.FormCreate(Sender: TObject);
  begin
    IsWaitCommand := True;
    FFileName := '';
    ReportMemoryLeaksOnShutdown := True;
    FStream := TMemoryStream.Create;
  end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FStream.Free;
end;

function TForm1.ProcIsReceived(F: TStream; var S: string): Boolean;
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

procedure TForm1.ProcMemoMessage(S: string);
begin
  Memo1.Lines.Add(FormatDateTime('hh:nn:ss.zzz ', Now) + S);
end;

procedure TForm1.ProcSendLn(S: string);
begin
  var F := TMemoryStream.Create;
  try
    ProcWriteString(F, S);
    CommPortDriver1.SendData(F.Memory, F.Size);
  finally
    F.Free;
  end;
end;

procedure TForm1.ProcWriteString(F: TStream; S: string);
begin
  F.Write(Pointer(S)^, Length(S) * SizeOf(Char));
  var Term: Byte := 10;
  F.Write(Term, SizeOf(Term));
end;

procedure TForm1.ProcSendFileData;
begin
  var F := TMemoryStream.Create;
  try
    var FBuf := TMemoryStream.Create;
    try
      FBuf.LoadFromFile(FFileName);
      FBuf.Position := 0;
      var FDiv := FBuf.Size div MAX_SIZE;
      var FMod := FBuf.Size mod MAX_SIZE;
      for var Cnt := 0 to FDiv-1 do begin
        F.CopyFrom(FBuf, MAX_SIZE);
        CommPortDriver1.SendData(F.Memory, F.Size);
        F.Clear;
        //Sleep(100);
      end;
      if FMod > 0 then begin
        F.CopyFrom(FBuf, FMod);
        CommPortDriver1.SendData(F.Memory, F.Size)
      end;
    finally
      FBuf.Free;
    end;
  finally
    F.Free;
  end;
end;

procedure TForm1.CheckBoxConnectClick(Sender: TObject);
begin
  if CheckBoxConnect.Checked then begin
    CommPortDriver1.Port := pnCOM5;
    CommPortDriver1.BaudRate := br256000;
    CommPortDriver1.OutBufSize := MAX_SIZE;
    CommPortDriver1.InBufSize := MAX_SIZE;
    CommPortDriver1.Connect;
  end
  else begin
    CommPortDriver1.Disconnect;
  end;
end;

// *********************************************************************************************************************
{$ENDREGION}
// *********************************************************************************************************************

procedure TForm1.ButtonSendDataClick(Sender: TObject);
begin
  if IsWaitCommand then begin
    if OpenDialog1.Execute then begin
      FFileName := OpenDialog1.FileName;
      var F := TMemoryStream.Create;
      try
        F.LoadFromFile(FFileName);
        // 送信データ：[ファイル名],[ファイルサイズ]<LF>
        var S := Format('%s,%d', [TPath.GetFileName(FFileName), F.Size]);
        ProcMemoMessage(Format('SendData = %s', [S]));
        ProcSendLn(S);
        IsWaitCommand := False;
      finally
        F.Free;
      end;
    end;
  end
  else begin
    ProcMemoMessage('[Error] データ送信中');
  end;
end;

procedure TForm1.CommPortDriver1ReceiveData(Sender: TObject; DataPtr: Pointer; DataSize: Cardinal);
var
  SData: string;
begin
  //Memo1.Lines.Add(Format('ReceiveData DataSize=%d', [DataSize]));
  FStream.WriteData(DataPtr, DataSize);
  if ProcIsReceived(FStream, SData) then begin
    ProcMemoMessage(Format('ReceiveData = %s', [SData]));
    if SData = 'Command Success' then begin
      ProcSendFileData;
    end
    else if SData = 'Data Success' then begin
      IsWaitCommand := True;
    end;
    FStream.Clear;
  end;
end;

procedure TForm1.CommPortDriver1ReceivePacket(Sender: TObject; Packet: Pointer; DataSize: Cardinal);
begin
  Memo1.Lines.Add(Format('ReceivePacket DataSize=%d', [DataSize]));
end;

end.
