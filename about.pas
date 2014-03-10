unit about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TAboutFrm }

  TAboutFrm = class(TForm)
    Image1: TImage;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutFrm: TAboutFrm;

implementation

{$R *.lfm}

end.

