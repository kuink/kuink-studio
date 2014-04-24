unit ide;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynHighlighterXML, SynEdit, SynEditTypes,
  SynCompletion, Forms, Controls, Graphics, Dialogs, ComCtrls, ShellCtrls,
  ExtCtrls, Menus, laz2_XMLRead, laz2_DOM, about, XMLConf, LCLType,
  DefaultTranslator, PairSplitter, Identifiers;

const
  FileFound = 0;

type
  TKuinkNodeType = (KuinkApplication, KuinkProcess, KuinkNode);

  TKuinkNodeData = class
     Text : string;
  end;

  TKuinkEditorTab = class(TTabSheet)
  private
    PFilename: string;
    PChanged: boolean; //This source has changed?
    PCode: boolean; //In CODE(true) view or DESIGN(false) view?
    PNodeType: TKuinkNodeType; //Type of this node
    PXmlNode: TXMLDocument; //Stores the last saved xml node
  published
    property Filename: string read PFilename write PFilename;
    property Changed: boolean read PChanged write PChanged;
    property Code: boolean read PCode write PCode;
    property NodeType: TKuinkNodeType read PNodeType write PNodeType;
    property XmlNode: TXMLDocument read PXmlNode write PXmlNode;
  end;

  { TIDEFrm }

  TIDEFrm = class(TForm)
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    FileSave: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    FileOpen: TMenuItem;
    FileCloseAll: TMenuItem;
    APIPopupMenu: TPopupMenu;
    APIPopupAdd: TMenuItem;
    APIPopupMenuRemove: TMenuItem;
    MenuEdit: TMenuItem;
    MenuEditReplace: TMenuItem;
    MenuFileExit: TMenuItem;
    APIPopupMenuRefresh: TMenuItem;
    MenuEditFindNext: TMenuItem;
    FileExplorerPopupMenu: TPopupMenu;
    FEPopupNew: TMenuItem;
    FEPopupNewXmlFile: TMenuItem;
    FileNew: TMenuItem;
    FileNewXmlFile: TMenuItem;
    FEPopupRefresh: TMenuItem;
    MenuViewDesign: TMenuItem;
    MenuViewCode: TMenuItem;
    MenuView: TMenuItem;
    ReplaceDialog1: TReplaceDialog;
    SaveAll: TMenuItem;
    PageControl1: TPageControl;
    SaveDialog1: TSaveDialog;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    ShellTreeView1: TShellTreeView;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    SynAutoComplete1: TSynAutoComplete;
    SynCompletion1: TSynCompletion;
    SynXMLSyn1: TSynXMLSyn;
    APIInspector: TTreeView;
    NodeInspector: TTreeView;
    XMLConfig1: TXMLConfig;
    procedure APIPopupAddClick(Sender: TObject);
    procedure APIPopupMenuRefreshClick(Sender: TObject);
    procedure APIPopupMenuRemoveClick(Sender: TObject);
    procedure FEPopupNewXmlFileClick(Sender: TObject);
    procedure FEPopupRefreshClick(Sender: TObject);
    procedure FileNewXmlFileClick(Sender: TObject);
    procedure FileOpenClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    function GetKuinkFunctionSignature(AppName, ProcessName, FunctionName:
      string): string;
    procedure MenuEditFindNextClick(Sender: TObject);
    procedure MenuEditReplaceClick(Sender: TObject);
    procedure MenuFileExitClick(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure LoadAPIInspector();
    procedure NodeInspectorDblClick(Sender: TObject);
    procedure ShellTreeView1Changing(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure StoreAPIInspector();
    procedure PopulateAPIInspector(Tree: TTreeView; BaseApplication: string);
    procedure PopulateInspectorAddElement(XMLDoc: TXMLDocument; tagContainer: string; curTag: string; curAttr: string);
    procedure PopulateNodeInspector(TabSheet: TKuinkEditorTab);
    procedure FileMenuClick(Sender: TObject);
    procedure FileSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure EditorFindNext();
    procedure ReplaceDialog1Find(Sender: TObject);
    procedure ReplaceDialog1Replace(Sender: TObject);
    procedure SaveAllClick(Sender: TObject);
    procedure ShellTreeView1SelectionChanged(Sender: TObject);
    procedure ClosePage(Sender: TObject);
    procedure PageChanged(Sender: TObject);
    procedure EditorDragOver(Sender, Source: TObject; X, Y: integer;
      State: TDragState; var Accept: boolean);
    procedure EditorDragDrop(Sender, Source: TObject; X, Y: integer);
    procedure EditorAutoComplete(Sender: TObject; var Key: char);
    procedure EditorEnter(Sender: TObject);
    procedure SaveNode(TabSheet: TKuinkEditorTab);
    procedure SaveAllNodes(Ask: Boolean);
    procedure OpenNode(FileName: String);

    function KuinkTypeToStr(KuinkType: TKuinkNodeType): string;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  IDEFrm: TIDEFrm;

implementation

{$R *.lfm}

{ TIDEFrm }

procedure TIDEFrm.LoadAPIInspector();
Var OpenedAPI, CurrentAPI: string;
  i: integer;
begin
  //API's -> Load APIInspector with previous ones
  OpenedAPI := XmlConfig1.GetValue(CONFIG_APIINSPECTOR, '');
  CurrentAPI := '';
  for i := 1 to length(OpenedAPI) do
    if ((OpenedAPI[i] = ',') or (i = length(OpenedAPI))) then
    begin
       if (i = length(OpenedAPI)) then
          CurrentAPI :=  CurrentAPI + OpenedAPI[i];
       PopulateAPIInspector(APIInspector, ShellTreeView1.Root + '/' + CurrentAPI);
       CurrentAPI := '';
    end
    else
        CurrentAPI := CurrentAPI + OpenedAPI[i];
end;

procedure TIDEFrm.NodeInspectorDblClick(Sender: TObject);
var
  Selected: TTreeNode;
  TabSheet: TKuinkEditorTab;
  Editor: TSynEdit;
  Opt : TSynSearchOptions;
begin
  Selected := NodeInspector.Selected;

  TabSheet := TKuinkEditorTab(PageControl1.ActivePage);
  Editor := TSynEdit(tabSheet.Controls[0]);
  Opt := [];
  Include(Opt, ssoEntireScope);
  Editor.SearchReplace(TKuinkNodeData(Selected.Data).Text, '', Opt);
end;

procedure TIDEFrm.ShellTreeView1Changing(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
//  ShowMessage(Node.Text);
end;

procedure TIDEFrm.StoreAPIInspector();
  Var OpenedAPI, OpenedFiles: string;
    i, Max: integer;
    CurrentNode: TTreeNode;
  begin
    //Store the API's to inspect
    OpenedAPI := '';
    CurrentNode := APIInspector.Items.GetFirstNode;
    while (CurrentNode <> nil) do
    begin
         if (OpenedAPI <> '') Then
           OpenedAPI := OpenedAPI + ',' + CurrentNode.Text
         else
           OpenedAPI := CurrentNode.Text;

         CurrentNode := CurrentNode.GetNextSibling;
    end;
    XmlConfig1.SetValue (CONFIG_APIINSPECTOR, OpenedAPI);
    //Store the opened files
end;

procedure TIDEFrm.PopulateAPIInspector(Tree: TTreeView; BaseApplication: string);
var
  XMLDoc: TXMLDocument;
  iNode, iAttr: TDOMNode;
  SearchResult: TSearchRec;
  FileName, FunctionName: string;
  TreeNode, BaseTreeNode: TTreeNode;

begin
  if FindFirst(BaseApplication +
    '/process/*', (faDirectory), SearchResult) = FileFound then
  begin
    BaseTreeNode := Tree.Items.AddChild(nil, ExtractFileName(BaseApplication));
    repeat
      if (SearchResult.Name <> '.') and (SearchResult.Name <> '..') then
      begin
        FileName := BaseApplication +
          '/process/' + SearchResult.Name + '/lib/' + SearchResult.Name + '_api.xml';
        if FileExists(FileName) then
        begin
          ReadXMLFile(XMLDoc, FileName);
          iNode := XMLDoc.DocumentElement.FindNode('Library');
          TreeNode := Tree.Items.AddChild(BaseTreeNode, SearchResult.Name);
          if iNode <> nil then
          begin
            //Process Function names
            iNode := iNode.FirstChild;
            while iNode <> nil do
            begin
              if iNode.HasAttributes and (iNode.Attributes.Length > 0) then
              begin
                FunctionName := iNode.Attributes.GetNamedItem('name').NodeValue;
                //iNode.Attributes[0].NodeValue;
                Tree.Items.AddChild(TreeNode, FunctionName);
              end;
              iNode := iNode.NextSibling;
            end;
          end;
        end;
      end;
    until (FindNext(SearchResult) <> FileFound);
  end;
  XMLDoc.Free;
  FindClose(SearchResult);
  Tree.AlphaSort; //Sort the tree Alphabetical order
end;

procedure TIDEFrm.PopulateInspectorAddElement(XMLDoc: TXMLDocument; tagContainer: string; curTag: string; curAttr: string);
var
   TreeNode, NewNode: TTreeNode;
   iNode, iAttr: TDOMNode;
   AttrName: String;
   i: integer;
   s: string;
begin
  iNode := XMLDoc.DocumentElement.FindNode(tagContainer);
  TreeNode := NodeInspector.Items.AddChild(nil, curTag);
  if iNode <> nil then
  begin
    //Process Screen names
    iNode := iNode.FirstChild;
    while iNode <> nil do
    begin
      s := '';
      if iNode.HasAttributes and (iNode.Attributes.Length > 0) then
      begin
        for i := 0 to iNode.Attributes.Length-1 do
        begin
          s := s + ' ' + string(iNode.Attributes.Item[i].NodeName) +'="' + string(iNode.Attributes.Item[i].NodeValue + '"');
        end;
        iAttr := iNode.Attributes.GetNamedItem(curAttr);
        if iAttr <> nil then
        begin
             AttrName :=  iAttr.NodeValue;
             NewNode := NodeInspector.Items.AddChild(TreeNode, AttrName);
             NewNode.Data := TKuinkNodeData.Create();
             TKuinkNodeData(NewNode.Data).Text := '<'+curTag+s;
        end;
      end;
      iNode := iNode.NextSibling;
    end;
  end;
end;

procedure TIDEFrm.PopulateNodeInspector(TabSheet: TKuinkEditorTab);
begin
  NodeInspector.Items.Clear;
  //TabSheet := TKuinkEditorTab(PageControl1.ActivePage);
  PopulateInspectorAddElement(TabSheet.XmlNode, 'Screens', 'Screen', 'id');
  PopulateInspectorAddElement(TabSheet.XmlNode, 'Actions', 'Action','name');
  PopulateInspectorAddElement(TabSheet.XmlNode, 'Library', 'Function','name');
end;

function TIDEFrm.GetKuinkFunctionSignature(AppName, ProcessName,
  FunctionName: string): string;
var
  FxSignature: string;
  XMLDoc: TXMLDocument;
  iNode, iParamNode, iAttr: TDOMNode;
  SearchResult: TSearchRec;
  FileName, FxName, ParamName, ParamRequired, RootDir: string;
  TreeNode, BaseTreeNode: TTreeNode;
begin
  RootDir := ShellTreeView1.Root;
  FileName := RootDir + '/' + AppName + '/process/' +
    ProcessName + '/lib/' + ProcessName + '_api.xml';
  if FileExists(FileName) then
  begin
    ReadXMLFile(XMLDoc, FileName);
    iNode := XMLDoc.DocumentElement.FindNode('Library');
    if iNode <> nil then
    begin
      FxSignature := '<Call library="' + AppName + ',' + ProcessName +
        ',' + 'api" function="' + FunctionName + '">' + #13 + #10;
      //Process Function names
      iNode := iNode.FirstChild;
      while iNode <> nil do
      begin
        if iNode.HasAttributes and (iNode.Attributes.Length > 0) then
        begin
          FxName := iNode.Attributes.GetNamedItem('name').NodeValue;
          if FxName = FunctionName then
          begin
            iParamNode := iNode.FindNode('Params');
            if iParamNode <> nil then
            begin
              iParamNode := iParamNode.FirstChild;
              while iParamNode <> nil do
              begin
                //Process Params
                if (iParamNode.HasAttributes) and (iParamNode.Attributes.Length > 0) then
                begin
                  ParamName := iParamNode.Attributes.GetNamedItem('name').NodeValue;
                  ParamRequired := '';
                  if (iParamNode.Attributes.GetNamedItem('required') <> nil) then
                     if (iParamNode.Attributes.GetNamedItem('required').NodeValue = 'false') then
                        ParamRequired := '<!-- Optional -->';
                  FxSignature :=
                    FxSignature + '  <Param name="' + ParamName + '"></Param>' + ParamRequired + #13 + #10;
                end;
                iParamNode := iParamNode.NextSibling;
              end;
              break;
            end;
          end;
        end;
        iNode := iNode.NextSibling;
      end;
    end;
  end
  else
      ShowMessage('File Not Found: ' + FileName);

  FxSignature := FxSignature + '</Call>' + #13 + #10;
  GetKuinkFunctionSignature := FxSignature;
end;

procedure TIDEFrm.MenuEditFindNextClick(Sender: TObject);
begin
  EditorFindNext();
end;

procedure TIDEFrm.MenuEditReplaceClick(Sender: TObject);
begin
  ReplaceDialog1.Execute;
end;

procedure TIDEFrm.MenuFileExitClick(Sender: TObject);
begin
  Halt;
end;

procedure TIDEFrm.MenuItem2Click(Sender: TObject);
begin
  AboutFrm.ShowModal;
end;

procedure TIDEFrm.FormActivate(Sender: TObject);
begin
end;

procedure TIDEFrm.FormCreate(Sender: TObject);
Var OpenedAPI, CurrentAPI: string;
  i: integer;
begin
  //Load the default data:
  SynAutoComplete1.AutoCompleteList.LoadFromFile('autoComplete.txt');
  //Root dir
  ShellTreeView1.Root := XmlConfig1.GetValue(CONFIG_ROOTDIR, CONFIG_ROOTDIR_DEFAULT);

  //API's -> Load APIInspector with previous ones
  LoadAPIInspector();
end;

procedure TIDEFrm.FileOpenClick(Sender: TObject);
begin
  SelectDirectoryDialog1.InitialDir := '.';
  if SelectDirectoryDialog1.Execute then
  begin
    //Select the root dir ansassign it to the treeview as well as store it in the config file
    ShellTreeView1.Root := SelectDirectoryDialog1.FileName;
    XmlConfig1.SetValue (CONFIG_ROOTDIR, SelectDirectoryDialog1.FileName);
  end;

end;

procedure TIDEFrm.APIPopupAddClick(Sender: TObject);
Var cfgValue: string;
begin
  //Defaults to the rrot of the project
  SelectDirectoryDialog1.InitialDir := ShellTreeView1.Root;
  if SelectDirectoryDialog1.Execute then
  begin
    //Select the root dir ansassign it to the treeview as well as store it in the config file
    PopulateAPIInspector(APIInspector, SelectDirectoryDialog1.FileName);
    //XmlConfig1.SetValue (CONFIG_ROOTDIR, SelectDirectoryDialog1.FileName);
  end;
end;

procedure TIDEFrm.APIPopupMenuRefreshClick(Sender: TObject);
Var Api: String;
begin
  StoreAPIInspector();
  APIInspector.Items.Clear;
  LoadAPIInspector();
end;

procedure TIDEFrm.APIPopupMenuRemoveClick(Sender: TObject);
begin
  If (APIInspector.Selected <> nil) Then
    APIInspector.Items.Delete(APIInspector.Selected);
end;

procedure TIDEFrm.FEPopupNewXmlFileClick(Sender: TObject);
begin
  OpenNode('untitled');
end;

procedure TIDEFrm.FEPopupRefreshClick(Sender: TObject);
begin
  ShellTreeView1.Refresh;
end;

procedure TIDEFrm.FileNewXmlFileClick(Sender: TObject);
begin
  OpenNode('untitled');
end;

function TIDEFrm.KuinkTypeToStr(KuinkType: TKuinkNodeType): string;
var
  KuinkTypeStr: string;
begin
  case KuinkType of
    KuinkApplication: KuinkTypeStr := 'Application';
    KuinkProcess: KuinkTypeStr := 'Process';
    KuinkNode: KuinkTypeStr := 'Node';
  end;
  KuinkTypeToStr := KuinkTypeStr;
end;

procedure TIDEFrm.EditorDragOver(Sender, Source: TObject; X, Y: integer;
  State: TDragState; var Accept: boolean);
begin
  Accept := True;
end;

Procedure AuxGetTagInfoAtLine(Line: String; X: Integer; var XmlCurrentTag: String; var XmlCurrentAttribute: String; Var InsideQuot: Boolean);
 Var I: Integer;
 Var XmlTag, CurrentLine: string;
 Var InsideTag: Boolean;
 Var InsideAttr: Boolean;
Begin
  XmlTag := '';
  InsideTag := False;
  InsideAttr := False;
  InsideQuot := False;
  For I := 1 To X-1 Do
  Begin
    If (Line[I] = '"') Then
       InsideQuot := Not InsideQuot;

    If (Line[I] = '<') Then
    Begin
       InsideTag := True;
       XmlTag := '';
    End
    Else If (Line[I] = ' ') Or (Line[I] = '/') then
    Begin
       InsideTag := False;
       If (Line[I] = '/') Then
          XmlTag := '';
    End;
    If InsideTag And (Line[I] <> '<') Then
       XmlTag := XmlTag + Line[I];
    //ShowMessage(XmlTag + IntToStr(I));
  End;
  XmlCurrentTag := XmlTag;
End;

Procedure TIDEFrm.EditorAutoComplete(Sender: TObject; var Key: char);
Var Editor: TSynEdit;
Var CurrentPosition: TPoint;
Var XmlTag, XmlAttribute: string;
Var InsideQuot: Boolean;
Begin
  Editor := TSynEdit(Sender);
  CurrentPosition := Editor.CaretXY;
  Case key Of
  '>':
  Begin
    AuxGetTagInfoAtLine(Editor.LineText, Editor.CaretX, XmlTag, XmlAttribute, InsideQuot);
    //ShowMessage(XmlTag);
    If (XmlTag <> '') And Not InsideQuot Then
    Begin
     Editor.InsertTextAtCaret('</'+XmlTag+'>');
     Editor.CaretXY := CurrentPosition;
    End;
  End;

  '"': Begin
    If (Editor.LineText[Editor.CaretX]='"') Then
    Begin
       Editor.CaretX := Editor.CaretX+1;
       key := ' ';
    End;
    If (Editor.LineText[Editor.CaretX-1] = '=') Then
    Begin
         Editor.InsertTextAtCaret('"');
         Editor.CaretXY := CurrentPosition;
    End;
    End;
  End;

End;

procedure TIDEFrm.EditorEnter(Sender: TObject);
var Editor: TSynEdit;
  TabSheet: TKuinkEditorTab;
begin
     Editor := TSynEdit(Sender);
     SynAutoComplete1.Editor := Editor;
     SynCompletion1.Editor := Editor;
     TabSheet := TKuinkEditorTab(Editor.Parent);
     PopulateNodeInspector(TabSheet);
end;

procedure TIDEFrm.EditorDragDrop(Sender, Source: TObject; X, Y: integer);
var
  Tree: TTreeView;
  AppNode, ProcessNode, FxNode: TTreeNode;
  Editor: TSynEdit;
  InsertText: string;
begin
  if Source is TTreeView then
  begin
    Tree := TTreeView(Source);
    fxNode := Tree.Selected;
    ProcessNode := fxNode.Parent;
    AppNode := ProcessNode.Parent;
    InsertText := GetKuinkFunctionSignature(AppNode.Text, ProcessNode.Text, FxNode.Text);
  end;

  Editor := TSynEdit(Sender);
  //  Editor.CaretX:=X;
  //  Editor.CaretY := Y;
  Editor.InsertTextAtCaret(InsertText);

end;

procedure TIDEFrm.ClosePage(Sender: TObject);
var
  TabSheet: TKuinkEditorTab;
  PageNum: integer;
  Reply, BoxStyle: integer;
  Message: String;
begin
  TabSheet := Sender as TKuinkEditorTab;
  PageNum := TabSheet.PageIndex;

  //Check if the page has changed
  if (TabSheet.Changed) then
  begin
    BoxStyle := MB_ICONQUESTION + MB_YESNOCANCEL;
    Message := 'Save ' + ExtractFileName(Tabsheet.Filename)+ '?';
    Reply := Application.MessageBox(PChar(Message), 'Content Changed', BoxStyle);
    If Reply = IDYES Then
      SaveNode(TabSheet);
  end;
  if Reply <> IDCANCEL then
    PageControl1.Page[PageNum].Destroy;

end;

procedure TIDEFrm.PageChanged(Sender: TObject);
var
  Editor: TSynEdit;
  TabSheet: TKuinkEditorTab;
begin
  Editor := TSynEdit(Sender);
  TabSheet := TKuinkEditorTab(Editor.Parent);

  if Not TabSheet.Changed then
  begin
    TabSheet.Changed := True;
    TabSheet.Caption := TabSheet.Caption + '*';
  end;
end;

procedure TIDEFrm.ShellTreeView1SelectionChanged(Sender: TObject);
var
  TabSheet: TTabSheet;
  OpenedTab: TKuinkEditorTab;
  Editor: TSynEdit;
  OnExitEventHandler: TNotifyEvent;
  i: integer;
  Found, IsDirectory: boolean;
  NodeName: string;
  NodeType: TKuinkNodeType;
begin
  //Check to see if it is allready open
  //If so, activate it
  //Else, create and activate it
  Found := False;
  for i := 0 to PageControl1.PageCount - 1 do
  begin
    OpenedTab := TKuinkEditorTab(PageControl1.Pages[i]);
    if OpenedTab.Filename = ShellTreeView1.GetSelectedNodePath() then
    begin
      Found := True;
      PageControl1.ActivePageIndex := i;
      StatusBar1.SimpleText := 'NodeType: ' + KuinkTypeToStr(OpenedTab.NodeType);
    end;
  end;

  //Check to see if it is a directory
  IsDirectory := DirectoryExists(ShellTreeView1.GetSelectedNodePath());

  //If it is a directory, dont create the TabSheet
  if (not Found) and (not isDirectory) then
  begin
       OpenNode(ShellTreeView1.GetSelectedNodePath());
  end;
end;

procedure TIDEFrm.FileMenuClick(Sender: TObject);
begin
end;

procedure TIDEFrm.FileSaveClick(Sender: TObject);
var
  TabSheet: TKuinkEditorTab;
begin
  TabSheet := TKuinkEditorTab(PageControl1.ActivePage);

  SaveNode(TabSheet);
end;

procedure TIDEFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //Store the opened files
  StoreAPIInspector();
  SaveAllNodes(True);
end;

procedure TIDEFrm.EditorFindNext();
  var
    TabSheet: TKuinkEditorTab;
    Editor: TSynEdit;
    Opt : TSynSearchOptions;
  begin
    TabSheet := TKuinkEditorTab(PageControl1.ActivePage);
    Editor := TSynEdit(tabSheet.Controls[0]);
    Opt := [];
    if frWholeWord in ReplaceDialog1.Options then
      Include(Opt,ssoWholeWord);
    if frMatchCase in ReplaceDialog1.Options then
      Include(Opt,ssoMatchCase);
    if frEntireScope in ReplaceDialog1.Options then
       Include(Opt, ssoEntireScope);
    if not (frDown in ReplaceDialog1.Options) then
      Include(Opt,ssoBackwards);

    Editor.SearchReplace(ReplaceDialog1.FindText, '', Opt);
end;

procedure TIDEFrm.ReplaceDialog1Find(Sender: TObject);
  begin
       EditorFindNext();
  end;

procedure TIDEFrm.ReplaceDialog1Replace(Sender: TObject);
  var
    TabSheet: TKuinkEditorTab;
    Editor: TSynEdit;
    Opt : TSynSearchOptions;
  begin
    TabSheet := TKuinkEditorTab(PageControl1.ActivePage);
    Editor := TSynEdit(tabSheet.Controls[0]);
    Opt := [];
    if frWholeWord in ReplaceDialog1.Options then
      Include(Opt,ssoWholeWord);
    if frMatchCase in ReplaceDialog1.Options then
      Include(Opt,ssoMatchCase);
    if not (frDown in ReplaceDialog1.Options) then
      Include(Opt,ssoBackwards);
    if frEntireScope in ReplaceDialog1.Options then
       Include(Opt, ssoEntireScope);
    if (frReplace in ReplaceDialog1.Options) then
      Include(Opt,ssoReplace);
    if (frReplaceAll in ReplaceDialog1.Options) then
      Include(Opt,ssoReplaceAll);

    Editor.SearchReplace(ReplaceDialog1.FindText, ReplaceDialog1.ReplaceText, Opt);
end;

procedure TIDEFrm.SaveAllClick(Sender: TObject);
begin
  SaveAllNodes(False);
end;

{
procedure TIDEFrm.BeautifyNode(TabSheet: TKuinkEditorTab);
var
begin
end;
}

procedure TIDEFrm.SaveNode(TabSheet: TKuinkEditorTab);
var
  Editor: TSynEdit;
  XMLDoc: TXMLDocument;
  S: TStringStream;
  RefreshFileExplorer, SaveCanceled: Boolean;
begin
  RefreshFileExplorer := False;
  SaveCanceled := False;
  if TabSheet.Changed then
  begin
    Editor := TSynEdit(TabSheet.Controls[0]);
    //Validate the xml
    S := TStringStream.Create(Editor.Lines.GetText);
    try
      // Read complete XML document
      ReadXMLFile(XMLDoc, S);
    except
      on E: Exception do
         ShowMessage(E.Message);
    //finally
    //  S.Free;
    end;
    TabSheet.XmlNode := XMLDoc;
    S.Free;
    //Repopulate the node inspector
    PopulateNodeInspector(TabSheet);
    //XMLDoc.Free;

    If (TabSheet.Filename = 'untitled') Then
    begin
      SaveDialog1.InitialDir := ShellTreeView1.Root;
      if SaveDialog1.Execute then
      begin
          TabSheet.Filename := SaveDialog1.Filename;
          RefreshFileExplorer:=True;
      end
      else
          SaveCanceled:=True;
    end;

    If Not SaveCanceled Then
    begin
      StatusBar1.SimpleText := 'Saving... ' + TabSheet.Filename;
      Editor.Lines.SaveToFile(TabSheet.Filename);
      StatusBar1.SimpleText := 'Saved... ' + TabSheet.Filename;
      TabSheet.Changed := False;
      TabSheet.Caption := ExtractFileNameOnly(TabSheet.Filename);
      If RefreshFileExplorer Then
         ShellTreeView1.Refresh;
    end;
  end;
end;

procedure TIDEFrm.SaveAllNodes(Ask: Boolean);
var
  TabSheet: TKuinkEditorTab;
  i: Integer;
  Reply, BoxStyle: Integer;
  Message: String;
begin
  for i := 0 to PageControl1.PageCount - 1 do
  begin
    TabSheet := TKuinkEditorTab(PageControl1.Pages[i]);
    if TabSheet.Changed then
    begin
      if Ask then
      begin
        BoxStyle := MB_ICONQUESTION + MB_YESNOCANCEL;
        Message := 'Save ' + ExtractFileName(Tabsheet.Filename)+ '?';
        Reply := Application.MessageBox(PChar(Message), 'Content changed', BoxStyle);
        if Reply = IDYES then
          SaveNode(TabSheet);
      end
      else
          SaveNode(TabSheet);
    end;
  end;
end;

procedure TIDEFrm.OpenNode(FileName: String);
Var
  InnerPageControl: TPageControl;
  TabSheet: TKuinkEditorTab;
  Editor: TSynEdit;
  OnExitEventHandler: TNotifyEvent;
  NodeName: string;
  NodeType: TKuinkNodeType;
  NodeView: string;
  S: TStringStream;
  XMLDoc: TXMLDocument;
begin
  NodeName := ExtractFileNameOnly(FileName);
  PageControl1.Options := [nboShowCloseButtons];

  TabSheet := TKuinkEditorTab.Create(self);
  PageControl1.ActivePage := TabSheet;

  case NodeName of
    'application': NodeType := KuinkApplication;
    'process': NodeType := KuinkProcess;
    else
      NodeType := KuinkNode;
  end;
  TabSheet.Changed := False;
  TabSheet.NodeType := NodeType;
  TabSheet.Code := True; //Start in code view
  TabSheet.Parent := PageControl1;
  TabSheet.Filename := FileName;
  TabSheet.Caption := NodeName;//+'('+'Code'+')';

  OnExitEventHandler := @ClosePage;
  PageControl1.OnCloseTabClicked := onExitEventHandler;

  Editor := TSynEdit.Create(TabSheet);
  Editor.Parent := TabSheet;
  Editor.Align := alClient;
  Editor.Visible := True;
  Editor.Highlighter := SynXMLSyn1;
  Editor.LineHighlightColor.Background:=RGBToColor(246,224,63);
  Editor.LineHighlightColor.FrameColor:=RGBToColor(212,102,0);
  Editor.LineHighlightColor.FrameStyle:=slsDashed;
  Editor.BracketMatchColor.Background:=RGBToColor(173,189,6);
  Editor.OnChange := @PageChanged;
  Editor.OnDragOver := @EditorDragOver;
  Editor.OnDragDrop := @EditorDragDrop;
  Editor.OnKeyPress := @EditorAutoComplete;
  Editor.OnEnter:=@EditorEnter;
  Editor.Options:=[eoAutoIndent, eoTabIndent, eoTabsToSpaces, eoBracketHighlight];
  //Editor.Options:=[eoAutoIndent,eoBracketHighlight,eoGroupUndo,eoScrollPastEol,eoSmartTabs,eoTabIndent,eoTabsToSpaces,eoTrimTrailingSpaces];

  Editor.TabWidth:=StrToInt(XmlConfig1.GetValue(CONFIG_EDITOR_TABWIDTH, CONFIG_EDITOR_TABWIDTH_DEFAULT));
  If (FileName <> 'untitled') Then
    Editor.Lines.LoadFromFile(TabSheet.Filename);

  NodeView:='Code';
  If Not tabSheet.Code Then
     NodeView:='Design';

  StatusBar1.SimpleText := 'NodeType: ' + '::' + NodeView + '::' + KuinkTypeToStr(TabSheet.NodeType);

  //Load the node into the XML object in TabSheet
  S := TStringStream.Create(Editor.Lines.GetText);
  try
    // Read complete XML document
    ReadXMLFile(XMLDoc, S);
  except
    on E: Exception do
       ShowMessage(E.Message);
  //finally
  //  S.Free;
  end;
  S.Free;
  TabSheet.XmlNode := XMLDoc;

  //Populate NodeInspector
  PopulateNodeInspector(TabSheet);
  //XMLDoc.Free;
end;

end.
