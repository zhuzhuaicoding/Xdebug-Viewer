object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Xdebug viewer'
  ClientHeight = 162
  ClientWidth = 650
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 650
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 144
    Width = 650
    Height = 18
    Panels = <
      item
        Width = 150
      end
      item
        Style = psOwnerDraw
        Width = 50
      end>
    OnDrawPanel = StatusBarDrawPanel
  end
  object ProgressBar: TProgressBar
    Left = 208
    Top = 48
    Width = 150
    Height = 17
    TabOrder = 1
  end
  object TreeView: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 650
    Height = 144
    Align = alClient
    Header.AutoSizeIndex = 7
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoVisible]
    Header.Style = hsFlatButtons
    HintMode = hmHint
    ParentShowHint = False
    PopupMenu = PopupMenu1
    ShowHint = True
    TabOrder = 2
    TreeOptions.SelectionOptions = [toFullRowSelect]
    TreeOptions.StringOptions = [toSaveCaptions]
    OnCollapsed = TreeViewExpanded
    OnExpanded = TreeViewExpanded
    OnGetText = TreeViewGetText
    OnPaintText = TreeViewPaintText
    OnGetNodeDataSize = TreeViewGetNodeDataSize
    OnInitNode = TreeViewInitNode
    Columns = <
      item
        Position = 0
        Width = 60
        WideText = 'Start time'
      end
      item
        Position = 1
        Width = 60
        WideText = 'End time'
      end
      item
        Position = 2
        Width = 60
        WideText = 'Total time'
      end
      item
        Position = 3
        Width = 80
        WideText = 'Start memory'
      end
      item
        Position = 4
        Width = 80
        WideText = 'End memory'
      end
      item
        Position = 5
        Width = 80
        WideText = 'Memory delta'
      end
      item
        Position = 6
        Width = 100
        WideText = 'Function name'
      end
      item
        Position = 7
        Width = 126
        WideText = 'File name'
      end>
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'xt'
    Filter = 'Xdebug trace files (*.xt)|*.xt|All files (*.*)|*.*'
    Options = [ofEnableSizing]
    Title = 'Open file'
    Left = 8
    Top = 120
  end
  object ActionList1: TActionList
    Left = 40
    Top = 120
    object OpenFileAction: TAction
      Caption = 'Open file'
      ShortCut = 16463
      OnExecute = OpenFileActionExecute
    end
    object CloseFileAction: TAction
      Caption = 'CloseFileAction'
      ShortCut = 16471
      OnExecute = CloseFileActionExecute
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 72
    Top = 120
    object Openfile1: TMenuItem
      Action = OpenFileAction
    end
    object Closefile1: TMenuItem
      Action = CloseFileAction
      Caption = 'Close file'
    end
  end
end
