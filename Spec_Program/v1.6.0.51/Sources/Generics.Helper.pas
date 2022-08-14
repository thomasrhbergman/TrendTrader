unit Generics.Helper;

interface
{$REGION 'Region uses'}
uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults,
  Winapi.Messages, IABSocketAPI, IABSocketAPI_const;
{$ENDREGION}

type
  TInterfacedList<T> = class(TList<T>, IInterface)
  private const
    objDestroyingFlag = Integer($80000000);
  private
    [Volatile] FRefCount: Integer;
    class procedure __MarkDestroying(const Obj); static; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  public
    constructor Create; overload;
  end;

 TInterfacedObjectDictionary<TKey,TValue> = class(TObjectDictionary<TKey,TValue>, IInterface)
  private const
    objDestroyingFlag = Integer($80000000);
  private
    [Volatile] FRefCount: Integer;
    class procedure __MarkDestroying(const Obj); static; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  public
   constructor Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer = 0);
 end;

implementation

{ TInterfacedList }

constructor TInterfacedList<T>.Create;
begin
  inherited;
  FRefCount := 1;
end;

function TInterfacedList<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TInterfacedList<T>._AddRef: Integer;
begin
{$IFNDEF AUTOREFCOUNT}
  Result := AtomicIncrement(FRefCount);
{$ELSE}
  Result := __ObjAddRef;
{$ENDIF}
end;

function TInterfacedList<T>._Release: Integer;
begin
{$IFNDEF AUTOREFCOUNT}
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
  begin
    // Mark the refcount field so that any refcounting during destruction doesn't infinitely recurse.
    __MarkDestroying(Self);
    Destroy;
  end;
{$ELSE}
  Result := __ObjRelease;
{$ENDIF}
end;

class procedure TInterfacedList<T>.__MarkDestroying(const Obj);
var
  LRef: Integer;
begin
  repeat
    LRef := TInterfacedList<T>(Obj).FRefCount;
  until AtomicCmpExchange(TInterfacedList<T>(Obj).FRefCount, LRef or objDestroyingFlag, LRef) = LRef;
end;

{ TInterfacedObjectDictionary<TKey, TValue> }

constructor TInterfacedObjectDictionary<TKey, TValue>.Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer);
begin
  inherited Create(ACapacity, nil);
  FRefCount := 1;
end;

function TInterfacedObjectDictionary<TKey, TValue>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TInterfacedObjectDictionary<TKey, TValue>._AddRef: Integer;
begin
{$IFNDEF AUTOREFCOUNT}
  Result := AtomicIncrement(FRefCount);
{$ELSE}
  Result := __ObjAddRef;
{$ENDIF}
end;

function TInterfacedObjectDictionary<TKey, TValue>._Release: Integer;
begin
{$IFNDEF AUTOREFCOUNT}
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
  begin
    // Mark the refcount field so that any refcounting during destruction doesn't infinitely recurse.
    __MarkDestroying(Self);
    Destroy;
  end;
{$ELSE}
  Result := __ObjRelease;
{$ENDIF}
end;

class procedure TInterfacedObjectDictionary<TKey, TValue>.__MarkDestroying(const Obj);
var
  LRef: Integer;
begin
  repeat
    LRef := TInterfacedObjectDictionary<TKey,TValue>(Obj).FRefCount;
  until AtomicCmpExchange(TInterfacedObjectDictionary<TKey,TValue>(Obj).FRefCount, LRef or objDestroyingFlag, LRef) = LRef;
end;

end.
