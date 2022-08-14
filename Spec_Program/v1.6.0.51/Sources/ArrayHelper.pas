unit ArrayHelper;

///////////////////////////////////////////////////////////////////////////////
//
//  ArrayHelper  version 1.3
//  extends class TArray and add TArrayRecord<T> to make dynamic arrays
//  as simple, as TList
//
//  Copyright(c) 2017 by Willi Commer (wcs)
//  Licence GNU
//
//  Dynamic arrays are smart because its memore usage is handled by the memory
//  manager. But the funtion libraries are lean and differs from object based.
//  Based on TArray class, that gives Sort and Binary search, this unit will
//  extend TArray with functions available for TList or TStrings.
//  The next level is TArrayRecord<T> record type. It wraps a record around
//  the dynamic array. This give us the ability to use dynamic arrays like
//  objects with out the pain to organize the final Free call.
//
//  var
//    A: TArrayRecord<string>;
//    S: string;
//  begin
//    A.SetValues(['a','b','c']);
//    A.Add('d');
//    assert(  A.Count = 4 );    // same as length(A.Items);
//    assert(  A[1] = 'b' );
//    assert(  A.IndexOf('a') = 0 );
//    for S in A do
//      ..
//
//  For more examples see procedure Test_All_Helper_Functions
//  For updates check https://github.com/WilliCommer/ArrayHelper
//
//
//  History:
//  version 1.3
//    Enumeration added
//    new functions 'Unique' and 'CopyArray'
//
//  version 1.2
//    TArrayRecord<T>
//
///////////////////////////////////////////////////////////////////////////////

{ $DEFINE TEST_FUNCTION }  // change to active test function

interface

{$REGION 'Region uses'}
uses
  System.Classes, System.SysUtils, System.RTLConsts, System.Generics.Defaults, System.Generics.Collections;
{$ENDREGION}

type
  // callback function for function ForEach
  TArrayForEachCallback<T> = reference to procedure(var Value: T; Index: Integer);
  // callback function for function Map
  TArrayMapCallback<T> = reference to function(var Value: T; Index: Integer): Boolean;
  // callback function for function MapTo
  TArrayConvert<T, TTo> = reference to function(const Value: T): TTo;
  // callback function for function Find
  TArrayFindCallback<T> = reference to function(const Value: T): Boolean;

  // extends class TArray
  TArrayHelper = class helper for TArray
    // add item to array
    class function Add<T>(var Values: TArray<T>; Item: T): Integer; static;
    // add Unique item to array
    class function AddUnique<T>(var Values: TArray<T>; Item: T): Integer; static;
    // delete item at index
    class procedure Delete<T>(var Values: TArray<T>; Index: Integer); static;
    // insert item at index
    class procedure Insert<T>(var Values: TArray<T>; Index: Integer; Value: T); static;
    // append array
    class procedure AddRange<T>(var Values: TArray<T>; const ValuesToInsert: array of T); static;
    // insert array at index
    class procedure InsertRange<T>(var Values: TArray<T>; Index: Integer; const ValuesToInsert: array of T); static;
    // get index of equal item
    class function IndexOf<T>(var Values: TArray<T>; Item: T): Integer; overload; static;
    // get index of equal item (using IComparer)
    class function IndexOf<T>(var Values: TArray<T>; Item: T; const Comparer: IComparer<T>): Integer; overload; static;
    // get index of maximal item
    class function IndexOfMax<T>(var Values: TArray<T>): Integer; overload; static;
    // get index of maximal item (using IComparer)
    class function IndexOfMax<T>(var Values: TArray<T>; const Comparer: IComparer<T>): Integer; overload; static;
    // get index of minimal item
    class function IndexOfMin<T>(var Values: TArray<T>): Integer; overload; static;
    // get index of minimal item (using IComparer)
    class function IndexOfMin<T>(var Values: TArray<T>; const Comparer: IComparer<T>): Integer; overload; static;
    // is a equal item is member of values
    class function Contains<T>(var Values: TArray<T>; Item: T): Boolean; overload; static;
    // is a equal item is member of values (using IComparer)
    class function Contains<T>(var Values: TArray<T>; Item: T; const Comparer: IComparer<T>): Boolean; overload; static;
    // compare two arrays
    class function Compare<T>(const Values, ValuesToCompare: array of T): Boolean; overload; static;
    // compare two arrays (using IComparer)
    class function Compare<T>(const Values, ValuesToCompare: array of T; const Comparer: IComparer<T>): Boolean; overload; static;
    // ForEach
    class procedure ForEach<T>(var Values: TArray<T>; const Callback: TArrayForEachCallback<T>); static;
    // find with callback
    class function Find<T>(const Values: TArray<T>; const Callback: TArrayFindCallback<T>; const StartIndex: Integer = 0): Integer; overload; static;
    // return an array filtered and converted by callback function
    class function Map<T>(const Values: TArray<T>; const Callback: TArrayMapCallback<T>): TArray<T>; static;
    // get size
    class function Size<T>(var Values: TArray<T>): Integer; overload; static;
{$IFDEF TEST_FUNCTION}
    // test, debug and example function
    class procedure Test_All_Helper_Functions;
{$ENDIF TEST_FUNCTION}
  end;

type
  TArrayRecord<T> = record
  strict private
  type
    TEnumerator = class
    private
      FValue: ^TArrayRecord<T>;
      FIndex: Integer;
      function GetCurrent: T;
    public
      constructor Create(var AValue: TArrayRecord<T>);
      function MoveNext: Boolean;
      property Current: T read GetCurrent;
    end;
  private
    function GetCount: Integer;
    function GetItemAt(Index: Integer): T;
    procedure SetCount(const Value: Integer);
    procedure SetItemAt(Index: Integer; Value: T);
  public
    Items: TArray<T>;
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const AValues: array of T); overload;
    function GetEnumerator(): TEnumerator;

    function Add(const Value: T): Integer;
    function AddIfNotContains(const AItem: T): Boolean;
    function AddUnique(const Value: T): Integer;
    function Remove(const AItem: T): Boolean;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; Value: T);
    procedure SetItems(const Values: array of T);

    procedure AddRange(const ValuesToInsert: array of T); overload;
    procedure AddRange(const ValuesToInsert: TArrayRecord<T>); overload;

    procedure InsertRange(Index: Integer; const ValuesToInsert: array of T); overload;
    procedure InsertRange(Index: Integer; const ValuesToInsert: TArrayRecord<T>); overload;

    function IndexOf(Item: T): Integer; overload;
    function IndexOf(Item: T; const Comparer: IComparer<T>): Integer; overload;

    function IndexOfMax: Integer; overload;
    function IndexOfMax(const Comparer: IComparer<T>): Integer; overload;
    function IndexOfMin: Integer; overload;
    function IndexOfMin(const Comparer: IComparer<T>): Integer; overload;

    function Contains(Item: T): Boolean; overload;
    function Contains(Item: T; const Comparer: IComparer<T>): Boolean; overload;

    function Compare(const ValuesToCompare: array of T): Boolean; overload;
    function Compare(const ValuesToCompare: array of T; const Comparer: IComparer<T>): Boolean; overload;
    function Compare(const ValuesToCompare: TArrayRecord<T>): Boolean; overload;
    function Compare(const ValuesToCompare: TArrayRecord<T>; const Comparer: IComparer<T>): Boolean; overload;

    procedure ForEach(const Callback: TArrayForEachCallback<T>);
    function Find(const Callback: TArrayFindCallback<T>; const StartIndex: Integer = 0): Integer; overload;
    function Map(const Callback: TArrayMapCallback<T>): TArrayRecord<T>;
    function Convert<TTo>(const Callback: TArrayConvert<T, TTo>): TArrayRecord<TTo>;
    function Size: Integer;

    procedure Sort; overload;
    procedure Sort(const AComparer: IComparer<T>); overload;
    procedure Sort(const AComparer: IComparer<T>; AIndex, ACount: Integer); overload;
    function BinarySearch(const AItem: T; out AFoundIndex: Integer; const AComparer: IComparer<T>; AIndex, ACount: Integer): Boolean; overload;
    function BinarySearch(const AItem: T; out AFoundIndex: Integer; const AComparer: IComparer<T>): Boolean; overload;
    function BinarySearch(const AItem: T; out AFoundIndex: Integer): Boolean; overload;

    procedure Unique; // remove duplicates
    function CopyArray(FromIndex: Integer; Count: Integer = -1): TArrayRecord<T>; // return array slice

    // operator overloads
    class operator Equal(const L, R: TArrayRecord<T>): Boolean;
    class operator NotEqual(const L, R: TArrayRecord<T>): Boolean;

    property Count: Integer read GetCount write SetCount;
    property ItemAt[Index: Integer]: T read GetItemAt write SetItemAt; default;
  end;

  TDoubleArray = TArrayRecord<Double>;
  TStringArray = TArrayRecord<string>;
  TIntegerArray = TArrayRecord<Integer>;
  TDateArray = TArrayRecord<TDate>;
  TDateTimeArray = TArrayRecord<TDateTime>;

implementation

{ TArrayHelper }

class function TArrayHelper.Add<T>(var Values: TArray<T>; Item: T): Integer;
begin
  Result := Length(Values);
  SetLength(Values, Result + 1);
  Values[Result] := Item;
end;

class procedure TArrayHelper.Delete<T>(var Values: TArray<T>; Index: Integer);
var
  i: Integer;
begin
  if (Index < Low(Values)) or (Index > High(Values)) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  for i := Index + 1 to High(Values) do
    Values[i - 1] := Values[i];
  SetLength(Values, Length(Values) - 1);
end;

class procedure TArrayHelper.Insert<T>(var Values: TArray<T>; Index: Integer; Value: T);
var
  i, h: Integer;
begin
  if (Index < Low(Values)) or (Index > Length(Values)) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  h := High(Values);
  SetLength(Values, Length(Values) + 1);
  for i := h downto Index do
    Values[i + 1] := Values[i];
  Values[Index] := Value;
end;

class procedure TArrayHelper.InsertRange<T>(var Values: TArray<T>; Index: Integer; const ValuesToInsert: array of T);
var
  i, L, h: Integer;
begin
  L := Length(ValuesToInsert);
  if L = 0 then
    Exit;
  if (Index < Low(Values)) or (Index > Length(Values)) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  h := High(Values);
  SetLength(Values, Length(Values) + L);
  for i := h downto Index do
    Values[i + L] := Values[i];
  for i := Low(ValuesToInsert) to High(ValuesToInsert) do
    Values[Index + i] := ValuesToInsert[i];
end;

class procedure TArrayHelper.AddRange<T>(var Values: TArray<T>; const ValuesToInsert: array of T);
var
  i, Index: Integer;
begin
  Index := Length(Values);
  SetLength(Values, Length(Values) + Length(ValuesToInsert));
  for i := Low(ValuesToInsert) to High(ValuesToInsert) do
    Values[Index + i] := ValuesToInsert[i];
end;

class function TArrayHelper.AddUnique<T>(var Values: TArray<T>; Item: T): Integer;
begin
  Result := IndexOf<T>(Values, Item);
  if (Result = -1) then
    Result := Add<T>(Values, Item);
end;

class function TArrayHelper.IndexOf<T>(var Values: TArray<T>; Item: T; const Comparer: IComparer<T>): Integer;
begin
  for Result := Low(Values) to High(Values) do
    if Comparer.Compare(Values[Result], Item) = 0 then
      Exit;
  Result := -1;
end;

class function TArrayHelper.IndexOf<T>(var Values: TArray<T>; Item: T): Integer;
begin
  Result := IndexOf<T>(Values, Item, TComparer<T>.Default);
end;

class function TArrayHelper.IndexOfMax<T>(var Values: TArray<T>): Integer;
begin
  Result := IndexOfMax<T>(Values, TComparer<T>.Default);
end;

class function TArrayHelper.IndexOfMax<T>(var Values: TArray<T>; const Comparer: IComparer<T>): Integer;
var
  i: Integer;
begin
  if Length(Values) = 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  Result := 0;
  for i := 1 to High(Values) do
    if Comparer.Compare(Values[i], Values[Result]) > 0 then
      Result := i;
end;

class function TArrayHelper.IndexOfMin<T>(var Values: TArray<T>): Integer;
begin
  Result := IndexOfMin<T>(Values, TComparer<T>.Default);
end;

class function TArrayHelper.IndexOfMin<T>(var Values: TArray<T>; const Comparer: IComparer<T>): Integer;
var
  i: Integer;
begin
  if Length(Values) = 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  Result := 0;
  for i := 1 to High(Values) do
    if Comparer.Compare(Values[i], Values[Result]) < 0 then
      Result := i;
end;

class function TArrayHelper.Contains<T>(var Values: TArray<T>; Item: T; const Comparer: IComparer<T>): Boolean;
begin
  Result := IndexOf<T>(Values, Item, Comparer) <> -1;
end;

class function TArrayHelper.Contains<T>(var Values: TArray<T>; Item: T): Boolean;
begin
  Result := Contains<T>(Values, Item, TComparer<T>.Default);
end;

class function TArrayHelper.Compare<T>(const Values, ValuesToCompare: array of T; const Comparer: IComparer<T>): Boolean;
var
  i: Integer;
begin
  if Length(Values) <> Length(ValuesToCompare) then
    Exit(False);
  for i := Low(Values) to High(Values) do
    if Comparer.Compare(Values[i], ValuesToCompare[i]) <> 0 then
      Exit(False);
  Result := True;
end;

class function TArrayHelper.Compare<T>(const Values, ValuesToCompare: array of T): Boolean;
begin
  Result := Compare<T>(Values, ValuesToCompare, TComparer<T>.Default);
end;

class procedure TArrayHelper.ForEach<T>(var Values: TArray<T>; const Callback: TArrayForEachCallback<T>);
var
  i: Integer;
begin
  for i := Low(Values) to High(Values) do
    Callback(Values[i], i);
end;

class function TArrayHelper.Find<T>(const Values: TArray<T>; const Callback: TArrayFindCallback<T>; const StartIndex: Integer): Integer;
begin
  if (Length(Values) = 0) or (StartIndex < 0) or (StartIndex > High(Values)) then
    Exit(-1);
  for Result := StartIndex to High(Values) do
    if Callback(Values[Result]) then
      Exit;
  Result := -1;
end;

class function TArrayHelper.Map<T>(const Values: TArray<T>; const Callback: TArrayMapCallback<T>): TArray<T>;
var
  Item: T;
  i: Integer;
begin
  Result := nil;
  for i := Low(Values) to High(Values) do
  begin
    Item := Values[i];
    if Callback(Item, i) then
      Add<T>(Result, Item);
  end;
end;

class function TArrayHelper.Size<T>(var Values: TArray<T>): Integer;
begin
  Result := Length(Values);
end;

{ TArrayRecord<T>.TEnumerator }

constructor TArrayRecord<T>.TEnumerator.Create(var AValue: TArrayRecord<T>);
begin
  FValue := @AValue;
  FIndex := -1;
end;

function TArrayRecord<T>.TEnumerator.GetCurrent: T;
begin
  Result := FValue^.Items[FIndex];
end;

function TArrayRecord<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < High(FValue^.Items);
  Inc(FIndex);
end;

{ TArrayRecord<T> }

constructor TArrayRecord<T>.Create(ACapacity: Integer);
begin
  SetLength(Items, ACapacity);
end;

constructor TArrayRecord<T>.Create(const AValues: array of T);
begin
  SetLength(Items, 0);
  AddRange(AValues);
end;

procedure TArrayRecord<T>.Clear;
begin
  SetLength(Items, 0);
end;

class operator TArrayRecord<T>.Equal(const L, R: TArrayRecord<T>): Boolean;
begin
  Result := L.Compare(R);
end;

class operator TArrayRecord<T>.NotEqual(const L, R: TArrayRecord<T>): Boolean;
begin
  Result := not L.Compare(R);
end;

function TArrayRecord<T>.GetCount: Integer;
begin
  Result := Length(Items);
end;

function TArrayRecord<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TArrayRecord<T>.SetCount(const Value: Integer);
begin
  SetLength(Items, Value);
end;

procedure TArrayRecord<T>.SetItemAt(Index: Integer; Value: T);
begin
  Items[Index] := Value;
end;

procedure TArrayRecord<T>.SetItems(const Values: array of T);
begin
  SetLength(Items, 0);
  AddRange(Values);
end;

function TArrayRecord<T>.Size: Integer;
begin
  Result := Length(Items);
end;

function TArrayRecord<T>.GetItemAt(Index: Integer): T;
begin
  Result := Items[Index];
end;

procedure TArrayRecord<T>.AddRange(const ValuesToInsert: array of T);
begin
  TArray.AddRange<T>(Items, ValuesToInsert);
end;

procedure TArrayRecord<T>.AddRange(const ValuesToInsert: TArrayRecord<T>);
begin
  TArray.AddRange<T>(Items, ValuesToInsert.Items);
end;

function TArrayRecord<T>.AddUnique(const Value: T): Integer;
begin
  Result := TArray.IndexOf<T>(Items, Value);
  if (Result = -1) then
    Result := TArray.Add<T>(Items, Value);
end;

function TArrayRecord<T>.BinarySearch(const AItem: T; out AFoundIndex: Integer; const AComparer: IComparer<T>; AIndex, ACount: Integer): Boolean;
begin
  Result := TArray.BinarySearch<T>(Items, AItem, AFoundIndex, AComparer, AIndex, ACount);
end;

function TArrayRecord<T>.BinarySearch(const AItem: T; out AFoundIndex: Integer; const AComparer: IComparer<T>): Boolean;
begin
  Result := TArray.BinarySearch<T>(Items, AItem, AFoundIndex, AComparer);
end;

function TArrayRecord<T>.BinarySearch(const AItem: T; out AFoundIndex: Integer): Boolean;
begin
  Result := TArray.BinarySearch<T>(Items, AItem, AFoundIndex);
end;

procedure TArrayRecord<T>.Delete(Index: Integer);
begin
  TArray.Delete<T>(Items, Index);
end;

function TArrayRecord<T>.Remove(const AItem: T): Boolean;
var
  i: Integer;
begin
  i := IndexOf(AItem);
  if i < 0 then
    Result := False
  else
  begin
    Delete(i);
    Result := True;
  end;
end;

function TArrayRecord<T>.AddIfNotContains(const AItem: T): Boolean;
begin
  Result := Contains(AItem);
  if not Result then
    Add(AItem);
end;

function TArrayRecord<T>.Find(const Callback: TArrayFindCallback<T>; const StartIndex: Integer): Integer;
begin
  Result := TArray.Find<T>(Items, Callback, StartIndex);
end;

procedure TArrayRecord<T>.ForEach(const Callback: TArrayForEachCallback<T>);
begin
  TArray.ForEach<T>(Items, Callback);
end;

function TArrayRecord<T>.Compare(const ValuesToCompare: TArrayRecord<T>): Boolean;
begin
  Result := TArray.Compare<T>(Items, ValuesToCompare.Items);
end;

function TArrayRecord<T>.Compare(const ValuesToCompare: TArrayRecord<T>; const Comparer: IComparer<T>): Boolean;
begin
  Result := TArray.Compare<T>(Items, ValuesToCompare.Items, Comparer);
end;

function TArrayRecord<T>.Compare(const ValuesToCompare: array of T): Boolean;
begin
  Result := TArray.Compare<T>(Items, ValuesToCompare);
end;

function TArrayRecord<T>.Compare(const ValuesToCompare: array of T; const Comparer: IComparer<T>): Boolean;
begin
  Result := TArray.Compare<T>(Items, ValuesToCompare, Comparer);
end;

function TArrayRecord<T>.Contains(Item: T; const Comparer: IComparer<T>): Boolean;
begin
  Result := TArray.Contains<T>(Items, Item, Comparer);
end;

function TArrayRecord<T>.Contains(Item: T): Boolean;
begin
  Result := TArray.Contains<T>(Items, Item);
end;

function TArrayRecord<T>.IndexOf(Item: T; const Comparer: IComparer<T>): Integer;
begin
  Result := TArray.IndexOf<T>(Items, Item, Comparer);
end;

function TArrayRecord<T>.IndexOfMax: Integer;
begin
  Result := TArray.IndexOfMax<T>(Items);
end;

function TArrayRecord<T>.IndexOfMax(const Comparer: IComparer<T>): Integer;
begin
  Result := TArray.IndexOfMax<T>(Items, Comparer);
end;

function TArrayRecord<T>.IndexOfMin: Integer;
begin
  Result := TArray.IndexOfMin<T>(Items);
end;

function TArrayRecord<T>.IndexOfMin(const Comparer: IComparer<T>): Integer;
begin
  Result := TArray.IndexOfMin<T>(Items, Comparer);
end;

function TArrayRecord<T>.IndexOf(Item: T): Integer;
begin
  Result := TArray.IndexOf<T>(Items, Item);
end;

procedure TArrayRecord<T>.Insert(Index: Integer; Value: T);
begin
  TArray.Insert<T>(Items, Index, Value);
end;

procedure TArrayRecord<T>.InsertRange(Index: Integer; const ValuesToInsert: TArrayRecord<T>);
begin
  TArray.InsertRange<T>(Items, Index, ValuesToInsert.Items);
end;

procedure TArrayRecord<T>.InsertRange(Index: Integer; const ValuesToInsert: array of T);
begin
  TArray.InsertRange<T>(Items, Index, ValuesToInsert);
end;

function TArrayRecord<T>.Map(const Callback: TArrayMapCallback<T>): TArrayRecord<T>;
begin
  Result.Items := TArray.Map<T>(Items, Callback);
end;

function TArrayRecord<T>.Convert<TTo>(const Callback: TArrayConvert<T, TTo>): TArrayRecord<TTo>;
var
  i: Integer;
begin
  Result.Clear;
  for i := Low(Items) to High(Items) do
    Result.Add(Callback(Items[i]));
end;

function TArrayRecord<T>.CopyArray(FromIndex: Integer; Count: Integer): TArrayRecord<T>;
var
  i: Integer;
begin
  Result.Clear;
  if Count < 0 then
    Count := Length(Items);
  if Length(Items) < (FromIndex + Count) then
    Count := Length(Items) - FromIndex;
  if Count > 0 then
  begin
    SetLength(Result.Items, Count);
    for i := 0 to Count - 1 do
      Result.Items[i] := Items[i + FromIndex];
  end;
end;

procedure TArrayRecord<T>.Sort;
begin
  TArray.Sort<T>(Items);
end;

procedure TArrayRecord<T>.Sort(const AComparer: IComparer<T>);
begin
  TArray.Sort<T>(Items, AComparer);
end;

procedure TArrayRecord<T>.Sort(const AComparer: IComparer<T>; AIndex, ACount: Integer);
begin
  TArray.Sort<T>(Items, AComparer, AIndex, ACount);
end;

function TArrayRecord<T>.Add(const Value: T): Integer;
begin
  Result := TArray.Add<T>(Items, Value);
end;

procedure TArrayRecord<T>.Unique;
var
  Hash: TDictionary<T, Integer>;
  i: Integer;
begin
  Hash := TDictionary<T, Integer>.Create(Length(Items));
  try
    for i := Low(Items) to High(Items) do
      Hash.AddOrSetValue(Items[i], 0);
    Items := Hash.Keys.ToArray;
  finally
    Hash.Free;
  end;
end;

{$IFDEF TEST_FUNCTION}

type
  TTestRecord = record
    Name: string;
    Age: Integer;
    constructor Create(AName: string; AAge: Integer);
    class function NameComparer: IComparer<TTestRecord>; static;
    class function AgeComparer: IComparer<TTestRecord>; static;
    class function ConvertToNames(const Value: TTestRecord): string; static;
    class function ConvertToAges(const Value: TTestRecord): Integer; static;
  end;

constructor TTestRecord.Create(AName: string; AAge: Integer);
begin
  Name := AName;
  Age := AAge;
end;

class function TTestRecord.ConvertToNames(const Value: TTestRecord): string;
begin
  Result := Value.Name;
end;

class function TTestRecord.ConvertToAges(const Value: TTestRecord): Integer;
begin
  Result := Value.Age;
end;

class function TTestRecord.AgeComparer: IComparer<TTestRecord>;
begin
  Result := TComparer<TTestRecord>.Construct(
    function(const Left, Right: TTestRecord): Integer
    begin
      Result := TComparer<Integer>.Default.Compare(Left.Age, Right.Age);
    end);
end;

class function TTestRecord.NameComparer: IComparer<TTestRecord>;
begin
  Result := TComparer<TTestRecord>.Construct(
    function(const Left, Right: TTestRecord): Integer
    begin
      Result := TComparer<string>.Default.Compare(Left.Name, Right.Name);
    end);
end;

procedure Test_TestRecord;
var
  List: TArrayRecord<TTestRecord>;
  StrList: TArrayRecord<string>;
  i: Integer;
begin
  // create list
  List.Clear;
  List.Add(TTestRecord.Create('Jack', 26));
  List.Add(TTestRecord.Create('Anton', 28));
  List.Add(TTestRecord.Create('Barbie', 50));
  List.Add(TTestRecord.Create('Mickey Mouse', 90));

  // sort by name
  List.Sort(TTestRecord.NameComparer);
  // convert to string array

  StrList := List.Convert<string>(TTestRecord.ConvertToNames);
  assert(StrList.Compare(['Anton', 'Barbie', 'Jack', 'Mickey Mouse']));

  // convert to Integer array
  assert(List.Convert<Integer>(TTestRecord.ConvertToAges).Compare([28, 50, 26, 90]));

  // sort by age
  List.Sort(TTestRecord.AgeComparer);
  assert(List[0].Name = 'Jack');

  // IndexOf Min / Max
  assert(List.IndexOfMax(TTestRecord.AgeComparer) = 3);
  assert(List.IndexOfMin(TTestRecord.AgeComparer) = 0);

  i := List.IndexOfMax(TTestRecord.NameComparer);
  assert(List[i].Name = 'Mickey Mouse');

  i := List.IndexOfMin(TTestRecord.NameComparer);
  assert(List[i].Name = 'Anton');

  // Unique
  List.Add(List[0]);
  List.Insert(2, List[1]);
  List.Insert(4, List[1]);
  List.Unique;
  List.Sort(TTestRecord.NameComparer);
  StrList := List.Convert<string>(TTestRecord.ConvertToNames);
  assert(StrList.Compare(['Anton', 'Barbie', 'Jack', 'Mickey Mouse']));

end;

function CompareJokerFunction(const Value: string): Boolean;
begin
  Result := LowerCase(Value) = 'joker';
end;

procedure TestArrayContainer;
const
  CWeek: array [1 .. 8] of string = ('Mon', 'Tues', 'Wednes', 'Bug', 'Thurs', 'Fri', 'Satur', 'Sun');
var
  AStr: TArrayRecord<string>;
  AI, AI2: TArrayRecord<Integer>;
  i: Integer;
  S: string;
begin
  AI := TArrayRecord<Integer>.Create(0);
  assert(AI.Count = 0);
  AStr := TArrayRecord<string>.Create(10);
  assert((AStr.Count = 10) and (AStr[1] = ''));

  // Create
  AI.Create([1, 2, 3]);
  assert(AI.Compare([1, 2, 3]));

  // Add
  AI.Clear;
  assert(AI.Add(1) = 0);
  assert(AI.Add(2) = 1);
  assert(AI.Add(3) = 2);

  // IndexOf
  assert(AI.IndexOf(1) = 0);
  assert(AI.IndexOf(2) = 1);
  assert(AI.IndexOf(5) = -1);

  // Contains
  assert(AI.Contains(2) = TRUE);
  assert(AI.Contains(5) = FALSE);
  assert(AI.Contains(5, TComparer<Integer>.Construct(
    function(const Left, Right: Integer): Integer
    begin
      Result := (Left + 4) - Right;
    end)) = TRUE);

  // Delete
  AI.Delete(1);
  assert(AI.Contains(2) = FALSE);
  assert(AI.Count = 2);
  try
    AI.Delete(2);
    assert(TRUE);
  except
  end; // exception expected
  AI.Delete(0);
  assert(AI.Count = 1);
  AI.Delete(0);
  assert(AI.Count = 0);
  try
    AI.Delete(0);
    assert(TRUE);
  except
  end; // exception expected

  // Insert
  AStr.Clear;
  AStr.Insert(0, 'one');
  AStr.Insert(0, 'two');
  assert(AStr.Count = 2);
  assert(AStr[0] = 'two');
  assert(AStr[1] = 'one');

  AStr.Insert(2, 'three');
  assert((AStr.Count = 3) and (AStr[2] = 'three'));

  // AddRange
  AI.Clear;
  AI.AddRange(TArray<Integer>.Create(4, 5, 6));
  assert((AI.Count = 3) and (AI[2] = 6));
  AI.AddRange(TArray<Integer>.Create(10, 11, 12));
  assert((AI.Count = 6) and (AI[5] = 12) and (AI[0] = 4));

  // Compare
  AI.Create([1, 2, 3]);
  AI2 := AI;
  assert(AI.Compare([1, 2, 3]));
  assert(AI.Compare(AI.Items));
  assert(AI.Compare(AI2));
  AI2.Add(4);
  assert(not AI.Compare(AI2));

  // Equal
  AI.Create([1, 2, 3, 4, 5, 6]);
  AI2 := AI;
  assert(AI = AI2);
  AI.AddRange(AI2);
  assert((AI.Count = 12) and (AI <> AI2));
  AI2.InsertRange(AI2.Count, AI2);
  assert((AI.Count = AI2.Count) and (AI = AI2));

  // InsertRange
  AI.Clear;
  AI.InsertRange(0, TArray<Integer>.Create(4, 5, 6));
  assert((AI.Count = 3) and (AI[2] = 6));
  AI.InsertRange(0, [10, 11, 12]);
  assert((AI.Count = 6) and (AI[5] = 6) and (AI[0] = 10));
  AI.InsertRange(3, [21, 22]);
  assert((AI.Count = 8) and (AI[7] = 6) and (AI[0] = 10) and (AI[3] = 21));

  // ForEach
  AI.Items := TArray<Integer>.Create(5, 4, 3, 2, 1);
  AStr.Clear;
  AI.ForEach(
    procedure(var Value: Integer; Index: Integer)
    begin
      Value := Value * 10;
      AStr.Add(IntToStr(Value));
    end);
  // sort
  AI.Sort;
  AStr.Sort;
  assert(AI.Compare([10, 20, 30, 40, 50]));
  assert(AStr.Compare(['10', '20', '30', '40', '50']));

  // Find
  AI.Clear;
  AStr.SetItems(['4', 'king', 'joker', '7', 'JOKER', 'joker', 'ace', 'joker']);
  i := -1;
  repeat
    i := AStr.Find(CompareJokerFunction, i + 1);
    if i >= 0 then
      AI.Add(i);
  until i < 0;
  assert(AI.Compare([2, 4, 5, 7]));

  // Map
  AI.Clear;
  for i := 1 to 50 do
    AI.Add(i);
  AI := AI.Map(
    function(var Value: Integer; Index: Integer): Boolean
    begin
      Result := (Value >= 10) and (Value < 20);
      if Result then
        Value := Value + 100;
    end);
  assert(AI.Count = 10);
  assert(AI[1] = 111);

  // Map <string>
  AStr.SetItems(CWeek);
  AStr := AStr.Map(
    function(var Value: string; Index: Integer): Boolean
    begin
      Result := Value <> 'Bug';
      Value := Value + 'day';
    end);
  assert(AStr.Contains('Monday'));
  assert(AStr.Contains('Sunday'));
  assert(not AStr.Contains('Bugday'));

  // enumerate
  AI.Clear;
  AStr.SetItems(CWeek);
  for S in AStr do
    AI.Add(Length(S));
  assert(AI.Count = AStr.Count);
  assert(AI.Compare([3, 4, 6, 3, 5, 3, 5, 3]));
  // check empty enumeration
  AStr.Clear;
  for S in AStr do
    AI.Add(Length(S));
  assert(AI.Compare([3, 4, 6, 3, 5, 3, 5, 3]));

  // Unique
  AI.Unique;
  AI.Sort;
  assert(AI.Compare([3, 4, 5, 6]));

  // CopyArray
  assert(AI.CopyArray(2).Compare([5, 6]));
  assert(AI.CopyArray(0, 2).Compare([3, 4]));
  assert(AI.CopyArray(1, 2).Compare([4, 5]));

end;

procedure TestArrayHelper;
var
  AI: TArray<Integer>;
  AStr: TArray<string>;
  i: Integer;
begin
  // Add
  AI := NIL;
  assert(TArray.Add<Integer>(AI, 1) = 0);
  assert(TArray.Add<Integer>(AI, 2) = 1);
  assert(TArray.Add<Integer>(AI, 3) = 2);

  // IndexOf
  assert(TArray.IndexOf<Integer>(AI, 1) = 0);
  assert(TArray.IndexOf<Integer>(AI, 2) = 1);
  assert(TArray.IndexOf<Integer>(AI, 5) = -1);

  // Contains
  assert(TArray.Contains<Integer>(AI, 2) = TRUE);
  assert(TArray.Contains<Integer>(AI, 5) = FALSE);
  assert(TArray.Contains<Integer>(AI, 5, TComparer<Integer>.Construct(
    function(const Left, Right: Integer): Integer
    begin
      Result := Left - (Right + 4);
    end)) = FALSE);

  // Delete
  TArray.Delete<Integer>(AI, 1);
  assert(TArray.Contains<Integer>(AI, 2) = FALSE);
  assert(Length(AI) = 2);
  try
    TArray.Delete<Integer>(AI, 2);
    assert(TRUE);
  except
  end; // exception expected
  TArray.Delete<Integer>(AI, 0);
  assert(Length(AI) = 1);
  TArray.Delete<Integer>(AI, 0);
  assert(Length(AI) = 0);
  try
    TArray.Delete<Integer>(AI, 0);
    assert(TRUE);
  except
  end; // exception expected

  // Insert
  AStr := NIL;
  TArray.Insert<string>(AStr, 0, 'one');
  TArray.Insert<string>(AStr, 0, 'two');
  assert(Length(AStr) = 2);
  assert(AStr[0] = 'two');
  assert(AStr[1] = 'one');

  TArray.Insert<string>(AStr, 2, 'three');
  assert((Length(AStr) = 3) and (AStr[2] = 'three'));

  // AddRange
  AI := NIL;
  TArray.AddRange<Integer>(AI, TArray<Integer>.Create(4, 5, 6));
  assert((Length(AI) = 3) and (AI[2] = 6));
  TArray.AddRange<Integer>(AI, TArray<Integer>.Create(10, 11, 12));
  assert((Length(AI) = 6) and (AI[5] = 12) and (AI[0] = 4));

  // InsertRange
  AI := NIL;
  TArray.InsertRange<Integer>(AI, 0, TArray<Integer>.Create(4, 5, 6));
  assert((Length(AI) = 3) and (AI[2] = 6));
  TArray.InsertRange<Integer>(AI, 0, TArray<Integer>.Create(10, 11, 12));
  assert((Length(AI) = 6) and (AI[5] = 6) and (AI[0] = 10));
  TArray.InsertRange<Integer>(AI, 3, TArray<Integer>.Create(21, 22));
  assert((Length(AI) = 8) and (AI[7] = 6) and (AI[0] = 10) and (AI[3] = 21));

  // ForEach
  AI := TArray<Integer>.Create(5, 4, 3, 2, 1);
  AStr := NIL;
  TArray.ForEach<Integer>(AI,
    procedure(var Value: Integer; Index: Integer)
    begin
      Value := Value * 10;
      TArray.Add<string>(AStr, IntToStr(Value));
    end);
  TArray.Sort<Integer>(AI);
  TArray.Sort<string>(AStr);
  assert(TArray.Compare<Integer>(AI, TArray<Integer>.Create(10, 20, 30, 40, 50)));
  assert(TArray.Compare<string>(AStr, TArray<string>.Create('10', '20', '30', '40', '50')));

  // Find
  AI := NIL;
  AStr := TArray<string>.Create('4', 'king', 'joker', '7', 'JOKER', 'joker', 'ace', 'joker');
  i := -1;
  repeat
    i := TArray.Find<string>(AStr, CompareJokerFunction, i + 1);
    if i >= 0 then
      TArray.Add<Integer>(AI, i);
  until i < 0;
  assert(TArray.Compare<Integer>(AI, TArray<Integer>.Create(2, 4, 5, 7)));

  // Map
  AI := NIL;
  for i := 1 to 50 do
    TArray.Add<Integer>(AI, i);
  AI := TArray.Map<Integer>(AI,
    function(var Value: Integer; Index: Integer): Boolean
    begin
      Result := (Value >= 10) and (Value < 20);
      if Result then
        Value := Value + 100;
    end);
  assert(Length(AI) = 10);
  assert(AI[1] = 111);

  // Map <string>
  AStr := TArray<string>.Create('Mon', 'Tues', 'Wednes', 'Thurs', 'Fri', 'Satur', 'Sun');
  AStr := TArray.Map<string>(AStr,
    function(var Value: string; Index: Integer): Boolean
    begin
      Result := TRUE;
      Value := Value + 'day';
    end);
  assert(TArray.Contains<string>(AStr, 'Monday'));
  assert(TArray.Contains<string>(AStr, 'Sunday'));

end;

class procedure TArrayHelper.Test_All_Helper_Functions;
begin
  TestArrayHelper;
  TestArrayContainer;
  Test_TestRecord;
end;

{$ENDIF TEST_FUNCTION}

end.
