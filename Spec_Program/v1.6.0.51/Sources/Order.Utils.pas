unit Order.Utils;

interface

{$REGION 'Region uses'}
uses
  System.Math, System.Generics.Collections, System.SysUtils, IABFunctions, IABSocketAPI, BrokerHelperAbstr, Document,
  Scanner.Types, Utils, Global.Types, DebugWriter, XmlFiles, Entity.Sokid, Common.Types,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} IABSocketAPI_const;

{$ENDREGION}

type
  TOrderUtils = class
  public
    class function CheckPrecautionarySettings(const aSecurityType: TIABSecurityType; const aQuantity: Integer; const aLastPrice: Double): TPrecautionarySettingTypes;
    class function GetPriceCoef(const aLastPrice: Double): Double;
    class function GetVolatilityCoef(const aVolatility: Double): Double;
  end;

implementation

{ TOrderUtils }

class function TOrderUtils.GetPriceCoef(const aLastPrice: Double): Double;
var
  Coef: Double;
begin
  Result := 1;
  if General.AdjustmentCoefficients.UseVolatilityCoef and (aLastPrice > 0) then
  begin
    Coef := General.AdjustmentCoefficients.Price / 100;
    Result := Abs(1 - Coef * (1 - (100 / aLastPrice)));
  end;
end;

class function TOrderUtils.GetVolatilityCoef(const aVolatility: Double): Double;
var
  Coef: Double;
begin
  Result := 1;
  if General.AdjustmentCoefficients.UseVolatilityCoef then
  begin
    Coef := General.AdjustmentCoefficients.Volatility / 100;
    Result := Abs(1 - Coef * (1 - (aVolatility {/ 100})));
  end;
end;

class function TOrderUtils.CheckPrecautionarySettings(const aSecurityType: TIABSecurityType; const aQuantity: Integer; const aLastPrice: Double): TPrecautionarySettingTypes;
var
  PreArr: TPrecautionarySettingArray;
begin
  Result := [];
  if General.PrecautionarySettings.TryGetValue(aSecurityType, PreArr) then
  begin
    if (PreArr[psOrderQuantityMax] > 0) then
      if (aQuantity > PreArr[psOrderQuantityMax]) then
        Include(Result, psOrderQuantityMax);

    if (PreArr[psMaxAllowedPrice] > 0) then
      if (aLastPrice > PreArr[psMaxAllowedPrice]) then
        Include(Result, psMaxAllowedPrice);

    if (PreArr[psMinAllowedPrice] > 0) then
      if (aLastPrice < PreArr[psMinAllowedPrice]) then
        Include(Result, psMinAllowedPrice);

    // General.PrecautionarySettings.AlgorithmTotalValueLimit;
    // General.PrecautionarySettings.NumberOfTicks;
    // General.PrecautionarySettings.Percentage;
    // General.PrecautionarySettings.TotalValueLimit;
    // General.PrecautionarySettings.AlgorithmSizeLimit;
  end;
end;


end.
