BeginPackage["QED`"];

Unprotect["QED`*"];   (* Снимаем защиту *)
ClearAll["QED`*"];    (* Полностью очищаем определения и значения *)

(* публичные функции *)
MyInteractiveModule::usage = "MyInteractiveModule[initParams] creates interactive UI.";
MyCompareModule::usage  = "MyCompareModule[p1, p2] compares two parameter sets.";

(* ════════════════════════════════════════════════════════════════ *)
(* ФИЗИЧЕСКИЕ КОНСТАНТЫ И ОБЩИЕ СИМВОЛЫ *)
(* ════════════════════════════════════════════════════════════════ *)

(* Квант магнитного потока *)
$Phi0::usage = "Subscript[\[CapitalPhi], 0] - magnetic flux quantum (h/2e ≈ 2.067×10⁻¹⁵ Wb).";
$Phi0 = Subscript[Symbol["\[CapitalPhi]"], 0];

$Phi0Value::usage = "Numerical value of magnetic flux quantum in Wb.";
$Phi0Value = 2.067833848 * 10^-15;

(* Символ внешнего магнитного потока *)
$PhiExt::usage = "Subscript[\[CapitalPhi], ext] - external magnetic flux threading superconducting loops.";
$PhiExt = Subscript[Symbol["\[CapitalPhi]"], "ext"];

(* Базовые символы для параметров схемы *)
$CapacitanceSymbol::usage = "Base symbol C for capacitances. Use with Subscript[C, i].";
$CapacitanceSymbol = Symbol["C"];

$InductanceSymbol::usage = "Base symbol L for inductances. Use with Subscript[L, i].";
$InductanceSymbol = Symbol["L"];

$JosephsonEnergySymbol::usage = "Base symbol EJ for Josephson energies. Use with Subscript[EJ, i].";
$JosephsonEnergySymbol = Symbol["EJ"];

$JosephsonCapacitanceSymbol::usage = "Base symbol CJ for Josephson junction capacitances. Use with Subscript[CJ, i].";
$JosephsonCapacitanceSymbol = Symbol["CJ"];

(* Узловые динамические переменные *)
$FluxSymbol::usage = "Base symbol \[Phi] for node fluxes. Use with Subscript[\[Phi], i].";
$FluxSymbol = Symbol["\[Phi]"];

$ChargeSymbol::usage = "Base symbol q for node charges. Use with Subscript[q, i].";
$ChargeSymbol = Symbol["q"];

Begin["`Private`"];

Get[FileNameJoin[{DirectoryName[$InputFileName], "Bootstrap.wl"}]];
QED`Bootstrap`InitQED[];

End[];
EndPackage[];