BeginPackage["QED`"];

Unprotect["QED`*"];   (* Снимаем защиту *)
ClearAll["QED`*"];    (* Полностью очищаем определения и значения *)

(* ════════════════════════════════════════════════════════════════ *)
(* ФИЗИЧЕСКИЕ КОНСТАНТЫ И ОБЩИЕ СИМВОЛЫ *)
(* ════════════════════════════════════════════════════════════════ *)

(* Квант магнитного потока *)
$Phi0::usage = "Subscript[\[CapitalPhi], 0] - magnetic flux quantum (h/2e ≈ 2.067×10⁻¹⁵ Wb).";
$Phi0 = Subscript[Symbol["\[CapitalPhi]"], 0];

$Phi0Value::usage = "Numerical value of magnetic flux quantum in Wb.";
$Phi0Value = 2.067833848 * 10.^-15;

(* Константа Планка (приведённая) *)
$hbar::usage = "\[HBar] - reduced Planck constant (ℏ = h/2π ≈ 1.055×10⁻³⁴ J·s).";
$hbar = Symbol["\[HBar]"];

$hbarValue::usage = "Numerical value of reduced Planck constant in J·s.";
$hbarValue = 1.054571817 * 10.^-34;

(* Волновое сопротивление измерительного тракта (портов) *)
$Z0::usage = "Subscript[Z, 0] - reference impedance for scattering matrix ports.";
$Z0 = Subscript[Symbol["Z"], 0];

$Z0Value::usage = "Numerical value of reference impedance in Ohms.";
$Z0Value = 50.0;

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