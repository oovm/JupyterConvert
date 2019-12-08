(* ::Package:: *)

Notebook2Jupyter::usage = "";

Begin["`Private`"];
JupyterInputCell::usage = "";
JupyterCodeCell::usage = "";
JupyterMarkdownCell::usage = "";
JupyterRawCell::usage = "";


Options[Notebook2Jupyter] = {};
Notebook2Jupyter[nb_NotebookObject, o : OptionsPattern[]] := Block[
	{jp = $JupyterTemplate, parsed, cells},
	parsed = Flatten[parseCell /@ Cells[nb]];
	cells = SequenceSplit[parsed, {
		{text__JupyterMarkdownCell} :> JupyterMarkdownBuild[First /@ {text}],
		{in_JupyterInputCell, other__JupyterCodeCell} :> JupyterCodeBuild[First /@ {in, other}]
	}];
	jp["cells"] = cells;
	Return@jp;
];
Notebook2Jupyter[nb_NotebookObject, path_String, o : OptionsPattern[]] := Block[
	{jp = Notebook2Jupyter[nb, o]},
	File@Export[path, jp, "JSON"]
];


JupyterMarkdownBuild[text_List] := <|
	"cell_type" -> "markdown",
	"source" -> StringRiffle[text, "\n\n"]
|>;
JupyterCodeBuild[{code_, print___, out_}] := Block[
	{stdout = {print}},
	<|
		"cell_type" -> "code",
		"source" -> code,
		"outputs" -> Flatten@{
			If[
				stdout == {},
				Nothing,
				<|"name" -> "stdout", "output_type" -> "stream", "text" -> #|>& /@ print
			],
			<|
				"output_type" -> "execute_result",
				"data" -> out
			|>
		}
	|>
];


(* ::Chapter:: *)
(*Cell*)


(* ::Section:: *)
(*Template*)


$JupyterTemplate = <|
	"metadata" -> <||>
|>;


(* ::Section:: *)
(*Default*)


parseCell[co_CellObject] := parseCell[NotebookRead[co], co];
parseCell[c_Cell, co_CellObject] := parseCell[#2, #, co]& @@ c;
parseCell[s_, o___] := (
	Echo[Inactive[parseCell][s, o], "Todo: "];
	JupyterMarkdownCell@TemplateApply["[//]: # (No rules defined for ``)\n\n", {s}]
);


(* ::Section:: *)
(*Normal*)


parseCell["Title", data_, co_CellObject] := JupyterMarkdownCell["# " <> parseData@data];
parseCell["Subtitle", data_, co_CellObject] := JupyterMarkdownCell["## " <> parseData@data];
parseCell["Chapter", data_, co_CellObject] := JupyterMarkdownCell["### " <> parseData@data];
parseCell["Section", data_, co_CellObject] := JupyterMarkdownCell["#### " <> parseData@data];
parseCell["Subsection", data_, co_CellObject] := JupyterMarkdownCell["##### " <> parseData@data];
parseCell["Subsubsection", data_, co_CellObject] := JupyterMarkdownCell["###### " <> parseData@data];


parseCell["Text", data_, co_CellObject] := JupyterMarkdownCell[parseData@data];
parseCell["WolframAlphaShort", data_String, co_CellObject] := JupyterMarkdownCell[data];


(* ::Section:: *)
(*Code*)


parseCell["Input", boxes_, co_CellObject] := Block[
	{expr = MakeExpression[Cell[boxes], StandardForm], out},
	out = expr //. {
		HoldComplete[ExpressionCell[{a___, Null, b___}]] :> StringJoin[ToString[HoldForm@a], ";\n", ToString[HoldForm@b]],
		HoldComplete[ExpressionCell[a_]] :> ToString[HoldForm@a]
	};
	JupyterInputCell[out]
];
parseCell["Output", boxes_, co_CellObject] := Block[
	{data},
	data = <|"image/png" -> ExportString[Rasterize@co, {"Base64", "PNG"}, Background -> None]|>;
	JupyterCodeCell[data]
];


(* ::Section:: *)
(*TeX*)


boxesToTeX = ToString[ToExpression@#, TeXForm] &;
parseCell["Output", BoxData[FormBox[boxes_, TraditionalForm]], cellObj_CellObject] := TemplateApply["$$``$$\n\n", {boxesToTeX@boxes}];


(* ::Section:: *)
(*Pass*)


parseCell["Echo", data___] := {};
parseCell["Print", data___] := {};
parseCell["Message", data___] := {};
parseCell["Code", data___] := {};
parseCell[$Failed, data___] := {};


(* ::Chapter:: *)
(*Data*)


parseData[list_List] := parseData /@ list;
parseData[string_String] := string;
parseData[cell_Cell] := parseData@First@cell;
parseData[boxes_] := (
	Echo[Inactive[parseData][boxes], "Todo: "];
	parseData@First@boxes
);


parseData[data_BoxData] := List @@ (parseData /@ data);
parseData[data_TextData] := List @@ (parseData /@ data);


parseData[TemplateBox[{text_String, link_String}, "HyperlinkURL"]] := TemplateApply["[``](``)", {text, link}]




End[]
