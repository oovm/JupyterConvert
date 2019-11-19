(* ::Package:: *)

ClearAll["`*"];

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
	JupyterCodeCell["Input", out]
];
parseCell["Output", boxes_, co_CellObject] := Block[
	{data},
	data = <|"image/png" -> ExportString[Rasterize@co, {"Base64", "PNG"}]|>;
	JupyterCodeCell["Output", data]
];


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
