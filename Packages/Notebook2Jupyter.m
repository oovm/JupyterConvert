(* ::Package:: *)

ClearAll["`*"];



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
