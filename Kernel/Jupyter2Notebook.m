(* ::Package:: *)

Jupyter2Notebook::usage = "";

Begin["`Private`"];

Jupyter2Notebook[in_String, o : OptionsPattern[]] := Block[
	{out = FileNameJoin[{DirectoryName@in, FileBaseName@in <> ".nb"}]},
	Jupyter2Notebook[in, out, o]
];


WolframLanguageMake[text_String] := Block[
	{boxes = BoxData[RowBox[{"Echo", "[", "1", "]"}]]},
	Cell[boxes, "Input", CellLabel -> "In[ ]:="]
];
ExternalLanguageMake[lang_String, text_String] := Cell[
	text, "ExternalLanguage",
	CellEvaluationLanguage -> lang,
	CellLabel -> "In[ ]:="
];
EchoMake[text_String] := Cell[BoxData[text], "Echo"]
OutputMake[] := Cell[BoxData["1"], "Output", CellLabel -> "Out[ ]="];

End[]