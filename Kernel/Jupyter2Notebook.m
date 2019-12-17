(* ::Package:: *)

Jupyter2Notebook::usage = "";

Begin["`Private`"];

Options[Jupyter2Notebook] = {Debug -> False};
Jupyter2Notebook[in_String, o : OptionsPattern[]] := Block[
	{out = FileNameJoin[{DirectoryName@in, FileBaseName@in <> ".nb"}]},
	Jupyter2Notebook[in, out, o]
];
Jupyter2Notebook[in_String, out_String, o : OptionsPattern[]] := Block[
	{ipy, lang},
	ipy = Import[in, "RawJSON"];
	lang = ipy["metadata", "kernelspec", "name"];
	lang = Which[
		StringStartsQ[lang, "python"], "Python",
		True, "Python"
	];
	CellParser[lang] /@ ipy["cells"]
];

CellParser[lang_String][cell_Association] := Switch[
	cell["cell_type"],
	"markdown", MarkdownParser[StringJoin[cell["source"]]],
	"code", CodeParser[lang, StringJoin[cell["source"]], cell["outputs"]]
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
EchoMake[text_String] := Cell[BoxData[text], "Echo"];
OutputMake[] := Cell[BoxData["1"], "Output", CellLabel -> "Out[ ]="];

End[]