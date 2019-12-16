(* ::Package:: *)

Paclet[
	Name -> "JupyterConvert",
	Version -> "0.3.0",
	MathematicaVersion -> "12.0+",
	Loading -> Automatic,
	Extensions -> {
		{
			"Kernel",
			Root -> "Kernel",
			Context -> {"JupyterConvert`"},
			Symbols -> {
				"JupyterConvert`Jupyter2Notebook",
				"JupyterConvert`Notebook2Jupyter"
			},
			HiddenImport -> True
		}
	}
]
