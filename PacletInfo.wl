PacletObject[
  <|
    "Name" -> "QED",
    "Version" -> "0.0.1",
    "WolframVersion" -> "13.0+",
    "SystemID" -> All,              (* работает на всех платформах *)
    
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "src",
        "Context" -> {"QED`"},
        "Loading" -> "Manual"
      },
      {
        "Documentation",
        "Language" -> "English"
      }
    },
    
	  "Description" -> "Quantum qubit simulation package",
	  "Creator" -> "grumen911",
	  "URL" -> "https://github.com/grumen911/QED",
	  "License" -> "MIT", 
	  
	  (* версифицирование *)
	  "Updating" -> "Manual",         (* "Manual" или "Automatic" *)
	  
	  (* категория в Repository *)
	  "Categories" -> {
	    "Physics",
	    "Quantum Computing",
	    "Superconductivity"
	  }
  |>
]