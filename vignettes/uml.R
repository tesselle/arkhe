# Compile UML diagrams with the 'nomnoml' package
library(nomnoml)

nomnoml::nomnoml("
[<package>arkhe|

[base::matrix||
 replace_NA();
 replace_Inf();
 replace_zero();
 remove_NA();
 remove_Inf();
 remove_zero();
 remove_empty()
]

[<abstract>AbundanceMatrix|
 +samples: character;
 +groups: character;
 +totals: numeric;
 +date_from: integer;
 +date_to: integer|
 get_samples();
 set_samples();
 has_groups();
 get_groups();
 set_groups();
 get_totals();
 set_totals();
 get_dates();
 set_dates();
 as_long();
 as_features()
]

[IntegerMatrix||]
[NumericMatrix||]
[LogicalMatrix||]

[CountMatrix||
 CountMatrix();
 as_count()
]
[OccurrenceMatrix|
 +total: integer|
 as_occurrence();
 get_totals()
]

[CompositionMatrix||
 CompositionMatrix();
 as_composition()
]

[IncidenceMatrix||
 IncidenceMatrix();
 as_incidence()
]
[StratigraphicMatrix||
 as_stratigraphy()
]


[CountMatrix] -:> [AbundanceMatrix]
[CompositionMatrix] -:> [AbundanceMatrix]
[IncidenceMatrix] -:> [AbundanceMatrix]

[base::matrix] <:- [IntegerMatrix]
[base::matrix] <:- [NumericMatrix]
[base::matrix] <:- [LogicalMatrix]

[IntegerMatrix] <:- [CountMatrix]
[IntegerMatrix] <:- [OccurrenceMatrix]

[NumericMatrix] <:- [CompositionMatrix]

[LogicalMatrix] <:- [IncidenceMatrix]
[LogicalMatrix] <:- [StratigraphicMatrix]
]
", png = "vignettes/uml-1.png", width = NULL, height = NULL)
