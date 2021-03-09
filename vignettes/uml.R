# Compile UML diagrams with the 'nomnoml' package
library(nomnoml)

nomnoml::nomnoml("
[<package>arkhe|

[base::matrix||
 replace_NA();
 remove_NA();
 remove_zero();
 remove_empty()
]

[<abstract>AbundanceMatrix|
 +samples: character;
 +groups: character|
 get_samples();
 set_samples();
 has_groups();
 get_groups();
 set_groups();
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

[CompositionMatrix|
 +total: numeric|
 CompositionMatrix();
 as_composition();
 get_totals();
 set_totals()
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
