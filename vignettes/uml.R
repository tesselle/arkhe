# Compile UML diagrams with the 'nomnoml' package
library(nomnoml)

nomnoml::nomnoml("
[<package>arkhe|

[base::matrix||]

[DataMatrix|
 +groups: character|
 has_groups();
 get_groups();
 set_groups();
 as_long();
 as_features();
 replace_NA();
 remove_NA();
 remove_zero();
 remove_empty()
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

[AbundanceMatrix|
 +total: numeric|
 AbundanceMatrix();
 as_abundance();
 get_totals();
 set_totals()
]
[SimilarityMatrix|
 +method: character|
 as_similarity();
 get_method()
 set_method()
]

[IncidenceMatrix||
 IncidenceMatrix();
 as_incidence()
]
[StratigraphicMatrix||
 as_stratigraphy()
]


[base::matrix] <:- [DataMatrix]

[DataMatrix] <:- [IntegerMatrix]
[DataMatrix] <:- [NumericMatrix]
[DataMatrix] <:- [LogicalMatrix]

[IntegerMatrix] <:- [CountMatrix]
[IntegerMatrix] <:- [OccurrenceMatrix]

[NumericMatrix] <:- [AbundanceMatrix]
[NumericMatrix] <:- [SimilarityMatrix]

[LogicalMatrix] <:- [IncidenceMatrix]
[LogicalMatrix] <:- [StratigraphicMatrix]
]
", png = "vignettes/uml-1.png", width = NULL, height = NULL)
