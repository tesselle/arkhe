# Compile UML diagrams with the 'nomnoml' package
library(nomnoml)

nomnoml::nomnoml("
[<package>arkhe|

[base::matrix||
 remove_NA();
 replace_NA();
 remove_Inf();
 replace_Inf();
 remove_zero();
 replace_zero();
 remove_empty()
]

[<abstract>AbundanceMatrix|
 +samples: character;
 +groups: character;
 +totals: numeric;
 +dates: integer;
 +tpq: integer;
 +taq: integer|
 get_samples();
 set_samples();
 has_groups();
 get_groups();
 set_groups();
 get_totals();
 set_totals();
 has_dates();
 get_dates();
 set_dates();
 has_terminus();
 get_terminus();
 set_terminus();
 get_tpq();
 set_tpq();
 get_taq();
 set_taq();
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
