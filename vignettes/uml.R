# Compile UML diagrams with the 'nomnoml' package
library(nomnoml)

nomnoml::nomnoml("
[<package>arkhe|

[<abstract>DataMatrix|
 +size: integer;
 +row_names: character;
 +column_names: character;
 +group_names: character|
 length();
 dim();
 rownames();
 colnames();
 dimnames();
 diag();
 has_groups();
 get_groups();
 set_groups();
 rowMeans();
 colMeans();
 rowSums();
 colSums();
 var();
 cov();
 cor();
 summary();
 Logic();
 Math();
 Math2();
 Summary();
 as.data.frame();
 as.matrix();
 as_long();
 as_features();
 remove_zero()
]

[<abstract>IntegerMatrix|
 + values: integer|
]
[<abstract>NumericMatrix|
 + values: numeric|
]
[<abstract>LogicalMatrix|
 + values: logical|
]

[CountMatrix||
 CountMatrix();
 as_count();
 ca()
]
[OccurrenceMatrix|
 +n: integer|
 as_occurrence();
 get_totals()
]

[AbundanceMatrix|
 +totals: numeric|
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
