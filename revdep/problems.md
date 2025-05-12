# tabula

<details>

* Version: 3.2.1
* GitHub: NA
* Source code: https://github.com/cran/tabula
* Date/Publication: 2025-04-02 10:10:02 UTC
* Number of recursive dependencies: 31

Run `revdepcheck::revdep_details(, "tabula")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘tinytest.R’
     ERROR
    Running the tests in ‘tests/tinytest.R’ failed.
    Last 13 lines of output:
      ----- FAILED[data]: test_diversity.R<15--15>
       call| expect_equal_to_reference(boot, file = "_snaps/heterogeneity_bootstrap.rds")
       diff| current does not match target read from _snaps/heterogeneity_bootstrap.rds
       diff| Length mismatch: comparison on first 4 components
      ----- FAILED[data]: test_diversity.R<29--29>
       call| expect_equal_to_reference(boot, file = "_snaps/evenness_bootstrap.rds")
       diff| current does not match target read from _snaps/evenness_bootstrap.rds
       diff| Length mismatch: comparison on first 4 components
      ----- FAILED[data]: test_richness.R<15--15>
       call| expect_equal_to_reference(boot, file = "_snaps/richness_bootstrap.rds")
       diff| current does not match target read from _snaps/richness_bootstrap.rds
       diff| Length mismatch: comparison on first 4 components
      Error: 3 out of 105 tests failed
      In addition: There were 15 warnings (use warnings() to see them)
      Execution halted
    ```

