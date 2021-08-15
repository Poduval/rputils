# Changes in version 0.0.3

  * Fixed bug with the calculation of `factorLevelLength`.
  * Updatede package description.
  
# Changes in version 0.0.2

  * `class(rp)`: Added option to print only top or bottom records, default remains the same (prints all). Added parameter to set a threshold for automatic assignment of a character variable to factor for the summary analysis, default threshold is set to 30%. 
  * Updated the print method for `class(rp)`. Now with some additional parameter one can print the variable lists with it's type only. Fixed the issue with number of records to be printed.
  * `summary.rp`: Now it has a class `rp_summary` with a print method. Added `factorLevelLenth` in the in the list output. 
  
# Changes in version 0.0.1
  * Added class `rp`
  * Added `print` and `summary` methods for class `rp`
