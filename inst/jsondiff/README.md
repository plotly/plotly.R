# Create a plotly diff

Source the script `create_diff.R` and give the `create_diff()` function two plotly urls:

```r
source("create_diff.R")
create_diff(
 "https://plot.ly/~sfg/2141.png",
 "https://plot.ly/~sfg/2143.png"
)
```

View the demo [here](http://cpsievert.github.io/plotly-test-table/jsondiff/)

# Acknowledgements

* Diffing uses version 0.1.33 of [jsondiffpatch](https://github.com/benjamine/jsondiffpatch)
* Collapsing uses SHA 664b059ee2 of [renderjson](https://github.com/caldwell/renderjson)

# License 

The MIT License

Copyright (c) 2015 Carson Sievert

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
