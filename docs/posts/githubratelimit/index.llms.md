Lately I have been getting this error on occasion when running `devtools::install()` in R:

    > devtools::install()
    Error: HTTP error 401.
      Bad credentials

      Rate limit remaining: 59/60
      Rate limit reset at: 2023-07-27 21:55:54 UTC

The solution, from <https://community.rstudio.com/t/unable-to-install-packages-from-github/124372>, is to do

    Sys.unsetenv("GITHUB_PAT")

There have been past reports on this in the devtools package, but it was apparently fixed some time ago. I am using Mac OS 13.4, R 4.2.3. This line does fix it, though.

------------------------------------------------------------------------

To subscribe, go to <https://brianomeara.info/blog.xml> in an RSS reader.

## Citation

BibTeX citation:

``` quarto-appendix-bibtex
@online{o'meara2023,
  author = {O’Meara, Brian},
  title = {Github Rate Limit},
  date = {2023-07-27},
  url = {https://brianomeara.info/posts/githubratelimit/},
  langid = {en}
}
```

For attribution, please cite this work as:

O’Meara, Brian. 2023. “Github Rate Limit.” July 27. <https://brianomeara.info/posts/githubratelimit/>.
