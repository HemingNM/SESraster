## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
❯ On windows-x86_64-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Neander Marcel Heming <neanderh@yahoo.com.br>'
  
  New submission

❯ On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking package subdirectories ... NOTE
  Problems with news in 'NEWS.md':
  No news entries found.

❯ On windows-x86_64-devel (r-devel)
  checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

❯ On ubuntu-gcc-release (r-release)
  checking CRAN incoming feasibility ... [5s/11s] NOTE
  Maintainer: ‘Neander Marcel Heming <neanderh@yahoo.com.br>’
  
  New submission

❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking examples ... NOTE
  Examples with CPU (user + system) or elapsed time > 5s
                  user system elapsed
  bootspat_naive 5.399  0.056   5.457

❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

❯ On fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... [6s/13s] NOTE
  Maintainer: ‘Neander Marcel Heming <neanderh@yahoo.com.br>’
  
  New submission
