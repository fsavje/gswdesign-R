## Submit release

* Check and fix any errors at https://cran.r-project.org/web/checks/check_results_gswdesign.html
* Delete content of `NAMESPACE` and run `devtools::document()`
* Run `devtools::load_all()`, `devtools::test()` and `devtools::check()`
* Update package information
    - Set new version number in `DESCRIPTION`
    - Set release date in `DESCRIPTION`
    - Change "distances devel" to "distances VERSION" in `NEWS.md`
    - Update travis and appveyor with current versions
    - Update `cran-comments.md` with correct information
* Commit and push to github so automatic tests run
* Run `devtools::check_win_devel()`, `devtools::check_win_release()` and `devtools::check_win_oldrelease()`
* Run `revdepcheck::revdep_check()`, remove "revdep" folder when done
* Wait until all tests are done
* Submit to CRAN
	- Run `devtools::build()`
	- Upload to http://cran.r-project.org/submit.html
	- Add `cran-comments.md` as comment


## When accepted

* Add new release to Github
	- Add CRAN release as binary
	- Add relevant information from `NEWS.md`
* Update package information
	- Add .9000 to the version number in `DESCRIPTION`
	- Set date in `DESCRIPTION`
	- Add "distances devel" to `NEWS.md`
	- Commit and push to github