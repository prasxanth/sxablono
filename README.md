<a id='x-28SXABLONO-2EDOCS-3A-40INDEX-20MGL-PAX-3ASECTION-29'></a>

# Skeleton Template for Lisp Project

## Table of Contents

- [1 ASDF Details][71b0]
- [2 Project Structure][6462]
    - [2.1 Source Code][a270]
    - [2.2 Tests][427c]
    - [2.3 Documentation][f309]
- [3 How to Use][65a0]
- [4 API Reference][1dda]
    - [4.1 Auxillary Functions][c245]
    - [4.2 Utility Functions][3dd7]

###### \[in package SXABLONO.DOCS with nicknames SXDOCS\]
This is a skeleton template for a Lisp project that includes **tests** and **documentation** facilities. The purpose of this project is to create a starting point for the development of new *small/medium* libraries. Testing uses the [FIVEAM](https://github.com/lispci/fiveam) framework. Documentation uses the [MGL-PAX](http://melisgl.github.io/mgl-pax/) system.
<blockquote class="note">
Project name: **sxablono** `=>` *skeleton* in [Esperanto](https://en.wikipedia.org/wiki/Esperanto)!
</blockquote>

<a id='x-28SXABLONO-2EDOCS-3A-40ASDF-DETAILS-20MGL-PAX-3ASECTION-29'></a>

## 1 ASDF Details

- **Version**: 1.0.0

- **Description**: Skeleton template for Lisp project with test and documentation generator.

- **License**: Unlicense.

- **Author**: Prashanth Kumar

- **Mailto**: prasxanth.kumar@gmail.com

- **Homepage**: http://prasxanth.github.io/sxablono/

- **Bug tracker**: https://github.com/prasxanth/sxablono/issues

- **Source control**: http://github.com/sxablono.git


<a id='x-28SXABLONO-2EDOCS-3A-40PROJECT-STRUCTURE-20MGL-PAX-3ASECTION-29'></a>

## 2 Project Structure

The file and folder structure for the project are organized as follows,

```
├── LICENSE
├── docs
│   ├── build.lisp
│   ├── docs.lisp
│   ├── jquery.min.js
│   ├── package.lisp
│   ├── style.css
│   └── toc.min.js
├── src
│   ├── aux.lisp
│   ├── package.lisp
│   └── utils.lisp
├── sxablono.asd
└── tests
    ├── aux-tests.lisp
    ├── package.lisp
    ├── tests-suite.lisp
    └── utils-tests.lisp
```

There are three main directories for each of the components,

- Source Code: `src/`

- Tests: `tests/`

- Documentation: `docs/`

`sxablono.asd` contains all the system defintions for the source code, tests and documentation. The `package.lisp` file in each directory contains the respective package definitions. License documentation is in the `LICENSE` file.

The following sections explain each of the components and directory contents in more detail.
  

<a id='x-28SXABLONO-2EDOCS-3A-40SOURCE-CODE-20MGL-PAX-3ASECTION-29'></a>

### 2.1 Source Code

The `src/` folder contains all the **core** package files,

```
├── src
│   ├── aux.lisp
│   ├── package.lisp
│   └── utils.lisp
```

`package.lisp` contains the usual definitions for *use*, *imports* and *exports*,

```
(uiop:read-file-string "../src/package.lisp")
=> (defpackage #:sxablono
   (:nicknames #:sxbl)
   (:use #:cl)
   (:import-from #:mgl-pax
                 #:defsection)
   (:export #:do-the-job
	    #:foo)
   (:documentation "This is docstring for the package.

                   The package contains a function which does it's job by
                   applying transformation to the first and second arguments.

                   Note, despite the docstring indentation, it is displayed
                   correctly. And PAX is smart enough to distinguish the common
                   indentation and indentation of a code block below:

                       (apply #'foo 42)
                  "))
```

The example files `aux.lisp` and `utils.lisp` contain the full definitions in the package (macros, functions etc.).

<a id='x-28SXABLONO-2EDOCS-3A-40TESTS-20MGL-PAX-3ASECTION-29'></a>

### 2.2 Tests

The [FIVEAM](https://github.com/lispci/fiveam) framework is used for testing,

```
(uiop:read-file-string "../tests/package.lisp")
=> (defpackage #:sxablono.tests
   (:nicknames #:sxtests)
   (:documentation "Unit tests for SXABLONO")
   (:use #:cl #:fiveam #:sxablono)
   (:export #:principal-suite
  	    #:run-principal-suite))
```

As per Common Lisp convention, the filename containing the tests for each corresponding file in the `src/` folder is suffixed with `-tests`. Hence, `utils-tests.lisp` contains all the tests for `src/utils.lisp`, and `aux-tests.lisp` for `src/aux.lisp`.

```
└── tests
    ├── aux-tests.lisp
    ├── package.lisp
    ├── tests-suite.lisp
    └── utils-tests.lisp
```

The `tests-suite.lisp` contains `principal-suite`, the parent suite for all the tests.

To run the tests using `ASDF`,

```
(ql:quickload :sxablono)
(asdf:test-system :sxablono)
```


<a id='x-28SXABLONO-2EDOCS-3A-40DOCUMENTATION-20MGL-PAX-3ASECTION-29'></a>

### 2.3 Documentation

[MGL-PAX](http://melisgl.github.io/mgl-pax/) is used to generate the documentation.

```
(uiop:read-file-string "package.lisp")
=> (defpackage #:sxablono.docs
   (:nicknames #:sxdocs)
   (:use #:cl #:iterate)
   (:import-from #:mgl-pax
                 #:section
                 #:defsection)
   (:import-from #:sxablono
                 #:@aux
  		 #:@utils)
   (:export #:build))
```

`docs.lisp` contains the main narrative and contents. It includes both *handwritten* documentation and docstrings from functions in `src/`.

```
├── docs
│   ├── build.lisp
│   ├── docs.lisp
│   ├── jquery.min.js
│   ├── package.lisp
│   ├── style.css
│   └── toc.min.js
```

Documentation is generated using the `build` function defined in `build.lisp`.

This does the following,

- Updates *only* the `index.html` file. This file is created if it does not exist.

- Generates and overwrites the `README` and `README.md` files in the project root directory

After generating the documentation, the folder structure should resemble,

```
├── README
├── README.md
├── docs
│   ├── build.lisp
│   ├── docs.lisp
│   ├── index.html
│   ├── jquery.min.js
│   ├── package.lisp
│   ├── style.css
│   └── toc.min.js
```

A single `index.html` simplifies maintenance. It also eases [hosting on Github](https://pages.github.com/).

Larger documentation (distributed across multiple files) can be placed in a separate `build/` sub-directory. However, this makes the process of hosting the documentation slightly more complicated as the contents have to be moved or copied to the `gh-pages` branch of the repository.

<a id='x-28SXABLONO-2EDOCS-3A-40HOW-TO-USE-20MGL-PAX-3ASECTION-29'></a>

## 3 How to Use

As an example, consider a project named `spectre`, in the `~/lisp` directory, used to analyze and plot [continous spectra](https://en.wikipedia.org/wiki/Continuous_spectrum) data. The core functionality is contained in two files, `analyze.lisp` and `plot.lisp`.

*To adapt this template for the project, follow the steps below*,
<ol type="1">
<li> Download a copy to `~/lisp` and rename the folder to `spectre`. Replace all instances of `sxablono` with `spectre` in all the files. </li><li> In the `src/` directory,
<ol type="a">
<li>  Update `package.lisp`, </li>
<ol type="i">
<li> Confirm that the package name is `spectre`.</li>
<li> Modify the *uses*, *imports* and *exports*. </li>
<li> Do *not* delete the import of `defsection` from `mgl-pax`. </li>
<li> Include documentation for the package. </li>
</ol>
<li> Add source files, </li>
<ol type="i">
<li>  Add the core functionality (functions, macros, classes etc.). This can be in a single file or distributed across multiple files. </li>
<li> Ensure `(in-package :spectre)` is at the top of every file. </li>
</ol>
</ol>
</li><li> In the `tests/` directory,
<ol type="a">
<li> Confirm that the package name is `spectre.tests`.</li>
<li> Update `package.lisp` with the *uses*, *imports* and *exports*.  </li>
<li> Tests for each file in the `src/` directory should be in a corresponding file with the same name suffixed with `-tests`. So, tests should be called `analyze-tests.lisp` and `plot-tests.lisp`. </li>
<li> Update `tests-suite.lisp` with any additional tests. </li>
</ol>
</li><li> In the `docs/` directory,
<ol type="a">
<li> Confirm that the package name is `spectre.docs`.</li>
<li> Update `package.lisp` with the *uses*, *imports* and *exports*. </li>
<li> Import documentation for all exported function from relevant files in `src/`. </li>
</ol>
</li><li> Modify the `spectre.asd` file,
<ol type="a">
<li> Add/update the metadata (author, license, version, description etc.)  </li>
<li> Include all the details from the `package.lisp` files in each of the `src/`, `tests/` and `doc/` directories. </li>
</ol>
</li><li> Update the `LICENSE` file if applicable. </li><li> The directory structure should look like,

```
├── LICENSE
├── docs
│   ├── build.lisp
│   ├── docs.lisp
│   ├── jquery.min.js
│   ├── package.lisp
│   ├── style.css
│   └── toc.min.js
├── spectre.asd
├── src
│   ├── analyze.lisp
│   ├── package.lisp
│   └── plot.lisp
└── tests
    ├── analyze-tests.lisp
    ├── package.lisp
    ├── plot-tests.lisp
    └── tests-suite.lisp
```
</li><li> Create a softlink to `~/quicklisp/local-projects`,

```
ln -s ~/lisp/spectre ~/quicklisp/local-projects/spectre
```
</li><li> Quickload the project in SLIME to make it available,

```
(ql:quickload :spectre)
```
</li><li> Run the tests,

```
(asdf:test-system :spectre)
```
</li><li> Generate the documentation,

```
(ql:quickload :spectre.docs)
(spectre.docs:build)
```

The folder structure should now look like,

```
├── LICENSE
├── README
├── README.md
├── docs
│   ├── build.lisp
│   ├── docs.lisp
│   ├── index.html
│   ├── jquery.min.js
│   ├── package.lisp
│   ├── style.css
│   └── toc.min.js
├── spectre.asd
├── src
│   ├── analyze.lisp
│   ├── package.lisp
│   └── plot.lisp
└── tests
    ├── analyze-tests.lisp
    ├── package.lisp
    ├── plot-tests.lisp
    └── tests-suite.lisp
```

</li>
</ol>

<a id='x-28SXABLONO-2EDOCS-3A-40API-REFERENCE-20MGL-PAX-3ASECTION-29'></a>

## 4 API Reference

<a id='x-28SXABLONO-3A-40AUX-20MGL-PAX-3ASECTION-29'></a>

### 4.1 Auxillary Functions

###### \[in package SXABLONO with nicknames SXBL\]
This documentation is for the `aux.lisp` file.

It contains only one function definition:

<a id='x-28SXABLONO-3AFOO-20FUNCTION-29'></a>

- [function] **FOO** *FIRST &KEY (OTHER 100500)*

    **Synopsis** -- Function to concatentate arguments
    
    **Input Arguments**
    
    `FIRST` -- an *object*
    
    `OTHER` -- an *object*. Defaults to 100500.
    
    **Return Values**
    
    RESULT -- a *string*
    
    **Description**
    
    The documentation layout of this function is the template to be followed when documenting any function.
    
    Example of referencing another function -- [`sxablono:do-the-job`][e1ae].
    
    **Examples**
    
    The intent of the documentation is to ensure that it is as descriptive as possible, so the use of examples is highly encouraged. Liberal use of doctests, AKA *transcripts* in `MGL-PAX` terminology, as examples are strongly recommended as they are often better at conveying information. Along with tests, transcripts provide an additional layer of validation to the code.
    
    With default value of `:OTHER`,
    
    ```cl-transcript
    (foo :abc)
    => "ABC 100500"
    
    ```
    
    With custom value of `:OTHER`,
    
    ```cl-transcript
    (foo :abc :other 10101)
    => "ABC 10101"
    
    ```


Note that this capability to interleave file or package documentation within an overarching narrative can be used to provide context to the internal definitions.

An example of cross-referencing `->` For `sxablono`, here [`FOO`][7b82]
references [`SXABLONO:DO-THE-JOB`][e1ae], which is desribed in [Utility Functions][3dd7].

<a id='x-28SXABLONO-3A-40UTILS-20MGL-PAX-3ASECTION-29'></a>

### 4.2 Utility Functions

###### \[in package SXABLONO with nicknames SXBL\]
<a id='x-28SXABLONO-3ADO-THE-JOB-20FUNCTION-29'></a>

- [function] **DO-THE-JOB** *FIRST SECOND*

    **Synopsis** -- Function to concatenate arguments
    
    **Input Arguments**
    
    `FIRST` -- an *object*
    
    `SECOND` -- an *object*
    
    **Return Values**
    
    RESULT -- a *string*
    
    **Description**
    
    The documentation layout of this function is the template to be followed when documenting any function. It is akin to the *man* pages on Linux system.
    
    **Examples**
    
    The intent of the documentation is to ensure that it is as descriptive as possible so the use of examples is highly encouraged.
    
    ```cl-transcript
    (do-the-job :abc 1)
    => "ABC 1"
    
    ```


  [1dda]: #x-28SXABLONO-2EDOCS-3A-40API-REFERENCE-20MGL-PAX-3ASECTION-29 "API Reference"
  [3dd7]: #x-28SXABLONO-3A-40UTILS-20MGL-PAX-3ASECTION-29 "Utility Functions"
  [427c]: #x-28SXABLONO-2EDOCS-3A-40TESTS-20MGL-PAX-3ASECTION-29 "Tests"
  [6462]: #x-28SXABLONO-2EDOCS-3A-40PROJECT-STRUCTURE-20MGL-PAX-3ASECTION-29 "Project Structure"
  [65a0]: #x-28SXABLONO-2EDOCS-3A-40HOW-TO-USE-20MGL-PAX-3ASECTION-29 "How to Use"
  [71b0]: #x-28SXABLONO-2EDOCS-3A-40ASDF-DETAILS-20MGL-PAX-3ASECTION-29 "ASDF Details"
  [7b82]: #x-28SXABLONO-3AFOO-20FUNCTION-29 "(SXABLONO:FOO FUNCTION)"
  [a270]: #x-28SXABLONO-2EDOCS-3A-40SOURCE-CODE-20MGL-PAX-3ASECTION-29 "Source Code"
  [c245]: #x-28SXABLONO-3A-40AUX-20MGL-PAX-3ASECTION-29 "Auxillary Functions"
  [e1ae]: #x-28SXABLONO-3ADO-THE-JOB-20FUNCTION-29 "(SXABLONO:DO-THE-JOB FUNCTION)"
  [f309]: #x-28SXABLONO-2EDOCS-3A-40DOCUMENTATION-20MGL-PAX-3ASECTION-29 "Documentation"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
