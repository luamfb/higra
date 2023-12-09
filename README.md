## What is this?

This is the implementation for the `higra` graph description language.

### Usage

To use the compiler, one must have the Standard ML compiler SML/NJ installed.
Then, run
```
$ sml higra.cm
```

And in the interactive Standard ML prompt, run
```
Main.main input_path output_path
```

Where `input_path` and `output_path` are strings containing the paths to,
respectively, the input file and the directory where the output SVGs
will be written.
Some example input files can be found in the `input/` directory.
