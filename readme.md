# JSON benchmark for Ada

This is just a simple json benchmark utilizing [json-ada](https://github.com/onox/json-ada) and [GNATCOLL.JSON](https://github.com/adacore/gnatcoll-core).  VSS.json is also included in the alire project, but not used.

The large json file was from [json-iterator/test-data](https://github.com/json-iterator/test-data). I edited the file, multiplied it by about 16, and added named arrays.

## How to use it

If you want to build it, you can also clone this repository, `cd` to the directory, then build it with `alr update && alr build` in Alire.

After that, you must **unzip** the `large-file.zip` file to `large-file.json`.

To run, issue the following command from the project directory: `bin/json_benchmark large-file.json`

## Contribute

If you would like to contribute, feel free to add VSS to this.  I already have the following in the `alire.toml` file:
```
[[depends-on]]
vss = "^23.0.0"
```
