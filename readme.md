# JSON benchmark for Ada

This is just a simple json benchmark utilizing [json-ada](https://github.com/onox/json-ada), [GNATCOLL.JSON](https://github.com/adacore/gnatcoll-core), and [VSS.JSON](https://github.com/AdaCore/VSS).

The large json file was from [json-iterator/test-data](https://github.com/json-iterator/test-data). I edited the file, multiplied it by about 16, and added named arrays.

All it does is find the array with the key "sixteen", and then does this:
```ada
declare
   Random_Node : constant JSON_Array := Result.Get ("sixteen");
begin
   for X of Random_Node loop
      declare
         Random_String : constant String := X.Get ("id");
         Random_Int    : constant Long_Integer := X.Get ("actor").Get ("id");
         Another_Int : Long_Integer;
      begin
         Another_Int := Random_Int + 1;
      end;
   end loop;
end;
```

All of the JSON data is pre-loaded as a string or stream before any timers are ran, so this is just testing the actual processing.

It times how long it takes to parse / load the JSON data, how long it takes to get to the "sixteen" key, and how long it takes to iterate the object.

## How to use it

If you want to build it, you can also clone this repository, `cd` to the directory, then build it with `alr update && alr build` in Alire.

After that, you must **unzip** the `large-file.zip` file to `large-file.json`.

To run, issue the following command from the project directory: `bin/json_benchmark large-file.json`

## My results

This was the results on my computer, which is an M2 MacBook Air with 16GB of ram.

```
Testing json-ada's JSON
json-ada read is  2.274622000 seconds
json-ada seek is  0.001402000 seconds
json-ada iter is  0.008116000 seconds
Testing GNATCOLL's JSON
GNATCOLL.JSON read is  5.622553000 seconds
GNATCOLL.JSON seek is  0.005264000 seconds
GNATCOLL.JSON iter is  0.004949000 seconds
Testing VSS's JSON
VSS.JSON read is  11.717198000 seconds
VSS.JSON seek is  31.041688000 seconds
VSS.JSON iter is  2.125321000 seconds
```

json-ada and GNATCOLL.JSON both use standard dictionary lookups, but VSS's json system is loads things into a stream that you have to manually parse afterwards, so it's more complicated, and there's probably a better way to do that section.  If someone wants to try and get the VSS.JSON one faster, please let me know!

## Contribute

If you would like to contribute, feel free.  Maybe you can get VSS to speed up with the sample data...