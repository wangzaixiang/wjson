# wJson

## usage

see [Usage](doc/wjson.md)

## Build
1. generate the pattern parser
   ```bash
   # first install the bison parser generator
   # brew install bison 
   cd bison
   bison -o ../src/main/java/wjson/parse/Program.java Program
   ```
2. sbt compile

