Web application to help tracking down duplicate files.

Build:

```
mill proj.dist
```

Generate `hashes.txt` (execute the command one level above FOLDER):
```
find FOLDER -type f -exec md5sum {} \; > hashes.txt 2>hashes.err
```

Generate file list:
```
java -cp dist/out.jar com.gjuhasz86.dupfinder.backend.core.Preparation /path/to/FOLDER
```

Run web server
```
java -cp out.jar com.gjuhasz86.dupfinder.backend.server.WebServer public
```