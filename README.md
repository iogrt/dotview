I plan to include a bunch of tools and functionality for my personal uses of RDF.


For now I have `dotview-fs` which allows you to mount a userspace filesystem which has a file that represents note files as a rdf graph. It is always updated each time you read it

Usage: 
```
mkdir mydir
dotview-fs mydir
```

to unmount:
```
fusermount -u mydir
```

Paths are still hardcoded to my home directory, sorry!