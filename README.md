### Xdebug trace file viewer ###

This is a simple application written in Delphi that can parse an xdebug trace file and display its contents in a tree view.

The parsed file has to be in the "computer readable format" ```xdebug.trace_format = 1``` (see [Xdebug documentation](http://www.xdebug.org/docs/all_settings#trace_format)).

At the moment, all it can do is parse the file, create an in-memory data structure and display a tree structure of function calls. Its parse speed is about 15 MBps on my old T61 laptop :)

There is a compiled win32 binary file present in the relase, however the source code will probably compile using FreePascal/Lazarus on other plaforms.
