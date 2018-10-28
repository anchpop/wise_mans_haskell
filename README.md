To build the book, enter `wise_mans_haskell\wise_mans_haskell\`  and run:


```
pp -html -import=formatting/pp-macros/all.pp book.md | pandoc --toc -o build/index.html -s --template=./formatting/templates/tufte.html5
```

You may need to create a `build/` folder and copy the `assets` folder into it. Requires `pandoc` and [pp](https://github.com/CDSoft/pp) to be on your path. 

To try to build it to a PDF (although it does not currently work) use:

```
pp -pdf -import=formatting/pp-macros/all.pp book.md | pandoc -o build/book.pdf -f markdown+raw_tex --pdf-engine=xelatex --template=./formatting/templates/tufte-handout.tex -V documentclass:tufte-handout
```


The macros we have access to are:




```
!newthought(This is a new thought)

!sidenote(This is a sidenote)

!marginnote(This is a margin note)

!figure(This is a figure caption)(./image/path/image.jpg)

!marginfigure(This is a margin figure caption)(./image/path/image.jpg)

!fullwidthfigure(This is a full-width figure caption)(./image/path/image.jpg)
```

To build the test formatter in HTML, enter `wise_mans_haskell\wise_mans_haskell\formatting` and run:

```
pp -html -import=pp-macros/all.pp sample-handout.md | pandoc -o sample-handout.html -s --template=./templates/tufte.html5
```

To build the test formatter in PDF, enter that same directory and run:

```
pp -pdf -import=pp-macros/all.pp sample-handout.md | pandoc -o sample-handout.pdf -f markdown+raw_tex --pdf-engine=xelatex --template=./templates/tufte-handout.tex -V documentclass:tufte-handout
```

Note that this creates some junk folders that should not be checked in.