# cl-gfx

cl-gfx is a graphics library written entirely in Common Lisp!

## Overview

cl-gfx depends on Quicklisp; it is bundled for the user's convenience.

It uses the MDL spec (included as MDL.spec) and generates images by reading MDL commands from script.mdl.

## Features

- Lines
- Circles, Bezier and Hermite curves (and any parametric curves custom-specified with the function #'generate-parametric-curve in shape.lisp)
- Boxes, Spheres, Tori (and any 2D parametric curves with #'generate-nested-parametric-curve in shape.lisp)
- PPM native support, live display/PNG/JPG/GIF/other formats available through ImageMagick
- Rendering of solids with triangles
- MDL parser with ql:cl-lex and ql:yacc (in Common Lisp, too!!)
- Scanline conversion
- Z-buffering
- Lighting, lighting, lighting! (Ambient, diffuse, specular reflection)
- Wireframe or flat shading
- Very very slow! (Thanks, Clisp.)

# Organization

    init.lisp       ; Initialization file, gets run by default with makefile
    packages.lisp   ; Initializes quicklisp and package information
    engine.lisp     ; Imports and initializes graphics engine
    run.lisp        ; Specifies actions to take on run (parses script.mdl by default)
    util.lisp       ; Contains utility functions, mostly vector math
    matrix.lisp     ; Nearly-self-contained matrix library, could be used independently
    shape.lisp      ; Calculates point matrices for different geometric curves/solids
    draw.lisp       ; Handles edge/triangle matrices, coordinate systems, lighting, lines
    display.lisp    ; Handles file writing/converting and plotting pixels
    mdl.lisp        ; Parses MDL scripts, makes initial passes, and converts commands to functions
    script.mdl      ; Uses MDL (Motion Description Language) to specify an image
    makefile        ; Specifies build instructions
    anim/           ; Contains temporary animation frames -- outputs go to root folder

# Where Next?

- Goroud and Phong shading
- Optimize, optimize, optimize (never ends)
- Import meshes

# Credits

cl-gfx was written by Kenneth Li for Computer Graphics at Stuyvesant High School in the spring of 2017.
