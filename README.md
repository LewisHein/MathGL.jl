# MathGL

MathGL [http://mathgl.sourceforge.net] is a world-class open source graphics library that handles 1D, 2D, and 3D data arrays, with support for a total of over 50 different types of plots. As the following excerpt from the documentation shows, it also has many other non-trivial features, such as:

 - Arbitrary curvillinear coordinate systems, 
 - Several types of transparency and smoothed lighting
 - Vector fonts and parsing of $\tex$ symbols
 - Vector format (such as SVG and EPS) export available for plots
 - Animations
 - Widgets to embed in windowing toolkits, including WX, FLTK, and Qt


MathGL also brings several key paradigms to its job that enable absolute control over the final plot. In the words of MathGL's own  documentation:

>There are six most general (base) concepts:

> 1. Any picture is created in memory first. The internal (memory) representation can be different: bitmap picture (for SetQuality(MGL_DRAW_LMEM)) or the list of vector primitives (default). After that the user may decide what he/she want: save to file, display on the screen, run animation, do additional editing and so on. This approach assures a high portability of the program – the source code will produce exactly the same picture in any OS. Another big positive consequence is the ability to create the picture in the console program (using command line, without creating a window)!
> 2. Every plot settings (style of lines, font, color scheme) are specified by a string. It provides convenience for user/programmer – short string with parameters is more comprehensible than a large set of parameters. Also it provides portability – the strings are the same in any OS so that it is not necessary to think about argument types.
> 3. All functions have “simplified” and “advanced” forms. It is done for user’s convenience. One needs to specify only one data array in the “simplified” form in order to see the result. But one may set parametric dependence of coordinates and produce rather complex curves and surfaces in the “advanced” form. In both cases the order of function arguments is the same: first data arrays, second the string with style, and later string with options for additional plot tuning.
> 4. All data arrays for plotting are encapsulated in mglData(A) class. This reduces the number of errors while working with memory and provides a uniform interface for data of different types (mreal, double and so on) or for formula plotting.
> 5. All plots are vector plots. The MathGL library is intended for handling scientific data which have vector nature (lines, faces, matrices and so on). As a result, vector representation is used in all cases! In addition, the vector representation allows one to scale the plot easily – change the canvas size by a factor of 2, and the picture will be proportionally scaled.
> 6. New drawing never clears things drawn already. This, in some sense, unexpected, idea allows to create a lot of “combined” graphics. For example, to make a surface with contour lines one needs to call the function for surface plotting and the function for contour lines plotting (in any order). Thus the special functions for making this “combined” plots (as it is done in Matlab and some other plotting systems) are superfluous.

(Note: since julia already provides functionality similar to #4, this package uses julia's native array type)

This package provides a wrapper around an existing MathGL installation and adds new features such as the plotOpStack that allows the user to build an array of drawing functions, name them, turn them on and off at will, and draw the result to a pretty picture. See the comments in MathGL.jl for more information on this.

MathGL already has great documentation; this package faithfully follows the MathGL C++ API, with one exeption to make it more Julia-esque: In MathGL, each function is a method on an mglGraph object, so that you might write C++ code like
```{.cpp}
    gr = mglGraph()
    gr.Plot(stuff)
```

In julia, the thing.method() syntax is not supported (with good reason); thus the above code would become
```{.jl}
    gr = mglGraph()
    Plot(gr, stuff)
```

For further examples and information, refer to the MathGL documentation and examples. They should all work if translated according to the above rule.
