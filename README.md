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

There is one noteworthy feature that MathGL doesn't have: the concept of a plot operation stack, or plotOpStack for short.

Part of the general awesomeness of MathGL for scientific graphics is that it never does anything that it is not explicitly told to.
This allows for extreme flexibility in how your graphics look, but it also tends to create code that looks like this:

dat = generate_some_data()
gr = mglGraph()
SetRange(gr, 'x', size(dat, 1))
SetRange(gr, 'y', size(dat, 2))
SetRange(gr, 'z', minimum(dat), maximum(dat))
Surf(gr, data)
Axis(gr)
Box(gr)

rather than like this:

dat = generate_some_data()
Surf(gr, dat)

This is all very well, not to say entirely appropriate, when you have a program that generates lots of super-customized figures. It provides the ultimate control.
But it is a very different story at the REPL. To get a pretty picture, you have to type seven commands; If you mis-type number seven, then you have
to start all over again. But condensing these seven operations into one simple command, e.g. 'Surf', would create the opposite problem. You would
have to have a ton of different definitions of 'Surf' for however you wanted your graph to look.

The plotOpStack idea brings a new approach to this situation. There are two different methods for Surf: One follows the MathGL API faithfully to enable full
control by you when you need it. The other returns a set of operations to be performed on an mglGraph, stored in an array along with descriptive names and boolean
switches to turn them on and off. Thus, you can type:

```{.jl}
    # (At the beginning of the session, e.g. in the user's julia startup file)
    import Base.show
    function show(io::IO, ops::plotOpStack)
	ShowImage(draw(ops), "YourFavouriteImageViewer"))
    end

    # At the REPL
    dat = generate_some_data()
    ops = Surf(dat) #Note: when the first parameter is not an mglGraph object, the method that returns a plotOpStack is called
```
Then the plot will be shown. If you want to change some operation, you can easily disable it by calling disable! with the name of the
operation (in the names element of the plotOpStack) and/or it's index in the stack.
Conversely insert!(ops, someDrawingFunction, someIndex) will add someDrawingFunction to the stack at position someIndex.

Thus, you can create graphics that look great by default, but still have total control with a minimum of typing.


## Installation

### Installing the MathGL library

This library is only a wrapper around the MathGL library; Thus, you must have MathGL installed on your system before it will work, and libmgl2.so (or I assume libmgl2.dll for Windows users) must be present in your runtime shared library path.

#### Linux:
MathGL can probably be installed via your favourite Linux distro's repository. These versions may be way out of date; for instance the version for ubuntu is 2 years and several releases behind. I personally recommend downloading and compiling it yourself; it's not hard to do. (Someday, I hope to have this done automatically with BinDeps.)

#### Mac OS:
MathGL is availaible via hombrew

#### Windows:
There is a binary installer available at http://mathgl.sourceforge.net.

See the MathGL homepage [mathgl.sourceforge.net] for details.

### Installing the MathGL Julia package
This is as simple as 
```{.jl}
    Pkg.clone("https://github.com/LewisHein/MathGL.jl")
```
