import Base.findin
import Base.insert!
import Base.push!

"""
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

This is all very well, not to say entirely appropriate, when you have a program that generates lots of figures. It provides the ultimate control.
But it is a very different story at the REPL. To get a pretty picture, you have to type seven commands; If you mis-type number seven, then you have
to start all over again. But condensing these seven operations into one simple command, e.g. 'Surf', would create the opposite problem. You would
have to have a ton of different definitions of 'Surf' for however you wanted your graph to look.

The plotOpStack idea brings a new approach to this situation. There are two different methods for Surf: One follows the MathGL API faithfully to enable full
control by you when you need it. The other returns a set of operations to be performed on an mglGraph, stored in an array along with descriptive names and boolean
switches to turn them on and off. Thus the user can type:
#At the beginning of the session, e.g. in the user's julia startup file
import Base.show
function show(io::IO, ops::plotOpStack)
	ShowImage(draw(ops), "FancyImageViewer"))
end

# At the REPL
dat = generate_some_data()
ops = Surf(dat) #Note: when the first parameter is not an mglGraph object, the method that returns a plotOpStack is called

Then the plot will be shown. If the user wishes to change some operation, he/she can easily disable things by setting elements of ops.switches to false
or call insert!(ops, someDrawingFunction, someIndex) too add the operation someDrawingFunction to the stack at position someIndex
"""
struct plotOpStack
    ops::Array{Function, 1}
    names::Array{String, 1}
    switches::Array{Bool, 1}
    plotOpStack() = new(Array{Function, 1}(0), Array{String, 1}(0), Array{Bool, 1}(0))
end

"""Push operation _op_ with name _name_ and enabled state _on_ onto the end of a plotOpStack"""
function push!(opStack::plotOpStack, op::Function, name::String="", on::Bool=true)
	push!(opStack.ops, op)
	push!(opStack.names, name)
	push!(opStack.switches, on)
end

"""Insert operation _op_ into the plotOpStack at index _index_ with name _name_ and enabled/disabled state given by _on_"""
function insert!(opStack::plotOpStack, index::Integer, op::Function, name::String="", on::Bool=true)
	insert!(opStack.ops, index, op)
	insert!(opStack.names, index, name)
	insert!(opStack.switches, index, on)
end

"""Return the index(es) of the operation(s) with name(s) `name` in the plotOpStack `opStack`"""
function findin(opStack::plotOpStack, name::String)
    return find(x->x==name, opStack.names)
end

"""Disable the operation at index(es) `indecies...` in the plotOpStack `opStack`"""
function disable!(opStack::plotOpStack, indecies::Integer...)
    for index in indecies
	opStack.switches[index] = false
    end
    return nothing
end

"""Disable the operation named `name` in the plotOpStack `opStack`, if it exists""" 
function disable!(opStack::plotOpStack, name::String)
    indecies = findin(opStack, name)
    if length(indecies) == 0
	warn("Cannot disable operation $name because it is not present in the plotOpStack")
    end
    disable!(opStack, indecies...)
end

"""Apply the functions in _opStack_ to _gr_ in order"""
function draw!(gr::mglGraph, opStack::plotOpStack)
    nOps = size(opStack.ops, 1)
    for i in 1:nOps
	if opStack.switches[i]
		opStack.ops[i](gr)
		if isinteractive()
#			println("Drawing ", opStack.names[i])
		end
	end
    end
end

"""Create a mglGraph and apply the operations in _opStack_ to it"""
function draw(opStack::plotOpStack, width::Integer=-1, height::Integer=-1)
	if width == -1
		width = default_width
	end
	if height == -1
		height = default_height
	end

	gr = mglGraph(width, height)
	draw!(gr, opStack)
	return gr
end


export push!
export findin
export insert!
export disable!


export plotOpStack


