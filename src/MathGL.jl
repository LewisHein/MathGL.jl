module MathGL
import Base.push!
import Base.insert!
import Base.unsafe_convert

"""A thin wrapper around a pointer to an mglGraph that allows for a finalizer to free the graph"""
type graphPointer
	graph::Ptr{Void}
end

"""Convert a graphPointer to a void pointer so that it can be used transparently in ccall()"""
unsafe_convert(::Type{Ptr{Void}}, grPtr::graphPointer) = grPtr.graph

"""Frees an mglGraph pointer"""
function freeMglGraph(grPtr::graphPointer)
    ccall((:mgl_delete_graph, "libmgl2"), Void, (Ptr{Void},), grPtr)
end

"""A wrapper type around MathGL's mglGraph type"""
type mglGraph
    width::Int
    height::Int
    graph::graphPointer

    function mglGraph(width::Int, height::Int)
        width = width
        height = height
        graph=graphPointer(ccall((:mgl_create_graph, "libmgl2"), Ptr{Void}, (Int64, Int64), width, height))
	finalizer(graph, freeMglGraph)
        return new(width, height, graph)
    end

    mglGraph() = mglGraph(600, 400)

end

"""Convert an mglGraph to a Ptr{Void} so that it works seamlessly with ccall()"""
unsafe_convert(::Type{Ptr{Void}}, gr::mglGraph) = gr.graph.graph

"""Similarly to the case for mglGraph, a thin wrapper around a pointer to an mglData object"""
type dataPointer
	data::Ptr{Void}
end

"""convert a dataPointer object to Ptr{Void} so that it can be used transparently in ccall"""
unsafe_convert(::Type{Ptr{Void}}, dataPtr::dataPointer) = dataPtr.data

"""Frees an mglData pointer"""
function freeMglData(data::dataPointer)
	ccall((:mgl_delete_data, "libmgl2"), Void, (Ptr{Void},), data.data)
end

typealias mreal Cdouble
"""A wrapper around the MathGL mglData type"""
type mglData
    nx::Int
    ny::Int
    nz::Int
    data::dataPointer

    function mglData(nx::Integer, ny::Integer=1, nz::Integer=1)
        nx = nx
        ny = ny
        nz = nz
        data = dataPointer(ccall((:mgl_create_data_size, "libmgl2"), Ptr{Void}, (Int, Int, Int), nx, ny, nz))
	finalizer(data, freeMglData)
        new(nx, ny, nz, data)
    end

    function mglData(other::mglData)
        new(other.nx, other.ny, other.nz, other.data)
    end

    function mglData(a::AbstractArray)
        @assert ndims(a) < 4 "a must have dimension at most 3"
        nx = ndims(a) > 1 ? size(a)[2] : size(a)[1]
        ny = ndims(a) > 1 ? size(a)[1] : 1
        nz = ndims(a) > 2 ? size(a)[3] : 1
        data = dataPointer(ccall((:mgl_create_data_size, "libmgl2"), Ptr{Void}, (Cint, Cint, Cint), nx, ny, nz))
	finalizer(data, freeMglData)

        if ndims(a) > 1
            for k::Int in 1:nz
                for i in 1:nx
                    for j in 1:ny
                        ccall((:mgl_data_set_value, "libmgl2"), Void, (Ptr{Void}, mreal, Cint, Cint, Cint), data, a[j,i,k], i-1, j-1, k-1)
                    end
                end
            end
        else
            for i in 1:nx
                ccall((:mgl_data_set_value, "libmgl2"), Void, (Ptr{Void}, mreal, Cint, Cint, Cint), data, a[i], i-1, 0, 0)
            end
        end
        new(nx, ny, nz, data)
    end

end

"""Convert an mglData object to a Ptr{Void} so that it can be used transparently in ccall"""
unsafe_convert(::Type{Ptr{Void}}, data::mglData) = data.data.data

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
type plotOpStack
    ops::Array{Function, 1}
    names::Array{ASCIIString, 1}
    switches::Array{Bool, 1}
    plotOpStack() = new(Array{Function, 1}(0), Array{ASCIIString, 1}(0), Array{Bool, 1}(0))
end

"""Push operation _op_ with name _name_ and enabled state _on_ onto the end of a plotOpStack"""
function push!(opStack::plotOpStack, op::Function, name::ASCIIString="", on::Bool=true)
	push!(opStack.ops, op)
	push!(opStack.names, name)
	push!(opStack.switches, on)
end

"""Insert operation _op_ into the plotOpStack at index _index_ with name _name_ and enabled/disabled state given by _on_"""
function insert!(opStack::plotOpStack, index::Integer, op::Function, name::ASCIIString="", on::Bool=true)
	insert!(opStack.ops, index, op)
	insert!(opStack.names, index, name)
	insert!(opStack.switches, index, on)
end

export push!
export insert!


export plotOpStack

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
function draw(opStack::plotOpStack, width::Integer=600, height::Integer=400)
	gr = mglGraph(width, height)
	draw!(gr, opStack)
	return gr
end

typealias mglDataA mglData

type mglPoint
    x::mreal
    y::mreal
    z::mreal
end

type mglFormula
    mglFormula() = error("mglFormula is not implemented")
end
typealias HMEX mglFormula

type mglFormulaC
    mglFormula() = error("mglFormulaC is not implemented")
end
typealias HAEX mglFormula

type mglDataC
    mglDataC() = error("mglDataC is not implemented")
end
typealias HADT mglDataC
type dual
    dual() = error("dual is not implemented")
end

function plot(gr::mglGraph, dat::Array, pen::ASCIIString, opt::ASCIIString)
    ccall((:mgl_plot, "libmgl2"), Void, (Ptr{Void}, Ptr{Void}, Ptr{UInt8}, Ptr{UInt8}), gr, dat.data, pen, opt)
end

mglNaN = NaN
function stop(gr::mglGraph, stop::Bool=true)

    	ccall((:mgl_ask_stop,"libmgl2"), Void, (Ptr{Void},Bool), gr, stop)
end

#function setEventFunc(gr::mglGraph, void (*func)(void *), void *par=NULL)

#    	ccall((:mgl_set_event_func,"libmgl2"), Void, (Ptr{Void},void,void), gr, func, par)
#end

function alpha(gr::mglGraph, enable::Bool)

    	ccall((:mgl_set_alpha,"libmgl2"), Void, (Ptr{Void},Bool), gr, enable)
end

function setAlphaDef(gr::mglGraph, alpha::Number)

    	ccall((:mgl_set_alpha_default,"libmgl2"), Void, (Ptr{Void},Cdouble), gr, alpha)
end

function setTranspType(gr::mglGraph, transpType::Cint)

    	ccall((:mgl_set_transp_type,"libmgl2"), Void, (Ptr{Void},Cint), gr, transpType)
end

function light(gr::mglGraph, enable::Bool)

    	ccall((:mgl_set_light,"libmgl2"), Void, (Ptr{Void},Bool), gr, enable)
end

function light(gr::mglGraph, n::Cint, enable::Bool)

    	ccall((:mgl_set_light_n,"libmgl2"), Void, (Ptr{Void},Cint,Bool), gr, n, enable)
end

function setDifLight(gr::mglGraph, dif::Bool)

    	ccall((:mgl_set_light_dif,"libmgl2"), Void, (Ptr{Void},Bool), gr, dif)
end

function attachLight(gr::mglGraph, enable::Bool)

    	ccall((:mgl_set_attach_light,"libmgl2"), Void, (Ptr{Void},Bool), gr, enable)
end

function addLight(gr::mglGraph, n::Cint, p::mglPoint, col::Char='w', bright::Number=0.5, ap::Number=0.0)

    	ccall((:mgl_add_light_ext,"libmgl2"), Void, (Ptr{Void},Cint,mreal,mreal,mreal,Cchar,Cdouble,Cdouble), gr, n, p.x, p.y, p.z, col, bright, ap)
end

function addLight(gr::mglGraph, n::Cint, r::mglPoint, p::mglPoint, col::Char='w', bright::Number=0.5, ap::Number=0.0)

    	ccall((:mgl_add_light_loc,"libmgl2"), Void, (Ptr{Void},Cint,mreal,mreal,mreal,mreal,mreal,mreal,Cchar,Cdouble,Cdouble), gr, n, r.x, r.y, r.z, p.x, p.y, p.z, col, bright, ap)
end

function setAmbient(gr::mglGraph, i::Number)

    	ccall((:mgl_set_ambbr,"libmgl2"), Void, (Ptr{Void},Cdouble), gr, i)
end

function setDiffuse(gr::mglGraph, i::Number)

    	ccall((:mgl_set_difbr,"libmgl2"), Void, (Ptr{Void},Cdouble), gr, i)
end

function fog(gr::mglGraph, d::Number, dz::Number=0.25)

    	ccall((:mgl_set_fog,"libmgl2"), Void, (Ptr{Void},Cdouble,Cdouble), gr, d, dz)
end

function setBarWidth(gr::mglGraph, width::Number)

    	ccall((:mgl_set_bar_width,"libmgl2"), Void, (Ptr{Void},Cdouble), gr, width)
end

function setMarkSize(gr::mglGraph, size::Number)

    	ccall((:mgl_set_mark_size,"libmgl2"), Void, (Ptr{Void},Cdouble), gr, size)
end

function setArrowSize(gr::mglGraph, size::Number)

    	ccall((:mgl_set_arrow_size,"libmgl2"), Void, (Ptr{Void},Cdouble), gr, size)
end

function setMeshNum(gr::mglGraph, num::Cint)

    	ccall((:mgl_set_meshnum,"libmgl2"), Void, (Ptr{Void},Cint), gr, num)
end

function setFaceNum(gr::mglGraph, num::Cint)

    	ccall((:mgl_set_facenum,"libmgl2"), Void, (Ptr{Void},Cint), gr, num)
end

function setCut(gr::mglGraph, cut::Bool)

    	ccall((:mgl_set_cut,"libmgl2"), Void, (Ptr{Void},Bool), gr, cut)
end

function setCutBox(gr::mglGraph, p1::mglPoint, p2::mglPoint)

    	ccall((:mgl_set_cut_box,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,mreal,mreal,mreal), gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z)
end

function cutOff(gr::mglGraph,EqC::ASCIIString)

    	ccall((:mgl_set_cutoff,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar}), gr,pointer("$EqC\0".data))
end

function setFontSize(gr::mglGraph, size::Number)

    	ccall((:mgl_set_font_size,"libmgl2"), Void, (Ptr{Void},Cdouble), gr, size)
end

function setFontDef(gr::mglGraph,fnt::ASCIIString)

    	ccall((:mgl_set_font_def,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar}), gr,pointer("$fnt\0".data))
end

function loadFont(gr::mglGraph,name::ASCIIString,path::ASCIIString=NULL)

    	ccall((:mgl_load_font,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$name\0".data),pointer("$path\0".data))
end

function copyFont(gr::mglGraph, GR::mglGraph)

    	ccall((:mgl_copy_font,"libmgl2"), Void, (Ptr{Void},Ptr{Void}), gr, GR.gr)
end

function setRotatedText(gr::mglGraph, rotated::Bool)

    	ccall((:mgl_set_rotated_text,"libmgl2"), Void, (Ptr{Void},Bool), gr, rotated)
end

function setPalette(gr::mglGraph,colors::ASCIIString)

    	ccall((:mgl_set_palette,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar}), gr,pointer("$colors\0".data))
end

function setDefScheme(gr::mglGraph,sch::ASCIIString)

    	ccall((:mgl_set_def_sch,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar}), gr,pointer("$sch\0".data))
end

function setMaskAngle(gr::mglGraph, angle::Cint)

    	ccall((:mgl_set_mask_angle,"libmgl2"), Void, (Ptr{Void},Cint), gr, angle)
end

function zoomAxis(gr::mglGraph, p1::mglPoint=mglPoint(0,0,0,0), p2::mglPoint=mglPoint(1,1,1,1))

    	ccall((:mgl_zoom_axis,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,mreal), gr, p1.x,p1.y,p1.z,p1.c, p2.x,p2.y,p2.z,p2.c)
end

function addRange(gr::mglGraph, dir::Char, v1::Number, v2::Number)

    	ccall((:mgl_add_range_val,"libmgl2"), Void, (Ptr{Void},Cchar,Cdouble,Cdouble), gr, dir, v1, v2)
end

function setRange(gr::mglGraph, dir::Char, v1::Number, v2::Number)

    	ccall((:mgl_set_range_val,"libmgl2"), Void, (Ptr{Void},Cchar,Cdouble,Cdouble), gr, dir, v1, v2)
end

function setRange(gr::mglGraph, dir::Char, dat::Array, add::Bool=false)
  datDat = mglData(dat)

     	ccall((:mgl_set_range_val,"libmgl2"), Void, (Ptr{Void},Cchar,Ptr{Void},Cint), gr, dir,  datDat.data, add)
end

function setRanges(gr::mglGraph, x1::Number, x2::Number, y1::Number, y2::Number, z1::Number=0.0, z2::Number=0.0)

    	ccall((:mgl_set_ranges,"libmgl2"), Void, (Ptr{Void},Cdouble,Cdouble,Cdouble,Cdouble,Cdouble,Cdouble), gr, x1, x2, y1, y2, z1, z2)
end

function setRanges(gr::mglGraph, p1::mglPoint, p2::mglPoint)

    	ccall((:mgl_set_range_dat,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,mreal,mreal,mreal), gr,'x',xx,0);	mgl_set_range_dat(gr.graph,'y',yy,0)
end

function setAutoRanges(gr::mglGraph, x1::Number, x2::Number, y1::Number=0.0, y2::Number=0.0, z1::Number=0.0, z2::Number=0.0, c1::Number=0.0, c2::Number=0.0)

    	ccall((:mgl_set_auto_ranges,"libmgl2"), Void, (Ptr{Void},Cdouble,Cdouble,Cdouble,Cdouble,Cdouble,Cdouble,Cdouble,Cdouble), gr, x1, x2, y1, y2, z1, z2, c1, c2)
end

function setAutoRanges(gr::mglGraph, p1::mglPoint, p2::mglPoint)

    	ccall((:mgl_set_auto_ranges,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,mreal,mreal,mreal), gr, p1.x, p2.x, p1.y, p2.y, p1.z, p2.z, p1.c, p2.c)
end

function setOrigin(gr::mglGraph, p::mglPoint)

    	ccall((:mgl_set_origin,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal), gr, p.x, p.y, p.z)
end

function setOrigin(gr::mglGraph, x0::Number, y0::Number, z0::Number=mglNaN)

    	ccall((:mgl_set_origin,"libmgl2"), Void, (Ptr{Void},Cdouble,Cdouble,Cdouble), gr, x0, y0, z0)
end

function setFunc(gr::mglGraph,EqX::ASCIIString,EqY::ASCIIString,EqZ::ASCIIString=NULL,EqA::ASCIIString=NULL)

    	ccall((:mgl_set_func,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$EqX\0".data),pointer("$EqY\0".data),pointer("$EqZ\0".data),pointer("$EqA\0".data))
end

function setCoor(gr::mglGraph, how::Cint)

    	ccall((:mgl_set_coor,"libmgl2"), Void, (Ptr{Void},Cint), gr, how)
end

function ternary(gr::mglGraph, val::Cint)

    	ccall((:mgl_set_ternary,"libmgl2"), Void, (Ptr{Void},Cint), gr, val)
end

function setTickLen(gr::mglGraph, len::Number, stt::Number=1.0)

    	ccall((:mgl_set_tick_len,"libmgl2"), Void, (Ptr{Void},Cdouble,Cdouble), gr, len, stt)
end

function setAxisStl(gr::mglGraph,stl::ASCIIString="k",tck::ASCIIString=0,sub::ASCIIString=0)

    	ccall((:mgl_set_axis_stl,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$stl\0".data),pointer("$tck\0".data),pointer("$sub\0".data))
end

function setTicks(gr::mglGraph, dir::Char, d::Number=0.0, ns::Cint=0, org::Number=mglNaN,factor::ASCIIString="")

    	ccall((:mgl_set_ticks_fact,"libmgl2"), Void, (Ptr{Void},Cchar,Cdouble,Cint,Cdouble,Ptr{Cchar}), gr, dir, d, ns, org,pointer("$factor\0".data))
end

function setTicks(gr::mglGraph, dir::Char, d::Number, ns::Cint, org::Number, factor::UTF8String)

    	ccall((:mgl_set_ticks_fact,"libmgl2"), Void, (Ptr{Void},Cchar,Cdouble,Cint,Cdouble,Ptr{Cwchar_t}), gr, dir, d, ns, org, pointer("$factor\0".data))
end

function adjust(gr::mglGraph,dir::ASCIIString="xyzc")

    	ccall((:mgl_adjust_ticks,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar}), gr,pointer("$dir\0".data))
end

function setTuneTicks(gr::mglGraph, tune::Cint, fact_pos::Number=1.15)

    	ccall((:mgl_tune_ticks,"libmgl2"), Void, (Ptr{Void},Cint,Cdouble), gr, tune, fact_pos)
end

function subPlot(gr::mglGraph, nx::Cint, ny::Cint,m::Cint,style::ASCIIString="<>_^", dx::Number=0.0, dy::Number=0.0)

    	ccall((:mgl_subplot_d,"libmgl2"), Void, (Ptr{Void},Cint,Cint,Cint,Ptr{Cchar},Cdouble,Cdouble), gr, nx, ny, m, style, dx, dy)
end

function multiPlot(gr::mglGraph, nx::Cint, ny::Cint,m::Cint, dx::Cint, dy::Cint,style::ASCIIString="<>_^")

    	ccall((:mgl_multiplot,"libmgl2"), Void, (Ptr{Void},Cint,Cint,Cint,Cint,Cint,Ptr{Cchar}), gr, nx, ny, m, dx, dy,pointer("$style\0".data))
end

function aspect(gr::mglGraph, Ax::Number, Ay::Number,Az::Number=1.0)

    	ccall((:mgl_aspect,"libmgl2"), Void, (Ptr{Void},Cdouble,Cdouble,Cdouble), gr, Ax, Ay, Az)
end

function rotate(gr::mglGraph, TetX::Number, TetZ::Number=0.0,TetY::Number=0.0)

    	ccall((:mgl_rotate,"libmgl2"), Void, (Ptr{Void},Cdouble,Cdouble,Cdouble), gr, TetX, TetZ, TetY)
end

function rotateN(gr::mglGraph, Tet::Number, x::Number,y::Number,z::Number)

    	ccall((:mgl_rotate_vector,"libmgl2"), Void, (Ptr{Void},Cdouble,Cdouble,Cdouble,Cdouble), gr, Tet, x, y, z)
end

function perspective(gr::mglGraph, val::Number)

    	ccall((:mgl_perspective,"libmgl2"), Void, (Ptr{Void},Cdouble), gr, val)
end

function view(gr::mglGraph, TetX::Number, TetZ::Number=0.0,TetY::Number=0.0)

    	ccall((:mgl_view,"libmgl2"), Void, (Ptr{Void},Cdouble,Cdouble,Cdouble), gr, TetX, TetZ, TetY)
end

function viewAsRotate(gr::mglGraph, TetZ::Number, TetX::Number,TetY::Number=0.0)

    	ccall((:mgl_view,"libmgl2"), Void, (Ptr{Void},Cdouble,Cdouble,Cdouble), gr, -TetX, -TetZ, -TetY)
end

function zoom(gr::mglGraph, x1::Number, y1::Number, x2::Number, y2::Number)

    error("Zoom is not implemented")
#    	/// Zoom in/out a part of ccall((:picture,"libmgl2"), Void, (Ptr{Void},Cdouble,Cdouble,Cdouble,Cdouble), use Zoom(0, 0, 1, 1) for restore default)
end

function setSize(gr::mglGraph, width::Cint, height::Cint)

    	ccall((:mgl_set_size,"libmgl2"), Void, (Ptr{Void},Cint,Cint), gr, width, height)
end

function setQuality(gr::mglGraph, qual::Cint=MGL_DRAW_NORM)

    	ccall((:mgl_set_quality,"libmgl2"), Void, (Ptr{Void},Cint), gr, qual)
end

function startGroup(gr::mglGraph,name::ASCIIString)

    	ccall((:mgl_start_group,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar}), gr,pointer("$name\0".data))
end

function highlight(gr::mglGraph, id::Cint)

    	ccall((:mgl_highlight,"libmgl2"), Void, (Ptr{Void},Cint), gr, id)
end

function showImage(gr::mglGraph,viewer::ASCIIString, keep::Bool=false)

    	ccall((:mgl_show_image,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Bool), gr,pointer("$viewer\0".data), keep)
end

function writeFrame(gr::mglGraph,fname::ASCIIString=0,descr::ASCIIString="")

    	ccall((:mgl_write_frame,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0".data),pointer("$descr\0".data))
end

function writeJPEG(gr::mglGraph,fname::ASCIIString,descr::ASCIIString="")

    	ccall((:mgl_write_jpg,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0".data),pointer("$descr\0".data))
end

function writeBMP(gr::mglGraph,fname::ASCIIString,descr::ASCIIString="")

    	ccall((:mgl_write_bmp,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0".data),pointer("$descr\0".data))
end

function writeTGA(gr::mglGraph,fname::ASCIIString,descr::ASCIIString="")

    	ccall((:mgl_write_tga,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0".data),pointer("$descr\0".data))
end

function writeEPS(gr::mglGraph,fname::ASCIIString,descr::ASCIIString="")

    	ccall((:mgl_write_eps,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0".data),pointer("$descr\0".data))
end

function writeTEX(gr::mglGraph,fname::ASCIIString,descr::ASCIIString="")

    	ccall((:mgl_write_tex,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0".data),pointer("$descr\0".data))
end

function writeBPS(gr::mglGraph,fname::ASCIIString,descr::ASCIIString="")

    	ccall((:mgl_write_bps,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0".data),pointer("$descr\0".data))
end

function writeSVG(gr::mglGraph,fname::ASCIIString,descr::ASCIIString="")

    	ccall((:mgl_write_svg,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0".data),pointer("$descr\0".data))
end

function writeGIF(gr::mglGraph,fname::ASCIIString,descr::ASCIIString="")

    	ccall((:mgl_write_gif,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0".data),pointer("$descr\0".data))
end

function writeOBJ(gr::mglGraph,fname::ASCIIString,descr::ASCIIString="",use_png::Bool=true)

    	ccall((:mgl_write_obj,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar},Bool), gr,pointer("$fname\0".data),pointer("$descr\0".data), use_png)
end

function writeOBJold(gr::mglGraph,fname::ASCIIString,descr::ASCIIString="",use_png::Bool=true)

    	ccall((:mgl_write_obj_old,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar},Bool), gr,pointer("$fname\0".data),pointer("$descr\0".data), use_png)
end

function writeXYZ(gr::mglGraph,fname::ASCIIString,descr::ASCIIString="")

    	ccall((:mgl_write_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0".data),pointer("$descr\0".data))
end

function writeSTL(gr::mglGraph,fname::ASCIIString,descr::ASCIIString="")

    	ccall((:mgl_write_stl,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0".data),pointer("$descr\0".data))
end

function writeOFF(gr::mglGraph,fname::ASCIIString,descr::ASCIIString="", colored::Bool=false)

    	ccall((:mgl_write_off,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar},Bool), gr,pointer("$fname\0".data),pointer("$descr\0".data),colored)
end

function writePRC(gr::mglGraph,fname::ASCIIString,descr::ASCIIString="",make_pdf::Bool=true)

    	ccall((:mgl_write_prc,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar},Bool), gr,pointer("$fname\0".data),pointer("$descr\0".data), make_pdf)
end

function delFrame(gr::mglGraph, i::Cint)

    	ccall((:mgl_del_frame,"libmgl2"), Void, (Ptr{Void},Cint), gr, i)
end

function getFrame(gr::mglGraph, i::Cint)

    	ccall((:mgl_get_frame,"libmgl2"), Void, (Ptr{Void},Cint), gr, i)
end

function setFrame(gr::mglGraph, i::Cint)

    	ccall((:mgl_set_frame,"libmgl2"), Void, (Ptr{Void},Cint), gr, i)
end

function showFrame(gr::mglGraph, i::Cint)

    	ccall((:mgl_show_frame,"libmgl2"), Void, (Ptr{Void},Cint), gr, i)
end

function startGIF(gr::mglGraph,fname::ASCIIString, ms::Cint=100)

    	ccall((:mgl_start_gif,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Cint), gr,pointer("$fname\0".data),ms)
end

function exportMGLD(gr::mglGraph,fname::ASCIIString,descr::ASCIIString="")

    	ccall((:mgl_export_mgld,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0".data),pointer("$descr\0".data))
end

function importMGLD(gr::mglGraph,fname::ASCIIString, add::Bool=false)

    	ccall((:mgl_import_mgld,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Bool), gr,pointer("$fname\0".data), add)
end

function clf(gr::mglGraph, r::Number, g::Number, b::Number)

    	ccall((:mgl_clf_rgb,"libmgl2"), Void, (Ptr{Void},Cdouble,Cdouble,Cdouble), gr, r, g, b)
end

function clf(gr::mglGraph,col::ASCIIString)

    	ccall((:mgl_clf_str,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar}), gr,pointer("$col\0".data))
end

function clf(gr::mglGraph, col::Char)

    	ccall((:mgl_clf_str,"libmgl2"), Void, (Ptr{Void},Cchar), gr, col)
end

function mark(gr::mglGraph, p::mglPoint,mark::ASCIIString)

    	ccall((:mgl_mark,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,Ptr{Cchar}), gr, p.x, p.y, p.z,pointer("$mark\0".data))
end

function line(gr::mglGraph, p1::mglPoint, p2::mglPoint, pen::ASCIIString="B", n::Int=2)

    	ccall((:mgl_line,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,mreal,mreal,mreal,Ptr{Cchar},Cint), gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z,pointer("$pen\0".data), n)
end

function curve(gr::mglGraph, p1::mglPoint, d1::mglPoint, p2::mglPoint, d2::mglPoint,pen::ASCIIString="B", n::Int=100)

    	ccall((:mgl_curve,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,Ptr{Cchar},Cint), gr, p1.x, p1.y, p1.z, d1.x, d1.y, d1.z, p2.x, p2.y, p2.z, d2.x, d2.y, d2.z,pointer("$pen\0".data), n)
end

function error(gr::mglGraph, p::mglPoint, e::mglPoint,pen::ASCIIString="k")

    	ccall((:mgl_error_box,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,mreal,mreal,mreal,Ptr{Cchar}), gr, p.x, p.y, p.z, e.x, e.y, e.z,pointer("$pen\0".data))
end

function face(gr::mglGraph, p1::mglPoint, p2::mglPoint, p3::mglPoint, p4::mglPoint,stl::ASCIIString="r")

    	ccall((:mgl_face,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,Ptr{Cchar}), gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z, p3.x, p3.y, p3.z, p4.x, p4.y, p4.z,pointer("$stl\0".data))
end

function faceX(gr::mglGraph, p::mglPoint, wy::Number, wz::Number,stl::ASCIIString="w", dx::Number=0.0, dy::Number=0.0)

    	ccall((:mgl_facex,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,Cdouble,Cdouble,Ptr{Cchar},Cdouble,Cdouble), gr, p.x, p.y, p.z, wy, wz,pointer("$stl\0".data), dx, dy)
end

function faceY(gr::mglGraph, p::mglPoint, wx::Number, wz::Number,stl::ASCIIString="w", dx::Number=0.0, dy::Number=0.0)

    	ccall((:mgl_facey,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,Cdouble,Cdouble,Ptr{Cchar},Cdouble,Cdouble), gr, p.x, p.y, p.z, wx, wz,pointer("$stl\0".data), dx, dy)
end

function faceZ(gr::mglGraph, p::mglPoint, wx::Number, wy::Number,stl::ASCIIString="w", dx::Number=0.0, dy::Number=0.0)

    	ccall((:mgl_facez,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,Cdouble,Cdouble,Ptr{Cchar},Cdouble,Cdouble), gr, p.x, p.y, p.z, wx, wy,pointer("$stl\0".data), dx, dy)
end

function drop(gr::mglGraph, p::mglPoint, d::mglPoint, r::Number,col::ASCIIString="r", shift::Number=1.0, ap::Number=1.0)

    	ccall((:mgl_drop,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,mreal,mreal,mreal,Cdouble,Ptr{Cchar},Cdouble,Cdouble), gr, p.x, p.y, p.z, d.x, d.y, d.z, r,pointer("$col\0".data), shift, ap)
end

function sphere(gr::mglGraph, p::mglPoint, r::Number,col::ASCIIString="r")

    	ccall((:mgl_sphere,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,Cdouble,Ptr{Cchar}), gr, p.x, p.y, p.z, r,pointer("$col\0".data))
end

function cone(gr::mglGraph, p1::mglPoint, p2::mglPoint, r1::Number, r2::Number=-1.0,stl::ASCIIString="r@")

    	ccall((:mgl_cone,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,mreal,mreal,mreal,Cdouble,Cdouble,Ptr{Cchar}), gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z,r1,r2,pointer("$stl\0".data))
end

function ellipse(gr::mglGraph, p1::mglPoint, p2::mglPoint, r::Number,stl::ASCIIString="r")

    	ccall((:mgl_ellipse,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,mreal,mreal,mreal,Cdouble,Ptr{Cchar}), gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z, r,pointer("$stl\0".data))
end

function circle(gr::mglGraph, p::mglPoint, r::Number,stl::ASCIIString="r")

    	ccall((:mgl_ellipse,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,Cdouble,Ptr{Cchar}), gr, p.x, p.y, p.z, p.x, p.y, p.z, r,pointer("$stl\0".data))
end

function rhomb(gr::mglGraph, p1::mglPoint, p2::mglPoint, r::Number,stl::ASCIIString="r")

    	ccall((:mgl_rhomb,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,mreal,mreal,mreal,Cdouble,Ptr{Cchar}), gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z, r,pointer("$stl\0".data))
end

function polygon(gr::mglGraph, p1::mglPoint, p2::mglPoint, n::Cint,stl::ASCIIString="r")

    	ccall((:mgl_polygon,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,mreal,mreal,mreal,Cint,Ptr{Cchar}), gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z, n,pointer("$stl\0".data))
end

function arc(gr::mglGraph, p0::mglPoint, pr::mglPoint, p1::mglPoint, a::Number,stl::ASCIIString="r")

    	ccall((:mgl_arc_ext,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,Cdouble,Ptr{Cchar}), gr, p0.x,p0.y,p0.z, pr.x,pr.y,pr.z, p1.x,p1.y,p1.z, a,pointer("$stl\0".data))
end

function arc(gr::mglGraph, p0::mglPoint, p1::mglPoint, a::Number,stl::ASCIIString="r")

    	ccall((:mgl_arc_ext,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,mreal,mreal,mreal,Cdouble,Ptr{Cchar}), gr, p0.x,p0.y,p0.z, 0,0,1, p1.x,p1.y,p0.z, a,pointer("$stl\0".data))
end

function logo(gr::mglGraph, w::Clong, h::Clong, rgba::ASCIIString, smooth::Bool=false,opt::ASCIIString="")

    	ccall((:mgl_logo,"libmgl2"), Void, (Ptr{Void},Clong,Clong,Ptr{Cchar},Bool,Ptr{Cchar}), gr, w, h, rgba, smooth,pointer("$opt\0".data))
end

function logo(gr::mglGraph,fname::ASCIIString, smooth::Bool=false,opt::ASCIIString="")

    	ccall((:mgl_logo_file,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Bool,Ptr{Cchar}), gr,pointer("$fname\0".data), smooth,pointer("$opt\0".data))
end

function putsw(gr::mglGraph, p::mglPoint, text::UTF8String,font::ASCIIString=":C",size::Number=-1.0)

    	ccall((:mgl_putsw,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,Ptr{Cwchar_t},Ptr{Cchar},Cdouble), gr, p.x, p.y, p.z, pointer("$text\0".data), font, size)
end

function puts(gr::mglGraph, p::mglPoint,text::ASCIIString,font::ASCIIString=":C",size::Number=-1.0)

    	ccall((:mgl_puts,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,Ptr{Cchar},Ptr{Cchar},Cdouble), gr, p.x, p.y, p.z,pointer("$text\0".data), font, size)
end

function putsw(gr::mglGraph, x::Number, y::Number, text::UTF8String,font::ASCIIString=":AC",size::Number=-1.0)

	ccall((:mgl_putsw,"libmgl2"), Void, (Ptr{Void},Cdouble,Cdouble,Ptr{Cwchar_t},Ptr{Cchar},Cdouble), gr, x, y, 0, pointer("$text\0"), font, size)
end

function puts(gr::mglGraph, x::Number, y::Number,text::ASCIIString,font::ASCIIString=":AC",size::Number=-1.0)

    	ccall((:mgl_puts,"libmgl2"), Void, (Ptr{Void},Cdouble,Cdouble,Ptr{Cchar},Ptr{Cchar},Cdouble), gr, x, y, 0,pointer("$text\0".data), font, size)
end

function putsw(gr::mglGraph, p::mglPoint, d::mglPoint, text::UTF8String,font::ASCIIString=":L", size::Number=-1.0)

    	ccall((:mgl_putsw,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,mreal,mreal,mreal,Ptr{Cwchar_t},Ptr{Cchar},Cdouble), gr, x, y, 0, pointer("$text\0".data),pointer("$font\0".data), size)
end

function puts(gr::mglGraph, p::mglPoint, d::mglPoint,text::ASCIIString,font::ASCIIString=":L", size::Number=-1.0)

    	ccall((:mgl_puts,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,mreal,mreal,mreal,Ptr{Cchar},Ptr{Cchar},Cdouble), gr, x, y, 0,pointer("$text\0".data),pointer("$font\0".data), size)
end

function text(gr::mglGraph,x::Array,y::Array,z::Array,text::ASCIIString,font::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_text_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$text\0".data),pointer("$font\0".data),pointer("$opt\0".data))
end

function text(gr::mglGraph,x::Array,y::Array,text::ASCIIString,font::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_text_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data,pointer("$text\0".data),pointer("$font\0".data),pointer("$opt\0".data))
end

function text(gr::mglGraph,y::Array,text::ASCIIString,font::ASCIIString="",opt::ASCIIString="")
  yDat = mglData(y)

    	ccall((:mgl_text_y,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data,pointer("$text\0".data),pointer("$font\0".data),pointer("$opt\0".data))
end

function text(gr::mglGraph,x::Array,y::Array,z::Array, text::UTF8String,font::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_text_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, pointer("$text\0".data),pointer("$font\0".data),pointer("$opt\0".data))
end

function text(gr::mglGraph,x::Array,y::Array, text::UTF8String,font::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_text_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, pointer("$text\0".data),pointer("$font\0".data),pointer("$opt\0".data))
end

function text(gr::mglGraph,y::Array, text::UTF8String,font::ASCIIString="",opt::ASCIIString="")
  yDat = mglData(y)

    	ccall((:mgl_text_y,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, pointer("$text\0".data),pointer("$font\0".data),pointer("$opt\0".data))
end

function box(gr::mglGraph,col::ASCIIString="", ticks::Bool=true)

    	ccall((:mgl_box_str,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Bool), gr,pointer("$col\0".data), ticks)
end

function axis(gr::mglGraph,dir::ASCIIString="xyzt",stl::ASCIIString="",opt::ASCIIString="")

    	ccall((:mgl_axis,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$dir\0".data),pointer("$stl\0".data),pointer("$opt\0".data))
end

function grid(gr::mglGraph,dir::ASCIIString="xyzt",pen::ASCIIString="B",opt::ASCIIString="")

    	ccall((:mgl_axis_grid,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$dir\0".data),pointer("$pen\0".data),pointer("$opt\0".data))
end

function label(gr::mglGraph, dir::Char,text::ASCIIString, pos::Number=+1.0,opt::ASCIIString="")

    	ccall((:mgl_label,"libmgl2"), Void, (Ptr{Void},Cchar,Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, dir,pointer("$text\0".data), pos,pointer("$opt\0".data))
end

function label(gr::mglGraph, dir::Char, text::UTF8String, pos::Number=+1.0,opt::ASCIIString="")

    	ccall((:mgl_label,"libmgl2"), Void, (Ptr{Void},Cchar,Ptr{Cwchar_t},Cdouble,Ptr{Cchar}), gr, dir, pointer("$text\0".data), pos,pointer("$opt\0".data))
end

function colorbar(gr::mglGraph,sch::ASCIIString="")

    	ccall((:mgl_colorbar,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar}), gr,pointer("$sch\0".data))
end

function colorbar(gr::mglGraph,sch::ASCIIString, x::Number,y::Number,w::Number=1.0,h::Number=1.0)

    	ccall((:mgl_colorbar_ext,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Cdouble,Cdouble,Cdouble,Cdouble), gr,pointer("$sch\0".data), x,y,w,h)
end

function colorbar(gr::mglGraph,val::Array,sch::ASCIIString="")
  valDat = mglData(val)

    	ccall((:mgl_colorbar_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar}), gr, valDat.data,pointer("$sch\0".data))
end

function colorbar(gr::mglGraph,val::Array,sch::ASCIIString, x::Number,y::Number,w::Number=1.0,h::Number=1.0)
  valDat = mglData(val)

    	ccall((:mgl_colorbar_val_ext,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Cdouble,Cdouble,Cdouble), gr, valDat.data,pointer("$sch\0".data), x,y,w,h)
end

function addLegend(gr::mglGraph,text::ASCIIString,style::ASCIIString)

    	ccall((:mgl_add_legend,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$text\0".data),pointer("$style\0".data))
end

function addLegend(gr::mglGraph, text::UTF8String,style::ASCIIString)

    	ccall((:mgl_add_legend,"libmgl2"), Void, (Ptr{Void},Ptr{Cwchar_t},Ptr{Cchar}), gr, pointer("$text\0".data),pointer("$style\0".data))
end

function legend(gr::mglGraph, x::Number, y::Number,font::ASCIIString="#",opt::ASCIIString="")

    	ccall((:mgl_legend_pos,"libmgl2"), Void, (Ptr{Void},Cdouble,Cdouble,Ptr{Cchar},Ptr{Cchar}), gr, x, y,pointer("$font\0".data),pointer("$opt\0".data))
end

function legend(gr::mglGraph, where::Cint=3,font::ASCIIString="#",opt::ASCIIString="")

    	ccall((:mgl_legend,"libmgl2"), Void, (Ptr{Void},Cint,Ptr{Cchar},Ptr{Cchar}), gr, where,pointer("$font\0".data),pointer("$opt\0".data))
end

function setLegendMarks(gr::mglGraph, num::Cint)

    	ccall((:mgl_set_legend_marks,"libmgl2"), Void, (Ptr{Void},Cint), gr, num)
end

function plot(gr::mglGraph,x::Array,y::Array,z::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_plot_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function plot(gr::mglGraph,x::Array,y::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_plot_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function plot(y::Array, pen::ASCIIString="", opt::ASCIIString="")
        opStack=plotOpStack()
        push!(opStack, gr->SetRange(gr, 'y', minimum(y), maximum(y)), "Set ranges")
        push!(opStack, gr->Box(gr), "Box")
        push!(opStack, gr->Axis(gr), "Axes")
        push!(opStack, gr->Plot(gr, y, pen, opt), "Plot")
        return opStack
end


function plot(gr::mglGraph,y::Array,pen::ASCIIString="",opt::ASCIIString="")

  yDat = mglData(y)
    	ccall((:mgl_plot,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr,yDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
    	
end

function tape(gr::mglGraph,x::Array,y::Array,z::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_tape_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function tape(gr::mglGraph,x::Array,y::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_tape_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function tape(gr::mglGraph,y::Array,pen::ASCIIString="",opt::ASCIIString="")
  yDat = mglData(y)

    	ccall((:mgl_tape,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function radar(gr::mglGraph,a::Array,pen::ASCIIString="",opt::ASCIIString="")
  aDat = mglData(a)

    	ccall((:mgl_radar,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, aDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function step(gr::mglGraph,x::Array,y::Array,z::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_step_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function step(gr::mglGraph,x::Array,y::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_step_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function step(gr::mglGraph,y::Array,pen::ASCIIString="",opt::ASCIIString="")
  yDat = mglData(y)

    	ccall((:mgl_step,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function tens(gr::mglGraph,x::Array,y::Array,z::Array,c::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  cDat = mglData(c)

    	ccall((:mgl_tens_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, cDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function tens(gr::mglGraph,x::Array,y::Array,c::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  cDat = mglData(c)

    	ccall((:mgl_tens_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, cDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function tens(gr::mglGraph,y::Array,c::Array,pen::ASCIIString="",opt::ASCIIString="")
  yDat = mglData(y)
  cDat = mglData(c)

    	ccall((:mgl_tens,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, cDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function area(gr::mglGraph,x::Array,y::Array,z::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_area_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function area(gr::mglGraph,x::Array,y::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_area_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function area(gr::mglGraph,y::Array,pen::ASCIIString="",opt::ASCIIString="")
  yDat = mglData(y)

    	ccall((:mgl_area,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function region(gr::mglGraph,y1::Array,y2::Array,pen::ASCIIString="",opt::ASCIIString="")
  y1Dat = mglData(y1)
  y2Dat = mglData(y2)

    	ccall((:mgl_region,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, y1Dat.data, y2Dat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function region(gr::mglGraph,x::Array,y1::Array,y2::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  y1Dat = mglData(y1)
  y2Dat = mglData(y2)

    	ccall((:mgl_region_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, y1Dat.data, y2Dat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function region(gr::mglGraph,x1::Array,y1::Array,z1::Array,x2::Array,y2::Array,z2::Array,pen::ASCIIString="",opt::ASCIIString="")
  x1Dat = mglData(x1)
  y1Dat = mglData(y1)
  z1Dat = mglData(z1)
  x2Dat = mglData(x2)
  y2Dat = mglData(y2)
  z2Dat = mglData(z2)

    	mgl_region_3ccall((:d,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, x1Dat.data, y1Dat.data, z1Dat.data, x2Dat.data, y2Dat.data, z2Dat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function region(gr::mglGraph,x1::Array,y1::Array,x2::Array,y2::Array,pen::ASCIIString="",opt::ASCIIString="")
  x1Dat = mglData(x1)
  y1Dat = mglData(y1)
  x2Dat = mglData(x2)
  y2Dat = mglData(y2)

    	mgl_region_3ccall((:d,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, x1Dat.data, y1Dat.data, NULL, x2Dat.data, y2Dat.data, NULL,pointer("$pen\0".data),pointer("$opt\0".data))
end

function stem(gr::mglGraph,x::Array,y::Array,z::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_stem_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function stem(gr::mglGraph,x::Array,y::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_stem_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function stem(gr::mglGraph,y::Array,pen::ASCIIString="",opt::ASCIIString="")
  yDat = mglData(y)

    	ccall((:mgl_stem,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function bars(gr::mglGraph,x::Array,y::Array,z::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_bars_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function bars(gr::mglGraph,x::Array,y::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_bars_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function bars(gr::mglGraph,y::Array,pen::ASCIIString="",opt::ASCIIString="")
  yDat = mglData(y)

    	ccall((:mgl_bars,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function barh(gr::mglGraph,y::Array,v::Array,pen::ASCIIString="",opt::ASCIIString="")
  yDat = mglData(y)
  vDat = mglData(v)

    	ccall((:mgl_barh_yx,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, vDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function barh(gr::mglGraph,v::Array,pen::ASCIIString="",opt::ASCIIString="")
  vDat = mglData(v)

    	ccall((:mgl_barh,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function chart(gr::mglGraph,a::Array,colors::ASCIIString="",opt::ASCIIString="")
  aDat = mglData(a)

    	ccall((:mgl_Cchart,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, aDat.data,pointer("$colors\0".data),pointer("$opt\0".data))
end

function oHLC(gr::mglGraph,x::Array,open::Array,high::Array,low::Array,close::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  openDat = mglData(open)
  highDat = mglData(high)
  lowDat = mglData(low)
  closeDat = mglData(close)

    	ccall((:mgl_ohlc_x,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, openDat.data, highDat.data, lowDat.data, closeDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function oHLC(gr::mglGraph,open::Array,high::Array,low::Array,close::Array,pen::ASCIIString="",opt::ASCIIString="")
  openDat = mglData(open)
  highDat = mglData(high)
  lowDat = mglData(low)
  closeDat = mglData(close)

    	ccall((:mgl_ohlc,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, openDat.data, highDat.data, lowDat.data, closeDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function boxPlot(gr::mglGraph,x::Array,y::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_boxplot_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function boxPlot(gr::mglGraph,y::Array,pen::ASCIIString="",opt::ASCIIString="")
  yDat = mglData(y)

    	ccall((:mgl_boxplot,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function candle(gr::mglGraph,x::Array,v1::Array,v2::Array,y1::Array,y2::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  v1Dat = mglData(v1)
  v2Dat = mglData(v2)
  y1Dat = mglData(y1)
  y2Dat = mglData(y2)

    	ccall((:mgl_candle_xyv,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, v1Dat.data, v2Dat.data, y1Dat.data, y2Dat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function candle(gr::mglGraph,v1::Array,v2::Array,y1::Array,y2::Array,pen::ASCIIString="",opt::ASCIIString="")
  v1Dat = mglData(v1)
  v2Dat = mglData(v2)
  y1Dat = mglData(y1)
  y2Dat = mglData(y2)

    	ccall((:mgl_candle_yv,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, v1Dat.data, v2Dat.data, y1Dat.data, y2Dat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function candle(gr::mglGraph,v1::Array,v2::Array,pen::ASCIIString="",opt::ASCIIString="")
  v1Dat = mglData(v1)
  v2Dat = mglData(v2)

    	ccall((:mgl_candle_yv,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, v1Dat.data, v2Dat.data, NULL, NULL,pointer("$pen\0".data),pointer("$opt\0".data))
end

function candle(gr::mglGraph,y::Array,y1::Array,y2::Array,pen::ASCIIString="",opt::ASCIIString="")
  yDat = mglData(y)
  y1Dat = mglData(y1)
  y2Dat = mglData(y2)

    	ccall((:mgl_candle,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, y1Dat.data, y2Dat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function candle(gr::mglGraph,y::Array,pen::ASCIIString="",opt::ASCIIString="")
  yDat = mglData(y)

    	ccall((:mgl_candle,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, NULL, NULL,pointer("$pen\0".data),pointer("$opt\0".data))
end

function cones(gr::mglGraph,x::Array,y::Array,z::Array,pen::ASCIIString="@",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_cones_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function cones(gr::mglGraph,x::Array,z::Array,pen::ASCIIString="@",opt::ASCIIString="")
  xDat = mglData(x)
  zDat = mglData(z)

    	ccall((:mgl_cones_xz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, zDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function cones(gr::mglGraph,z::Array,pen::ASCIIString="@",opt::ASCIIString="")
  zDat = mglData(z)

    	ccall((:mgl_cones,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function error(gr::mglGraph,y::Array,ey::Array,pen::ASCIIString="",opt::ASCIIString="")
  yDat = mglData(y)
  eyDat = mglData(ey)

    	ccall((:mgl_error,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, eyDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function error(gr::mglGraph,x::Array,y::Array,ey::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  eyDat = mglData(ey)

    	ccall((:mgl_error_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, eyDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function error(gr::mglGraph,x::Array,y::Array,ex::Array,ey::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  exDat = mglData(ex)
  eyDat = mglData(ey)

    	ccall((:mgl_error_exy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, exDat.data, eyDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function mark(gr::mglGraph,x::Array,y::Array,z::Array,r::Array,pen::ASCIIString,opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  rDat = mglData(r)

    	ccall((:mgl_mark_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, rDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function mark(gr::mglGraph,x::Array,y::Array,r::Array,pen::ASCIIString,opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  rDat = mglData(r)

    	ccall((:mgl_mark_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, rDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function mark(gr::mglGraph,y::Array,r::Array,pen::ASCIIString,opt::ASCIIString="")
  yDat = mglData(y)
  rDat = mglData(r)

    	ccall((:mgl_mark_y,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, rDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function textMark(gr::mglGraph,x::Array,y::Array,z::Array,r::Array,text::ASCIIString,fnt::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  rDat = mglData(r)

    	ccall((:mgl_textmark_xyzr,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, rDat.data,pointer("$text\0".data),pointer("$fnt\0".data),pointer("$opt\0".data))
end

function textMark(gr::mglGraph,x::Array,y::Array,r::Array,text::ASCIIString,fnt::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  rDat = mglData(r)

    	ccall((:mgl_textmark_xyr,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, rDat.data,pointer("$text\0".data),pointer("$fnt\0".data),pointer("$opt\0".data))
end

function textMark(gr::mglGraph,y::Array,r::Array,text::ASCIIString,fnt::ASCIIString="",opt::ASCIIString="")
  yDat = mglData(y)
  rDat = mglData(r)

    	ccall((:mgl_textmark_yr,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, rDat.data,pointer("$text\0".data),pointer("$fnt\0".data),pointer("$opt\0".data))
end

function textMark(gr::mglGraph,y::Array,text::ASCIIString,fnt::ASCIIString="",opt::ASCIIString="")
  yDat = mglData(y)

    	ccall((:mgl_textmark,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data,pointer("$text\0".data),pointer("$fnt\0".data),pointer("$opt\0".data))
end

function textMark(gr::mglGraph,x::Array,y::Array,z::Array,r::Array, text::UTF8String,fnt::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  rDat = mglData(r)

    	ccall((:mgl_textmark_xyzr,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, rDat.data, pointer("$text\0".data),pointer("$fnt\0".data),pointer("$opt\0".data))
end

function textMark(gr::mglGraph,x::Array,y::Array,r::Array, text::UTF8String,fnt::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  rDat = mglData(r)

    	ccall((:mgl_textmark_xyr,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, rDat.data, pointer("$text\0".data),pointer("$fnt\0".data),pointer("$opt\0".data))
end

function textMark(gr::mglGraph,y::Array,r::Array, text::UTF8String,fnt::ASCIIString="",opt::ASCIIString="")
  yDat = mglData(y)
  rDat = mglData(r)

    	ccall((:mgl_textmark_yr,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, rDat.data, pointer("$text\0".data),pointer("$fnt\0".data),pointer("$opt\0".data))
end

function textMark(gr::mglGraph,y::Array, text::UTF8String,fnt::ASCIIString="",opt::ASCIIString="")
  yDat = mglData(y)

    	ccall((:mgl_textmark,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, pointer("$text\0".data),pointer("$fnt\0".data),pointer("$opt\0".data))
end

function label(gr::mglGraph,x::Array,y::Array,z::Array,text::ASCIIString,fnt::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_label_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$text\0".data),pointer("$fnt\0".data),pointer("$opt\0".data))
end

function label(gr::mglGraph,x::Array,y::Array,text::ASCIIString,fnt::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_label_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data,pointer("$text\0".data),pointer("$fnt\0".data),pointer("$opt\0".data))
end

function label(gr::mglGraph, y::Array,text::ASCIIString, fnt::ASCIIString="",opt::ASCIIString="")

    	ccall((:mgl_label,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr, dir,pointer("$text\0".data), pos,pointer("$opt\0".data))
end

function label(gr::mglGraph,x::Array,y::Array,z::Array, text::UTF8String,fnt::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_label_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, pointer("$text\0".data),pointer("$fnt\0".data),pointer("$opt\0".data))
end

function label(gr::mglGraph,x::Array,y::Array, text::UTF8String,fnt::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_label_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, pointer("$text\0".data),pointer("$fnt\0".data),pointer("$opt\0".data))
end

function label(gr::mglGraph, y::Array, text::UTF8String, fnt::ASCIIString="",opt::ASCIIString="")

    	ccall((:mgl_label,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, dir, pointer("$text\0".data), pos,pointer("$opt\0".data))
end

function table(gr::mglGraph,val::Array,text::ASCIIString,fnt::ASCIIString="#|",opt::ASCIIString="")
  valDat = mglData(val)

    	ccall((:mgl_table,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr, 0, 0, valDat.data,pointer("$text\0".data),pointer("$fnt\0".data),pointer("$opt\0".data))
end

function table(gr::mglGraph,val::Array, text::UTF8String,fnt::ASCIIString="#|",opt::ASCIIString="")
  valDat = mglData(val)

    	ccall((:mgl_table,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, 0, 0, valDat.data, pointer("$text\0".data),pointer("$fnt\0".data),pointer("$opt\0".data))
end

function table(gr::mglGraph, x::Number, y::Number,val::Array,text::ASCIIString,fnt::ASCIIString="#|",opt::ASCIIString="")
  valDat = mglData(val)

    	ccall((:mgl_table,"libmgl2"), Void, (Ptr{Void},Cdouble,Cdouble,Ptr{Void},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr, x, y, valDat.data,pointer("$text\0".data),pointer("$fnt\0".data),pointer("$opt\0".data))
end

function table(gr::mglGraph, x::Number, y::Number,val::Array, text::UTF8String,fnt::ASCIIString="#|",opt::ASCIIString="")
  valDat = mglData(val)

    	ccall((:mgl_table,"libmgl2"), Void, (Ptr{Void},Cdouble,Cdouble,Ptr{Void},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, x, y, valDat.data, pointer("$text\0".data),pointer("$fnt\0".data),pointer("$opt\0".data))
end

function tube(gr::mglGraph,x::Array,y::Array,z::Array,r::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  rDat = mglData(r)

    	ccall((:mgl_tube_xyzr,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, rDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function tube(gr::mglGraph,x::Array,y::Array,z::Array, r::Number,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_tube_xyzr,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Cdouble,Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, r,pointer("$pen\0".data),pointer("$opt\0".data))
end

function tube(gr::mglGraph,x::Array,y::Array,r::Array,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  rDat = mglData(r)

    	ccall((:mgl_tube_xyr,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, rDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function tube(gr::mglGraph,x::Array,y::Array, r::Number,pen::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_tube_xyr,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Cdouble,Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, r,pointer("$pen\0".data),pointer("$opt\0".data))
end

function tube(gr::mglGraph,y::Array,r::Array,pen::ASCIIString="",opt::ASCIIString="")
  yDat = mglData(y)
  rDat = mglData(r)

    	ccall((:mgl_tube_r,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, rDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function tube(gr::mglGraph,y::Array, r::Number,pen::ASCIIString="",opt::ASCIIString="")
  yDat = mglData(y)

    	ccall((:mgl_tube_r,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Cdouble,Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, r,pointer("$pen\0".data),pointer("$opt\0".data))
end

function torus(gr::mglGraph,r::Array,z::Array,pen::ASCIIString="",opt::ASCIIString="")
  rDat = mglData(r)
  zDat = mglData(z)

    	ccall((:mgl_torus,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, rDat.data, zDat.data,pointer("$pen\0".data),pointer("$opt\0".data))
end

function mesh(gr::mglGraph,x::Array,y::Array,z::Array,stl::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_mesh_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function mesh(gr::mglGraph,z::Array,stl::ASCIIString="",opt::ASCIIString="")
  zDat = mglData(z)

    	ccall((:mgl_mesh,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function fall(gr::mglGraph,x::Array,y::Array,z::Array,stl::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_fall_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function fall(gr::mglGraph,z::Array,stl::ASCIIString="",opt::ASCIIString="")
  zDat = mglData(z)

    	ccall((:mgl_fall,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function belt(gr::mglGraph,x::Array,y::Array,z::Array,stl::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_belt_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function belt(gr::mglGraph,z::Array,stl::ASCIIString="",opt::ASCIIString="")
  zDat = mglData(z)

    	ccall((:mgl_belt,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function surf(gr::mglGraph,x::Array,y::Array,z::Array,stl::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_surf_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function surf(z::Array{mreal, 2}, stl::ASCIIString="", opt::ASCIIString="")
        opStack=plotOpStack()
        push!(opStack, gr->SetRange(gr, 'z', minimum(z), maximum(z)), "Set z range")
	push!(opStack, gr->SetRange(gr, 'x', 0, size(z, 2)), "Set x range")
	push!(opStack, gr->SetRange(gr, 'y', 0, size(z, 1)), "Set y range")
#        push!(opStack, gr->Rotate(gr, 50, 60))
        push!(opStack, gr->Box(gr), "Draw box")
        push!(opStack, gr->Axis(gr), "Axis")
        push!(opStack, gr->Surf(gr, z, stl, opt), "Surface")
        return opStack
end

function surf(gr::mglGraph,z::Array,stl::ASCIIString="",opt::ASCIIString="")
        data=mglData(z)
    	ccall((:mgl_surf,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, data.data, pointer("$stl\0".data),pointer("$opt\0".data))
    	
end

function grid(gr::mglGraph,x::Array,y::Array,z::Array,stl::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_grid_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function grid(gr::mglGraph, z::Array, stl::ASCIIString="",opt::ASCIIString="")

    	ccall((:mgl_axis_grid,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, dir, pen,pointer("$opt\0".data))
end

function tile(gr::mglGraph,x::Array,y::Array,z::Array,stl::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_tile_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function tile(gr::mglGraph,z::Array,stl::ASCIIString="",opt::ASCIIString="")
  zDat = mglData(z)

    	ccall((:mgl_tile,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function dens(gr::mglGraph,x::Array,y::Array,c::Array,stl::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  cDat = mglData(c)

    	ccall((:mgl_dens_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, cDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function dens(gr::mglGraph,c::Array,stl::ASCIIString="",opt::ASCIIString="")
  cDat = mglData(c)

    	ccall((:mgl_dens,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, cDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function dens(c::Array, stl::ASCIIString="", opt::ASCIIString="")
	opStack = plotOpStack()
	push!(opStack, gr->SetRange(gr, 'c', minimum(c), maximum(c)))
	push!(opStack, gr->SetRange(gr, 'x', 0, size(c)[2]))
	push!(opStack, gr->SetRange(gr, 'y', 0, size(c)[1]))
	push!(opStack, gr->Box(gr))
	push!(opStack, gr->Axis(gr))
	push!(opStack, gr->Dens(gr, c, stl, opt))
	
	return opStack
end

function boxs(gr::mglGraph,x::Array,y::Array,z::Array,stl::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_boxs_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function boxs(gr::mglGraph,z::Array,stl::ASCIIString="",opt::ASCIIString="")
  zDat = mglData(z)

    	ccall((:mgl_boxs,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function cont(gr::mglGraph,v::Array,x::Array,y::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  vDat = mglData(v)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_cont_xy_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, xDat.data, yDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function cont(gr::mglGraph,v::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  vDat = mglData(v)
  zDat = mglData(z)

    	ccall((:mgl_cont_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function cont(gr::mglGraph,x::Array,y::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_cont_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function cont(gr::mglGraph,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  zDat = mglData(z)

    	ccall((:mgl_cont,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function contF(gr::mglGraph,v::Array,x::Array,y::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  vDat = mglData(v)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_contf_xy_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, xDat.data, yDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function contF(gr::mglGraph,v::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  vDat = mglData(v)
  zDat = mglData(z)

    	ccall((:mgl_contf_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function contF(gr::mglGraph,x::Array,y::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_contf_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function contF(gr::mglGraph,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  zDat = mglData(z)

    	ccall((:mgl_contf,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function contD(gr::mglGraph,v::Array,x::Array,y::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  vDat = mglData(v)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_contd_xy_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, xDat.data, yDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function contD(gr::mglGraph,v::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  vDat = mglData(v)
  zDat = mglData(z)

    	ccall((:mgl_contd_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function contD(gr::mglGraph,x::Array,y::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_contd_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function contD(gr::mglGraph,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  zDat = mglData(z)

    	ccall((:mgl_contd,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function contV(gr::mglGraph,v::Array,x::Array,y::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  vDat = mglData(v)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_contv_xy_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, xDat.data, yDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function contV(gr::mglGraph,v::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  vDat = mglData(v)
  zDat = mglData(z)

    	ccall((:mgl_contv_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function contV(gr::mglGraph,x::Array,y::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_contv_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function contV(gr::mglGraph,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  zDat = mglData(z)

    	ccall((:mgl_contv,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function axial(gr::mglGraph,v::Array,x::Array,y::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  vDat = mglData(v)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_axial_xy_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, xDat.data, yDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function axial(gr::mglGraph,v::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  vDat = mglData(v)
  zDat = mglData(z)

    	ccall((:mgl_axial_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function axial(gr::mglGraph,x::Array,y::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_axial_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function axial(gr::mglGraph,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  zDat = mglData(z)

    	ccall((:mgl_axial,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function grid3(gr::mglGraph,x::Array,y::Array,z::Array,a::Array,stl::ASCIIString="", sVal::Number=-1.0,opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	mgl_grid3ccall((:_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function grid3(gr::mglGraph,a::Array,stl::ASCIIString="", sVal::Number=-1.0,opt::ASCIIString="")
  aDat = mglData(a)

    	mgl_grid3ccall((:,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function dens3(gr::mglGraph,x::Array,y::Array,z::Array,a::Array,stl::ASCIIString="", sVal::Number=-1.0,opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	mgl_dens3ccall((:_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function dens3(gr::mglGraph,a::Array,stl::ASCIIString="", sVal::Number=-1.0,opt::ASCIIString="")
  aDat = mglData(a)

    	mgl_dens3ccall((:,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function surf3(gr::mglGraph, Val::Number,x::Array,y::Array,z::Array,a::Array,stl::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	mgl_surf3ccall((:_xyz_val,"libmgl2"), Void, (Ptr{Void},Cdouble,Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, Val, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function surf3(gr::mglGraph, Val::Number,a::Array,stl::ASCIIString="",opt::ASCIIString="")
  aDat = mglData(a)

    	mgl_surf3ccall((:_val,"libmgl2"), Void, (Ptr{Void},Cdouble,Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, Val, aDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function surf3(gr::mglGraph,x::Array,y::Array,z::Array,a::Array,stl::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	mgl_surf3ccall((:_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function surf3(gr::mglGraph,a::Array,stl::ASCIIString="",opt::ASCIIString="")
  aDat = mglData(a)

    	mgl_surf3ccall((:,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, aDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function cloud(gr::mglGraph,x::Array,y::Array,z::Array,a::Array,stl::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	ccall((:mgl_cloud_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function cloud(gr::mglGraph,a::Array,stl::ASCIIString="",opt::ASCIIString="")
  aDat = mglData(a)

    	ccall((:mgl_cloud,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, aDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function cont3(gr::mglGraph,v::Array,x::Array,y::Array,z::Array,a::Array,sch::ASCIIString="", sVal::Number=-1.0,opt::ASCIIString="")
  vDat = mglData(v)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	mgl_cont3ccall((:_xyz_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, vDat.data, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$sch\0".data), sVal,pointer("$opt\0".data))
end

function cont3(gr::mglGraph,v::Array,a::Array,sch::ASCIIString="", sVal::Number=-1.0,opt::ASCIIString="")
  vDat = mglData(v)
  aDat = mglData(a)

    	mgl_cont3ccall((:_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, vDat.data, aDat.data,pointer("$sch\0".data), sVal,pointer("$opt\0".data))
end

function cont3(gr::mglGraph,x::Array,y::Array,z::Array,a::Array,sch::ASCIIString="", sVal::Number=-1.0,opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	mgl_cont3ccall((:_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$sch\0".data), sVal,pointer("$opt\0".data))
end

function cont3(gr::mglGraph,a::Array,sch::ASCIIString="", sVal::Number=-1.0,opt::ASCIIString="")
  aDat = mglData(a)

    	mgl_cont3ccall((:,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$sch\0".data), sVal,pointer("$opt\0".data))
end

function contF3(gr::mglGraph,v::Array,x::Array,y::Array,z::Array,a::Array,sch::ASCIIString="", sVal::Number=-1.0,opt::ASCIIString="")
  vDat = mglData(v)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	mgl_contf3ccall((:_xyz_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, vDat.data, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$sch\0".data), sVal,pointer("$opt\0".data))
end

function contF3(gr::mglGraph,v::Array,a::Array,sch::ASCIIString="", sVal::Number=-1.0,opt::ASCIIString="")
  vDat = mglData(v)
  aDat = mglData(a)

    	mgl_contf3ccall((:_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, vDat.data, aDat.data,pointer("$sch\0".data), sVal,pointer("$opt\0".data))
end

function contF3(gr::mglGraph,x::Array,y::Array,z::Array,a::Array,sch::ASCIIString="", sVal::Number=-1.0,opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	mgl_contf3ccall((:_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$sch\0".data), sVal,pointer("$opt\0".data))
end

function contF3(gr::mglGraph,a::Array,sch::ASCIIString="", sVal::Number=-1.0,opt::ASCIIString="")
  aDat = mglData(a)

    	mgl_contf3ccall((:,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$sch\0".data), sVal,pointer("$opt\0".data))
end

function beam(gr::mglGraph,tr::Array,g1::Array,g2::Array,a::Array, r::Number,stl::ASCIIString=0, flag::Cint=0, num::Cint=3)
  trDat = mglData(tr)
  g1Dat = mglData(g1)
  g2Dat = mglData(g2)
  aDat = mglData(a)

    	ccall((:mgl_beam,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Cdouble,Ptr{Cchar},Cint,Cint), gr, trDat.data, g1Dat.data, g2Dat.data, aDat.data,r,pointer("$stl\0".data),flag,num)
end

function tileS(gr::mglGraph,x::Array,y::Array,z::Array,r::Array,stl::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  rDat = mglData(r)

    	ccall((:mgl_tiles_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, rDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function tileS(gr::mglGraph,z::Array,r::Array,stl::ASCIIString="",opt::ASCIIString="")
  zDat = mglData(z)
  rDat = mglData(r)

    	ccall((:mgl_tiles,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data, rDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function surfC(gr::mglGraph,x::Array,y::Array,z::Array,c::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  cDat = mglData(c)

    	ccall((:mgl_surfc_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, cDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function surfC(gr::mglGraph,z::Array,c::Array,sch::ASCIIString="",opt::ASCIIString="")
  zDat = mglData(z)
  cDat = mglData(c)

    	ccall((:mgl_surfc,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data, cDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function surfA(gr::mglGraph,x::Array,y::Array,z::Array,c::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  cDat = mglData(c)

    	ccall((:mgl_surfa_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, cDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function surfA(gr::mglGraph,z::Array,c::Array,sch::ASCIIString="",opt::ASCIIString="")
  zDat = mglData(z)
  cDat = mglData(c)

    	ccall((:mgl_surfa,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data, cDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function surfCA(gr::mglGraph,x::Array,y::Array,z::Array,c::Array,a::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  cDat = mglData(c)
  aDat = mglData(a)

    	ccall((:mgl_surfca_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, cDat.data, aDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function surfCA(gr::mglGraph,z::Array,c::Array,a::Array,sch::ASCIIString="",opt::ASCIIString="")
  zDat = mglData(z)
  cDat = mglData(c)
  aDat = mglData(a)

    	ccall((:mgl_surfca,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data, cDat.data, aDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function map(gr::mglGraph,x::Array,y::Array,a::Array,b::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  aDat = mglData(a)
  bDat = mglData(b)

    	ccall((:mgl_map_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, aDat.data, bDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function map(gr::mglGraph,a::Array,b::Array,sch::ASCIIString="",opt::ASCIIString="")
  aDat = mglData(a)
  bDat = mglData(b)

    	ccall((:mgl_map,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, aDat.data, bDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function sTFA(gr::mglGraph,x::Array,y::Array,re::Array,im::Array, dn::Cint,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  reDat = mglData(re)
  imDat = mglData(im)

    	ccall((:mgl_stfa_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Cint,Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, reDat.data, imDat.data, dn,pointer("$sch\0".data),pointer("$opt\0".data))
end

function sTFA(gr::mglGraph,re::Array,im::Array, dn::Cint,sch::ASCIIString="",opt::ASCIIString="")
  reDat = mglData(re)
  imDat = mglData(im)

    	ccall((:mgl_stfa,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Cint,Ptr{Cchar},Ptr{Cchar}), gr, reDat.data, imDat.data, dn,pointer("$sch\0".data),pointer("$opt\0".data))
end

function surf3A(gr::mglGraph, Val::Number,x::Array,y::Array,z::Array,a::Array,b::Array,stl::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)
  bDat = mglData(b)

    	mgl_surf3ccall((:a_xyz_val,"libmgl2"), Void, (Ptr{Void},Cdouble,Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, Val, xDat.data, yDat.data, zDat.data, aDat.data, bDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function surf3A(gr::mglGraph, Val::Number,a::Array,b::Array,stl::ASCIIString="",opt::ASCIIString="")
  aDat = mglData(a)
  bDat = mglData(b)

    	mgl_surf3ccall((:a_val,"libmgl2"), Void, (Ptr{Void},Cdouble,Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, Val, aDat.data, bDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function surf3A(gr::mglGraph,x::Array,y::Array,z::Array,a::Array,b::Array,stl::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)
  bDat = mglData(b)

    	mgl_surf3ccall((:a_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, aDat.data, bDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function surf3A(gr::mglGraph,a::Array,b::Array,stl::ASCIIString="",opt::ASCIIString="")
  aDat = mglData(a)
  bDat = mglData(b)

    	mgl_surf3ccall((:a,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, aDat.data, bDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function surf3C(gr::mglGraph, Val::Number,x::Array,y::Array,z::Array,a::Array,c::Array,stl::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)
  cDat = mglData(c)

    	mgl_surf3ccall((:c_xyz_val,"libmgl2"), Void, (Ptr{Void},Cdouble,Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, Val, xDat.data, yDat.data, zDat.data, aDat.data, cDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function surf3C(gr::mglGraph, Val::Number,a::Array,c::Array,stl::ASCIIString="",opt::ASCIIString="")
  aDat = mglData(a)
  cDat = mglData(c)

    	mgl_surf3ccall((:c_val,"libmgl2"), Void, (Ptr{Void},Cdouble,Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, Val, aDat.data, cDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function surf3C(gr::mglGraph,x::Array,y::Array,z::Array,a::Array,c::Array,stl::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)
  cDat = mglData(c)

    	mgl_surf3ccall((:c_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, aDat.data, cDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function surf3C(gr::mglGraph,a::Array,c::Array,stl::ASCIIString="",opt::ASCIIString="")
  aDat = mglData(a)
  cDat = mglData(c)

    	mgl_surf3ccall((:c,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, aDat.data, cDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function surf3CA(gr::mglGraph, Val::Number,x::Array,y::Array,z::Array,a::Array,c::Array,b::Array,stl::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)
  cDat = mglData(c)
  bDat = mglData(b)

    	mgl_surf3ccall((:ca_xyz_val,"libmgl2"), Void, (Ptr{Void},Cdouble,Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, Val, xDat.data, yDat.data, zDat.data, aDat.data, cDat.data, bDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function surf3CA(gr::mglGraph, Val::Number,a::Array,c::Array,b::Array,stl::ASCIIString="",opt::ASCIIString="")
  aDat = mglData(a)
  cDat = mglData(c)
  bDat = mglData(b)

    	mgl_surf3ccall((:ca_val,"libmgl2"), Void, (Ptr{Void},Cdouble,Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, Val, aDat.data, cDat.data, bDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function surf3CA(gr::mglGraph,x::Array,y::Array,z::Array,a::Array,c::Array,b::Array,stl::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)
  cDat = mglData(c)
  bDat = mglData(b)

    	mgl_surf3ccall((:ca_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, aDat.data, cDat.data, bDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function surf3CA(gr::mglGraph,a::Array,c::Array,b::Array,stl::ASCIIString="",opt::ASCIIString="")
  aDat = mglData(a)
  cDat = mglData(c)
  bDat = mglData(b)

    	mgl_surf3ccall((:ca,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, aDat.data, cDat.data, bDat.data,pointer("$stl\0".data),pointer("$opt\0".data))
end

function dew(gr::mglGraph,x::Array,y::Array,ax::Array,ay::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	ccall((:mgl_dew_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, axDat.data, ayDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function dew(gr::mglGraph,ax::Array,ay::Array,sch::ASCIIString="",opt::ASCIIString="")
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	mgl_dew_2ccall((:d,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, axDat.data, ayDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function traj(gr::mglGraph,x::Array,y::Array,ax::Array,ay::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	ccall((:mgl_traj_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, axDat.data, ayDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function traj(gr::mglGraph,x::Array,y::Array,z::Array,ax::Array,ay::Array,az::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	ccall((:mgl_traj_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, axDat.data, ayDat.data, azDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function vect(gr::mglGraph,x::Array,y::Array,ax::Array,ay::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	ccall((:mgl_vect_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, axDat.data, ayDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function vect(gr::mglGraph,ax::Array,ay::Array,sch::ASCIIString="",opt::ASCIIString="")
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	mgl_vect_2ccall((:d,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, axDat.data, ayDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function vect(gr::mglGraph,x::Array,y::Array,z::Array,ax::Array,ay::Array,az::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	ccall((:mgl_vect_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, axDat.data, ayDat.data, azDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function vect(gr::mglGraph,ax::Array,ay::Array,az::Array,sch::ASCIIString="",opt::ASCIIString="")
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	mgl_vect_3ccall((:d,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, axDat.data, ayDat.data, azDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function vect3(gr::mglGraph,x::Array,y::Array,z::Array,ax::Array,ay::Array,az::Array,stl::ASCIIString="", sVal::Number=-1.0,opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	mgl_vect3ccall((:_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, axDat.data, ayDat.data, azDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function vect3(gr::mglGraph,ax::Array,ay::Array,az::Array,stl::ASCIIString="", sVal::Number=-1.0,opt::ASCIIString="")
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	mgl_vect3ccall((:,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, axDat.data, ayDat.data, azDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function flow(gr::mglGraph,x::Array,y::Array,ax::Array,ay::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	ccall((:mgl_flow_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, axDat.data, ayDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function flow(gr::mglGraph,ax::Array,ay::Array,sch::ASCIIString="",opt::ASCIIString="")
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	mgl_flow_2ccall((:d,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, axDat.data, ayDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function flow(gr::mglGraph,x::Array,y::Array,z::Array,ax::Array,ay::Array,az::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	ccall((:mgl_flow_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, axDat.data, ayDat.data, azDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function flow(gr::mglGraph,ax::Array,ay::Array,az::Array,sch::ASCIIString="",opt::ASCIIString="")
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	mgl_flow_3ccall((:d,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, axDat.data, ayDat.data, azDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function flowP(gr::mglGraph, p::mglPoint,x::Array,y::Array,ax::Array,ay::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	ccall((:mgl_flowp_xy,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, p.x, p.y, p.z, xDat.data, yDat.data, axDat.data, ayDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function flowP(gr::mglGraph, p::mglPoint,ax::Array,ay::Array,sch::ASCIIString="",opt::ASCIIString="")
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	mgl_flowp_2ccall((:d,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, p.x, p.y, p.z, axDat.data, ayDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function flowP(gr::mglGraph, p::mglPoint,x::Array,y::Array,z::Array,ax::Array,ay::Array,az::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	ccall((:mgl_flowp_xyz,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, p.x, p.y, p.z, xDat.data, yDat.data, zDat.data, axDat.data, ayDat.data, azDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function flowP(gr::mglGraph, p::mglPoint,ax::Array,ay::Array,az::Array,sch::ASCIIString="",opt::ASCIIString="")
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	mgl_flowp_3ccall((:d,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, p.x, p.y, p.z, axDat.data, ayDat.data, azDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function pipe(gr::mglGraph,x::Array,y::Array,ax::Array,ay::Array,sch::ASCIIString="", r0::Number=0.05,opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	ccall((:mgl_pipe_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, xDat.data, yDat.data, axDat.data, ayDat.data,pointer("$sch\0".data), r0,pointer("$opt\0".data))
end

function pipe(gr::mglGraph,ax::Array,ay::Array,sch::ASCIIString="", r0::Number=0.05,opt::ASCIIString="")
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	mgl_pipe_2ccall((:d,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, axDat.data, ayDat.data,pointer("$sch\0".data), r0,pointer("$opt\0".data))
end

function pipe(gr::mglGraph,x::Array,y::Array,z::Array,ax::Array,ay::Array,az::Array,sch::ASCIIString="", r0::Number=0.05,opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	ccall((:mgl_pipe_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, axDat.data, ayDat.data, azDat.data,pointer("$sch\0".data), r0,pointer("$opt\0".data))
end

function pipe(gr::mglGraph,ax::Array,ay::Array,az::Array,sch::ASCIIString="", r0::Number=0.05,opt::ASCIIString="")
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	mgl_pipe_3ccall((:d,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, axDat.data, ayDat.data, azDat.data,pointer("$sch\0".data), r0,pointer("$opt\0".data))
end

function densX(gr::mglGraph,a::Array,stl::ASCIIString="", sVal::Number=mglNaN,opt::ASCIIString="")
  aDat = mglData(a)

    	ccall((:mgl_dens_x,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function densY(gr::mglGraph,a::Array,stl::ASCIIString="", sVal::Number=mglNaN,opt::ASCIIString="")
  aDat = mglData(a)

    	ccall((:mgl_dens_y,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function densZ(gr::mglGraph,a::Array,stl::ASCIIString="", sVal::Number=mglNaN,opt::ASCIIString="")
  aDat = mglData(a)

    	ccall((:mgl_dens_z,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function contX(gr::mglGraph,a::Array,stl::ASCIIString="", sVal::Number=mglNaN,opt::ASCIIString="")
  aDat = mglData(a)

    	ccall((:mgl_cont_x,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function contX(gr::mglGraph,v::Array,a::Array,stl::ASCIIString="", sVal::Number=mglNaN,opt::ASCIIString="")
  vDat = mglData(v)
  aDat = mglData(a)

    	ccall((:mgl_cont_x_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, vDat.data, aDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function contY(gr::mglGraph,a::Array,stl::ASCIIString="", sVal::Number=mglNaN,opt::ASCIIString="")
  aDat = mglData(a)

    	ccall((:mgl_cont_y,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function contY(gr::mglGraph,v::Array,a::Array,stl::ASCIIString="", sVal::Number=mglNaN,opt::ASCIIString="")
  vDat = mglData(v)
  aDat = mglData(a)

    	ccall((:mgl_cont_y_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, vDat.data, aDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function contZ(gr::mglGraph,a::Array,stl::ASCIIString="", sVal::Number=mglNaN,opt::ASCIIString="")
  aDat = mglData(a)

    	ccall((:mgl_cont_z,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function contZ(gr::mglGraph,v::Array,a::Array,stl::ASCIIString="", sVal::Number=mglNaN,opt::ASCIIString="")
  vDat = mglData(v)
  aDat = mglData(a)

    	ccall((:mgl_cont_z_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, vDat.data, aDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function contFX(gr::mglGraph,a::Array,stl::ASCIIString="", sVal::Number=mglNaN,opt::ASCIIString="")
  aDat = mglData(a)

    	ccall((:mgl_contf_x,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function contFX(gr::mglGraph,v::Array,a::Array,stl::ASCIIString="", sVal::Number=mglNaN,opt::ASCIIString="")
  vDat = mglData(v)
  aDat = mglData(a)

    	ccall((:mgl_contf_x_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, vDat.data, aDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function contFY(gr::mglGraph,a::Array,stl::ASCIIString="", sVal::Number=mglNaN,opt::ASCIIString="")
  aDat = mglData(a)

    	ccall((:mgl_contf_y,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function contFY(gr::mglGraph,v::Array,a::Array,stl::ASCIIString="", sVal::Number=mglNaN,opt::ASCIIString="")
  vDat = mglData(v)
  aDat = mglData(a)

    	ccall((:mgl_contf_y_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, vDat.data, aDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function contFZ(gr::mglGraph,a::Array,stl::ASCIIString="", sVal::Number=mglNaN,opt::ASCIIString="")
  aDat = mglData(a)

    	ccall((:mgl_contf_z,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function contFZ(gr::mglGraph,v::Array,a::Array,stl::ASCIIString="", sVal::Number=mglNaN,opt::ASCIIString="")
  vDat = mglData(v)
  aDat = mglData(a)

    	ccall((:mgl_contf_z_val,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, vDat.data, aDat.data,pointer("$stl\0".data), sVal,pointer("$opt\0".data))
end

function fPlot(gr::mglGraph,fy::ASCIIString,stl::ASCIIString="",opt::ASCIIString="")

    	ccall((:mgl_fplot,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fy\0".data),pointer("$stl\0".data),pointer("$opt\0".data))
end

function fPlot(gr::mglGraph,fx::ASCIIString,fy::ASCIIString,fz::ASCIIString,stl::ASCIIString,opt::ASCIIString="")

    	ccall((:mgl_fplot_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fx\0".data),pointer("$fy\0".data),pointer("$fz\0".data),pointer("$stl\0".data),pointer("$opt\0".data))
end

function fSurf(gr::mglGraph,fz::ASCIIString,stl::ASCIIString="",opt::ASCIIString="")

    	ccall((:mgl_fsurf,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fz\0".data),pointer("$stl\0".data),pointer("$opt\0".data))
end

function fSurf(gr::mglGraph,fx::ASCIIString,fy::ASCIIString,fz::ASCIIString,stl::ASCIIString,opt::ASCIIString="")

    	ccall((:mgl_fsurf_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fx\0".data),pointer("$fy\0".data),pointer("$fz\0".data),pointer("$stl\0".data),pointer("$opt\0".data))
end

function triPlot(gr::mglGraph,nums::Array,x::Array,y::Array,z::Array,c::Array,sch::ASCIIString="",opt::ASCIIString="")
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  cDat = mglData(c)

    	ccall((:mgl_triplot_xyzc,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, numsDat.data, xDat.data, yDat.data, zDat.data, cDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function triPlot(gr::mglGraph,nums::Array,x::Array,y::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_triplot_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, numsDat.data, xDat.data, yDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function triPlot(gr::mglGraph,nums::Array,x::Array,y::Array,sch::ASCIIString="",opt::ASCIIString="")
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_triplot_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, numsDat.data, xDat.data, yDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function quadPlot(gr::mglGraph,nums::Array,x::Array,y::Array,z::Array,c::Array,sch::ASCIIString="",opt::ASCIIString="")
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  cDat = mglData(c)

    	ccall((:mgl_quadplot_xyzc,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, numsDat.data, xDat.data, yDat.data, zDat.data, cDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function quadPlot(gr::mglGraph,nums::Array,x::Array,y::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_quadplot_xyz,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, numsDat.data, xDat.data, yDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function quadPlot(gr::mglGraph,nums::Array,x::Array,y::Array,sch::ASCIIString="",opt::ASCIIString="")
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_quadplot_xy,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, numsDat.data, xDat.data, yDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function triCont(gr::mglGraph,nums::Array,x::Array,y::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_tricont_xyc,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, numsDat.data, xDat.data, yDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function triContV(gr::mglGraph,v::Array,nums::Array,x::Array,y::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  vDat = mglData(v)
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_tricont_xycv,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, numsDat.data, xDat.data, yDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function triCont(gr::mglGraph,nums::Array,x::Array,y::Array,z::Array,a::Array,sch::ASCIIString="",opt::ASCIIString="")
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	ccall((:mgl_tricont_xyzc,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, numsDat.data, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function triContV(gr::mglGraph,v::Array,nums::Array,x::Array,y::Array,z::Array,a::Array,sch::ASCIIString="",opt::ASCIIString="")
  vDat = mglData(v)
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	ccall((:mgl_tricont_xyzcv,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, numsDat.data, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function triCont(gr::mglGraph,v::Array,nums::Array,x::Array,y::Array,z::Array,a::Array,sch::ASCIIString="",opt::ASCIIString="")
  vDat = mglData(v)
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	ccall((:mgl_tricont_xyzcv,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, numsDat.data, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function triContVt(gr::mglGraph,nums::Array,x::Array,y::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_tricontv_xyc,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, numsDat.data, xDat.data, yDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function triContVt(gr::mglGraph,nums::Array,x::Array,y::Array,z::Array,a::Array,sch::ASCIIString="",opt::ASCIIString="")
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	ccall((:mgl_tricontv_xyzc,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, numsDat.data, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function triContVt(gr::mglGraph,v::Array,nums::Array,x::Array,y::Array,z::Array,a::Array,sch::ASCIIString="",opt::ASCIIString="")
  vDat = mglData(v)
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	ccall((:mgl_tricontv_xyzcv,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, numsDat.data, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function dots(gr::mglGraph,x::Array,y::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_dots,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function dots(gr::mglGraph,x::Array,y::Array,z::Array,a::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	ccall((:mgl_dots_a,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function dots(gr::mglGraph,x::Array,y::Array,z::Array,c::Array,a::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  cDat = mglData(c)
  aDat = mglData(a)

    	ccall((:mgl_dots_ca,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, cDat.data, aDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function crust(gr::mglGraph,x::Array,y::Array,z::Array,sch::ASCIIString="",opt::ASCIIString="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_crust,"libmgl2"), Void, (Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Void},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$sch\0".data),pointer("$opt\0".data))
end

function putsFit(gr::mglGraph, p::mglPoint,prefix::ASCIIString=0,font::ASCIIString="", size::Number=-1.0)

    	ccall((:mgl_puts_fit,"libmgl2"), Void, (Ptr{Void},mreal,mreal,mreal,Ptr{Cchar},Ptr{Cchar},Cdouble), gr, p.x, p.y, p.z,pointer("$prefix\0".data),pointer("$font\0".data), size)
end

function fill(gr::mglGraph,u::Array,eq::ASCIIString,opt::ASCIIString="")
  uDat = mglData(u)

    	ccall((:mgl_data_fill_eq,"libmgl2"), Void, (Ptr{Void},mglData,Ptr{Cchar},Ptr{Cchar}), gr, uDat.data,pointer("$eq\0".data), 0, 0,pointer("$opt\0".data))
end

function fill(gr::mglGraph,u::Array,eq::ASCIIString,v::Array,opt::ASCIIString="")
  uDat = mglData(u)
  vDat = mglData(v)

    	ccall((:mgl_data_fill_eq,"libmgl2"), Void, (Ptr{Void},mglData,Ptr{Cchar},Ptr{Void},Ptr{Cchar}), gr, uDat.data,pointer("$eq\0".data), vDat.data, 0,pointer("$opt\0".data))
end

function fill(gr::mglGraph,u::Array,eq::ASCIIString,v::Array,w::Array,opt::ASCIIString="")
  uDat = mglData(u)
  vDat = mglData(v)
  wDat = mglData(w)

    	ccall((:mgl_data_fill_eq,"libmgl2"), Void, (Ptr{Void},mglData,Ptr{Cchar},Ptr{Void},Ptr{Void},Ptr{Cchar}), gr, uDat.data,pointer("$eq\0".data), vDat.data, wDat.data,pointer("$opt\0".data))
end

function fill(gr::mglGraph,u::mglDataC,eq::ASCIIString,opt::ASCIIString="")

    	ccall((:mgl_data_fill_eq,"libmgl2"), Void, (Ptr{Void},mglDataC,Ptr{Cchar},Ptr{Cchar}), gr,mglData(u).data,pointer("$eq\0".data), 0, 0,pointer("$opt\0".data))
end

function fill(gr::mglGraph,u::mglDataC,eq::ASCIIString,v::Array,opt::ASCIIString="")
  vDat = mglData(v)

    	ccall((:mgl_data_fill_eq,"libmgl2"), Void, (Ptr{Void},mglDataC,Ptr{Cchar},Ptr{Void},Ptr{Cchar}), gr,mglData(u).data,pointer("$eq\0".data), vDat.data, 0,pointer("$opt\0".data))
end

function fill(gr::mglGraph,u::mglDataC,eq::ASCIIString,v::Array,w::Array,opt::ASCIIString="")
  vDat = mglData(v)
  wDat = mglData(w)

    	ccall((:mgl_data_fill_eq,"libmgl2"), Void, (Ptr{Void},mglDataC,Ptr{Cchar},Ptr{Void},Ptr{Void},Ptr{Cchar}), gr,mglData(u).data,pointer("$eq\0".data), vDat.data, wDat.data,pointer("$opt\0".data))
end

function title(gr::mglGraph, text::ASCIIString, stl::ASCIIString="", size::mreal=-2.0)
        ccall((:mgl_title, "libmgl2"), Void, (Ptr{Void}, Ptr{Cchar}, Ptr{Cchar}, Cdouble), gr, pointer("$text\0".data), pointer("$stl\0".data), size)
end

export mglGraph
export mglPoint
#export mglData
export draw
export view

export AddLegend
export AddLight
export AddRange
export Adjust
export Alpha
export Arc
export Area
export Aspect
export AttachLight
export Axial
export Axis
export Barh
export Bars
export Beam
export Belt
export Box
export BoxPlot
export Boxs
export Candle
export Chart
export Circle
export Clf
export Cloud
export Colorbar
export Cone
export Cones
export Cont
export Cont3
export ContD
export ContF
export ContF3
export ContFX
export ContFY
export ContFZ
export ContV
export ContX
export ContY
export ContZ
export CopyFont
export Crust
export Curve
export CutOff
export DelFrame
export Dens
export Dens3
export DensX
export DensY
export DensZ
export Dew
export Dots
export Drop
export Ellipse
export Error
export ExportMGLD
export Face
export FaceX
export FaceY
export FaceZ
export Fall
export Fill
export Flow
export FlowP
export Fog
export FPlot
export FSurf
export GetFrame
export Grid
export Grid3
export Highlight
export ImportMGLD
export Label
export Legend
export Light
export Line
export LoadFont
export Logo
export Map
export Mark
export Mesh
export MultiPlot
export OHLC
export Perspective
export Pipe
export Plot
export Polygon
export Puts
export PutsFit
export Putsw
export QuadPlot
export Radar
export Region
export Rhomb
export Rotate
export RotateN
export SetAlphaDef
export SetAmbient
export SetArrowSize
export SetAutoRanges
export SetAxisStl
export SetBarWidth
export SetCoor
export SetCut
export SetCutBox
export SetDefScheme
export SetDiffuse
export SetDifLight
export SetEventFunc
export SetFaceNum
export SetFontDef
export SetFontSize
export SetFrame
export SetFunc
export SetLegendMarks
export SetMarkSize
export SetMaskAngle
export SetMeshNum
export SetOrigin
export SetPalette
export SetQuality
export SetRange
export SetRanges
export SetRotatedText
export SetSize
export SetTickLen
export SetTicks
export SetTranspType
export SetTuneTicks
export ShowFrame
export ShowImage
export Sphere
export StartGIF
export StartGroup
export Stem
export Step
export STFA
export Stop
export SubPlot
export Surf
export Surf3
export Surf3A
export Surf3C
export Surf3CA
export SurfA
export SurfC
export SurfCA
export Table
export Tape
export Tens
export Ternary
export Text
export TextMark
export Tile
export TileS
export Torus
export Traj
export TriCont
export TriContV
export TriContVt
export TriPlot
export Tube
export Vect
export Vect3
export View
export ViewAsRotate
export WriteBMP
export WriteBPS
export WriteEPS
export WriteFrame
export WriteGIF
export WriteJPEG
export WriteOBJ
export WriteOBJold
export WriteOFF
export WritePRC
export WriteSTL
export WriteSVG
export WriteTEX
export WriteTGA
export WriteXYZ
export Zoom
export ZoomAxis
export Title
end # module

