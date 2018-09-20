module MathGL
import Base.unsafe_convert

const MGL_NO_ORIGIN = 0x100000
const MGL_USE_GMTIME = 0x000800
const mgllib_name = "libmgl"

default_width = 600
default_height = 400

"""A helper function"""
function test_ascii(strings::String...)
	for str in strings
		if !isascii(str)
			error("The argument must be ASCII characters only")
		end
	end
end

"""Set the default pixel size for plots"""
function set_default_size(width::Integer, height::Integer)
	global default_width = width
	global default_height = height
	return (default_width, default_height)
end


"""A wrapper type around MathGL's mglGraph type"""
mutable struct mglGraph
    width::Int
    height::Int
    graph::Ptr{Nothing}

    function mglGraph(width::Int, height::Int)
        width = width
        height = height
        graph=ccall((:mgl_create_graph, mgllib_name), Ptr{Nothing}, (Int64, Int64), width, height)
	gr = new(width, height, graph)
	finalizer(freeMglGraph, gr)
        return gr
    end

    mglGraph() = mglGraph(default_width, default_height)

end

"""Free an mglGraph"""
function freeMglGraph(gr::mglGraph)
    ccall((:mgl_delete_graph, mgllib_name), Nothing, (Ptr{Nothing},), gr.graph)
end

"""Convert an mglGraph to a Ptr{Nothing} so that it works seamlessly with ccall()"""
unsafe_convert(::Type{Ptr{Nothing}}, gr::mglGraph) = gr.graph

const mreal = Cdouble
"""A wrapper around the MathGL mglData type"""
mutable struct mglData
    nx::Int
    ny::Int
    nz::Int
    data::Ptr{Nothing}

    function mglData(nx::Integer, ny::Integer=1, nz::Integer=1)
        nx = nx
        ny = ny
        nz = nz
        dataPtr = dataPointer(ccall((:mgl_create_data_size, mgllib_name), Ptr{Nothing}, (Int, Int, Int), nx, ny, nz))
        data = new(nx, ny, nz, dataPtr)
	finalizer(freeMglData, data)
	return data
    end

    function mglData(other::mglData) #TODO: Is this function even necessary?
        new(other.nx, other.ny, other.nz, other.data)
    end

    function mglData(a::AbstractArray)
        @assert ndims(a) < 4 "a must have dimension at most 3"
        nx = ndims(a) > 1 ? size(a)[2] : size(a)[1]
        ny = ndims(a) > 1 ? size(a)[1] : 1
        nz = ndims(a) > 2 ? size(a)[3] : 1
        data = ccall((:mgl_create_data_size, mgllib_name), Ptr{Nothing}, (Cint, Cint, Cint), nx, ny, nz)

        if ndims(a) > 1
            for k::Int in 1:nz
                for i in 1:nx
                    for j in 1:ny
                        ccall((:mgl_data_set_value, mgllib_name), Nothing, (Ptr{Nothing}, mreal, Cint, Cint, Cint), data, a[j,i,k], i-1, j-1, k-1)
                    end
                end
            end
        else
            for i in 1:nx
                ccall((:mgl_data_set_value, mgllib_name), Nothing, (Ptr{Nothing}, mreal, Cint, Cint, Cint), data, a[i], i-1, 0, 0)
            end
        end

        dat = new(nx, ny, nz, data)
	finalizer(dat, freeMglData)
	return dat
    end

end

"""Free an mglData"""
function freeMglData(data::mglData)
	ccall((:mgl_delete_data, mgllib_name), Nothing, (Ptr{Nothing},), data.data)
end


"""Convert an mglData object to a Ptr{Nothing} so that it can be used transparently in ccall"""
unsafe_convert(::Type{Ptr{Nothing}}, data::mglData) = data.data

include("plotOpStack.jl")
#Now that mglGraph and plotOpStack are defined, we include the mimetype stuff
include("mimetypes.jl")

const mglDataA = mglData

struct mglPoint
    x::mreal
    y::mreal
    z::mreal
end

struct mglFormula
    mglFormula() = error("mglFormula is not implemented")
end

const HMEX = mglFormula

struct mglFormulaC
    mglFormulaC() = error("mglFormulaC is not implemented")
end

const HAEX = mglFormula

struct mglDataC
    mglDataC() = error("mglDataC is not implemented")
end
const HADT = mglDataC
struct dual
    dual() = error("dual is not implemented")
end

mglNaN = NaN
function Stop(ops::plotOpStack, stop::Bool=true)
	push!(ops, gr->Stop(gr, stop))
end

function SetWarn(gr::mglGraph, code::Int, info_::String)

	ccall((:mgl_set_warn,mgllib_name), Nothing, (Ptr{Nothing},Cint,Cstring), gr, code, info_)
end

function Message(gr::mglGraph)

	unsafe_string(ccall((:mgl_get_mess,mgllib_name), Cstring, (Ptr{Nothing},), gr))
end

function GetWarn(gr::mglGraph)

	ccall((:mgl_get_warn,mgllib_name), Cint, (Ptr{Nothing},), gr)
end

function Stop(gr::mglGraph, stop::Bool=true)

    	ccall((:mgl_ask_stop,mgllib_name), Nothing, (Ptr{Nothing},Bool), gr, stop)
end

#=function SetEventFunc(ops::plotOpStack, void (*func)(void *), void *par=NULL)
	push!(ops, gr->SetEventFunc(gr, void (*func)(void *), void *par=NULL))
end

function SetEventFunc(gr::mglGraph, void (*func)(void *), void *par=NULL)

    	ccall((:mgl_set_event_func,mgllib_name), Nothing, (Ptr{Nothing},void,void), gr, func, par)
end=#

function Alpha(ops::plotOpStack, enable::Bool)
	push!(ops, gr->Alpha(gr, enable))
end

function Alpha(gr::mglGraph, enable::Bool)

    	ccall((:mgl_set_alpha,mgllib_name), Nothing, (Ptr{Nothing},Bool), gr, enable)
end

function SetAlphaDef(ops::plotOpStack, alpha::Number)
	push!(ops, gr->SetAlphaDef(gr, alpha))
end

function SetAlphaDef(gr::mglGraph, alpha::Number)

    	ccall((:mgl_set_alpha_default,mgllib_name), Nothing, (Ptr{Nothing},Cdouble), gr, alpha)
end

function SetTranspType(ops::plotOpStack, transpType::Int)
	push!(ops, gr->SetTranspType(gr, transpType))
end

function SetTranspType(gr::mglGraph, transpType::Int)

    	ccall((:mgl_set_transp_type,mgllib_name), Nothing, (Ptr{Nothing},Cint), gr, transpType)
end

function Light(ops::plotOpStack, enable::Bool)
	push!(ops, gr->Light(gr, enable))
end

function Light(gr::mglGraph, enable::Bool)

    	ccall((:mgl_set_light,mgllib_name), Nothing, (Ptr{Nothing},Bool), gr, enable)
end

function Light(ops::plotOpStack, n::Int, enable::Bool)
	push!(ops, gr->Light(gr, n, enable))
end

function Light(gr::mglGraph, n::Int, enable::Bool)

    	ccall((:mgl_set_light_n,mgllib_name), Nothing, (Ptr{Nothing},Cint,Bool), gr, n, enable)
end

function SetDifLight(ops::plotOpStack, dif::Bool)
	push!(ops, gr->SetDifLight(gr, dif))
end

function SetDifLight(gr::mglGraph, dif::Bool)

    	ccall((:mgl_set_light_dif,mgllib_name), Nothing, (Ptr{Nothing},Bool), gr, dif)
end

function AttachLight(ops::plotOpStack, enable::Bool)
	push!(ops, gr->AttachLight(gr, enable))
end

function AttachLight(gr::mglGraph, enable::Bool)

    	ccall((:mgl_set_attach_light,mgllib_name), Nothing, (Ptr{Nothing},Bool), gr, enable)
end

function AddLight(ops::plotOpStack, n::Int, p::mglPoint, col::Char='w', bright::Number=0.5, ap::Number=0.0)
	push!(ops, gr->AddLight(gr, n, p, col, bright, ap))
end

function AddLight(gr::mglGraph, n::Int, p::mglPoint, col::Char='w', bright::Number=0.5, ap::Number=0.0)

    	ccall((:mgl_add_light_ext,mgllib_name), Nothing, (Ptr{Nothing},Cint,mreal,mreal,mreal,Cchar,Cdouble,Cdouble), gr, n, p.x, p.y, p.z, col, bright, ap)
end

function AddLight(ops::plotOpStack, n::Int, r::mglPoint, p::mglPoint, col::Char='w', bright::Number=0.5, ap::Number=0.0)
	push!(ops, gr->AddLight(gr, n, r, p, col, bright, ap))
end

function AddLight(gr::mglGraph, n::Int, r::mglPoint, p::mglPoint, col::Char='w', bright::Number=0.5, ap::Number=0.0)

    	ccall((:mgl_add_light_loc,mgllib_name), Nothing, (Ptr{Nothing},Cint,mreal,mreal,mreal,mreal,mreal,mreal,Cchar,Cdouble,Cdouble), gr, n, r.x, r.y, r.z, p.x, p.y, p.z, col, bright, ap)
end

function SetAmbient(ops::plotOpStack, i::Number)
	push!(ops, gr->SetAmbient(gr, i))
end

function SetAmbient(gr::mglGraph, i::Number)

    	ccall((:mgl_set_ambbr,mgllib_name), Nothing, (Ptr{Nothing},Cdouble), gr, i)
end

function SetDiffuse(ops::plotOpStack, i::Number)
	push!(ops, gr->SetDiffuse(gr, i))
end

function SetDiffuse(gr::mglGraph, i::Number)

    	ccall((:mgl_set_difbr,mgllib_name), Nothing, (Ptr{Nothing},Cdouble), gr, i)
end

function Fog(ops::plotOpStack, d::Number, dz::Number=0.25)
	push!(ops, gr->Fog(gr, d, dz))
end

function Fog(gr::mglGraph, d::Number, dz::Number=0.25)

    	ccall((:mgl_set_fog,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Cdouble), gr, d, dz)
end

function SetBarWidth(ops::plotOpStack, width::Number)
	push!(ops, gr->SetBarWidth(gr, width))
end

function SetBarWidth(gr::mglGraph, width::Number)

    	ccall((:mgl_set_bar_width,mgllib_name), Nothing, (Ptr{Nothing},Cdouble), gr, width)
end

function SetMarkSize(ops::plotOpStack, size::Number)
	push!(ops, gr->SetMarkSize(gr, size))
end

function SetMarkSize(gr::mglGraph, size::Number)

    	ccall((:mgl_set_mark_size,mgllib_name), Nothing, (Ptr{Nothing},Cdouble), gr, size)
end

function SetArrowSize(ops::plotOpStack, size::Number)
	push!(ops, gr->SetArrowSize(gr, size))
end

function SetArrowSize(gr::mglGraph, size::Number)

    	ccall((:mgl_set_arrow_size,mgllib_name), Nothing, (Ptr{Nothing},Cdouble), gr, size)
end

function SetMeshNum(ops::plotOpStack, num::Int)
	push!(ops, gr->SetMeshNum(gr, num))
end

function SetMeshNum(gr::mglGraph, num::Int)

    	ccall((:mgl_set_meshnum,mgllib_name), Nothing, (Ptr{Nothing},Cint), gr, num)
end

function SetFaceNum(ops::plotOpStack, num::Int)
	push!(ops, gr->SetFaceNum(gr, num))
end

function SetFaceNum(gr::mglGraph, num::Int)

    	ccall((:mgl_set_facenum,mgllib_name), Nothing, (Ptr{Nothing},Cint), gr, num)
end

function SetCut(ops::plotOpStack, cut::Bool)
	push!(ops, gr->SetCut(gr, cut))
end

function SetCut(gr::mglGraph, cut::Bool)

    	ccall((:mgl_set_cut,mgllib_name), Nothing, (Ptr{Nothing},Bool), gr, cut)
end

function SetCutBox(ops::plotOpStack, p1::mglPoint, p2::mglPoint)
	push!(ops, gr->SetCutBox(gr, p1, p2))
end

function SetCutBox(gr::mglGraph, p1::mglPoint, p2::mglPoint)

    	ccall((:mgl_set_cut_box,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,mreal,mreal,mreal), gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z)
end

function CutOff(ops::plotOpStack,EqC::String)
	push!(ops, gr->CutOff(gr,EqC))
end

function CutOff(gr::mglGraph,EqC::String)

    	ccall((:mgl_set_cutoff,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar}), gr,pointer("$EqC\0"))
end

function SetFontSize(ops::plotOpStack, size::Number)
	push!(ops, gr->SetFontSize(gr, size))
end

function SetFontSize(gr::mglGraph, size::Number)

    	ccall((:mgl_set_font_size,mgllib_name), Nothing, (Ptr{Nothing},Cdouble), gr, size)
end

function SetFontDef(ops::plotOpStack,fnt::String)
	push!(ops, gr->SetFontDef(gr,fnt))
end

function SetFontDef(gr::mglGraph,fnt::String)

    	ccall((:mgl_set_font_def,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar}), gr,pointer("$fnt\0"))
end

function LoadFont(ops::plotOpStack,name::String,path::String="")
	push!(ops, gr->LoadFont(gr,name,path))
end

function LoadFont(gr::mglGraph,name::String,path::String="")

    	ccall((:mgl_load_font,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$name\0"),pointer("$path\0"))
end

function CopyFont(ops::plotOpStack, GR::mglGraph)
	push!(ops, gr->CopyFont(gr, GR))
end

function CopyFont(gr::mglGraph, GR::mglGraph)

    	ccall((:mgl_copy_font,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing}), gr, GR.gr)
end

function SetRotatedText(ops::plotOpStack, rotated::Bool)
	push!(ops, gr->SetRotatedText(gr, rotated))
end

function SetRotatedText(gr::mglGraph, rotated::Bool)

    	ccall((:mgl_set_rotated_text,mgllib_name), Nothing, (Ptr{Nothing},Bool), gr, rotated)
end

function SetPalette(ops::plotOpStack,colors::String)
	push!(ops, gr->SetPalette(gr,colors))
end

function SetPalette(gr::mglGraph,colors::String)

    	ccall((:mgl_set_palette,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar}), gr,pointer("$colors\0"))
end

function SetDefScheme(ops::plotOpStack,sch::String)
	push!(ops, gr->SetDefScheme(gr,sch))
end

function SetDefScheme(gr::mglGraph,sch::String)

    	ccall((:mgl_set_def_sch,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar}), gr,pointer("$sch\0"))
end

function SetMaskAngle(ops::plotOpStack, angle::Int)
	push!(ops, gr->SetMaskAngle(gr, angle))
end

function SetMaskAngle(gr::mglGraph, angle::Int)

    	ccall((:mgl_set_mask_angle,mgllib_name), Nothing, (Ptr{Nothing},Cint), gr, angle)
end

function ZoomAxis(ops::plotOpStack, p1::mglPoint=mglPoint(0,0,0,0), p2::mglPoint=mglPoint(1,1,1,1))
	push!(ops, gr->ZoomAxis(gr, p1, p2))
end

function ZoomAxis(gr::mglGraph, p1::mglPoint=mglPoint(0,0,0,0), p2::mglPoint=mglPoint(1,1,1,1))

    	ccall((:mgl_zoom_axis,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal), gr, p1.x,p1.y,p1.z,p1.c, p2.x,p2.y,p2.z,p2.c)
end

function AddRange(ops::plotOpStack, dir::Char, v1::Number, v2::Number)
	push!(ops, gr->AddRange(gr, dir, v1, v2))
end

function AddRange(gr::mglGraph, dir::Char, v1::Number, v2::Number)

    	ccall((:mgl_add_range_val,mgllib_name), Nothing, (Ptr{Nothing},Cchar,Cdouble,Cdouble), gr, dir, v1, v2)
end

function SetRange(ops::plotOpStack, dir::Char, v1::Number, v2::Number)
	push!(ops, gr->SetRange(gr, dir, v1, v2))
end

function SetRange(gr::mglGraph, dir::Char, v1::Number, v2::Number)

    	ccall((:mgl_set_range_val,mgllib_name), Nothing, (Ptr{Nothing},Cchar,Cdouble,Cdouble), gr, dir, v1, v2)
end

function SetRange(ops::plotOpStack, dir::Char, dat::Array, add::Bool=false)
	push!(ops, gr->SetRange(gr, dir, dat, add))
end

function SetRange(gr::mglGraph, dir::Char, dat::Array, add::Bool=false)
  datDat = mglData(dat)

     	ccall((:mgl_set_range_val,mgllib_name), Nothing, (Ptr{Nothing},Cchar,Ptr{Nothing},Cint), gr, dir,  datDat.data, add)
end

function SetRanges(ops::plotOpStack, x1::Number, x2::Number, y1::Number, y2::Number, z1::Number=0.0, z2::Number=0.0)
	push!(ops, gr->SetRanges(gr, x1, x2, y1, y2, z1, z2))
end

function SetRanges(gr::mglGraph, x1::Number, x2::Number, y1::Number, y2::Number, z1::Number=0.0, z2::Number=0.0)

    	ccall((:mgl_set_ranges,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Cdouble,Cdouble,Cdouble,Cdouble,Cdouble), gr, x1, x2, y1, y2, z1, z2)
end

function SetRanges(ops::plotOpStack, p1::mglPoint, p2::mglPoint)
	push!(ops, gr->SetRanges(gr, p1, p2))
end


function SetAutoRanges(ops::plotOpStack, x1::Number, x2::Number, y1::Number=0.0, y2::Number=0.0, z1::Number=0.0, z2::Number=0.0, c1::Number=0.0, c2::Number=0.0)
	push!(ops, gr->SetAutoRanges(gr, x1, x2, y1, y2, z1, z2, c1, c2))
end

function SetAutoRanges(gr::mglGraph, x1::Number, x2::Number, y1::Number=0.0, y2::Number=0.0, z1::Number=0.0, z2::Number=0.0, c1::Number=0.0, c2::Number=0.0)

    	ccall((:mgl_set_auto_ranges,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Cdouble,Cdouble,Cdouble,Cdouble,Cdouble,Cdouble,Cdouble), gr, x1, x2, y1, y2, z1, z2, c1, c2)
end

function SetAutoRanges(ops::plotOpStack, p1::mglPoint, p2::mglPoint)
	push!(ops, gr->SetAutoRanges(gr, p1, p2))
end

function SetAutoRanges(gr::mglGraph, p1::mglPoint, p2::mglPoint)

    	ccall((:mgl_set_auto_ranges,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal), gr, p1.x, p2.x, p1.y, p2.y, p1.z, p2.z, p1.c, p2.c)
end

function SetOrigin(ops::plotOpStack, p::mglPoint)
	push!(ops, gr->SetOrigin(gr, p))
end

function SetOrigin(gr::mglGraph, p::mglPoint)

    	ccall((:mgl_set_origin,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal), gr, p.x, p.y, p.z)
end

function SetOrigin(ops::plotOpStack, x0::Number, y0::Number, z0::Number=mglNaN)
	push!(ops, gr->SetOrigin(gr, x0, y0, z0))
end

function SetOrigin(gr::mglGraph, x0::Number, y0::Number, z0::Number=mglNaN)

    	ccall((:mgl_set_origin,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Cdouble,Cdouble), gr, x0, y0, z0)
end

function SetFunc(ops::plotOpStack,EqX::String,EqY::String,EqZ::String="z",EqA::String="a")
	push!(ops, gr->SetFunc(gr,EqX,EqY,EqZ,EqA))
end

function SetFunc(gr::mglGraph,EqX::String,EqY::String,EqZ::String="z",EqA::String="a")

    	ccall((:mgl_set_func,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$EqX\0"),pointer("$EqY\0"),pointer("$EqZ\0"),pointer("$EqA\0"))
end

function SetCoor(ops::plotOpStack, how::Int)
	push!(ops, gr->SetCoor(gr, how))
end

function SetCoor(gr::mglGraph, how::Int)

    	ccall((:mgl_set_coor,mgllib_name), Nothing, (Ptr{Nothing},Cint), gr, how)
end

function Ternary(ops::plotOpStack, val::Int)
	push!(ops, gr->Ternary(gr, val))
end

function Ternary(gr::mglGraph, val::Int)

    	ccall((:mgl_set_ternary,mgllib_name), Nothing, (Ptr{Nothing},Cint), gr, val)
end

function SetTickLen(ops::plotOpStack, len::Number, stt::Number=1.0)
	push!(ops, gr->SetTickLen(gr, len, stt))
end

function SetTickLen(gr::mglGraph, len::Number, stt::Number=1.0)

    	ccall((:mgl_set_tick_len,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Cdouble), gr, len, stt)
end

function SetTickShift(gr::mglGraph, dx::Number=0.0, dy::Number=0.0, dz::Number=0.0, dc::Number=0.0)
	ccall((:mgl_set_tick_shift,mgllib_name), Nothing, (Ptr{Nothing},Cdouble, Cdouble, Cdouble, Cdouble), gr, dx, dy, dz, dc)
end

function SetTickRotate(gr::mglGraph, rot::Bool)

	ccall((:mgl_set_tick_rotate,mgllib_name), Nothing, (Ptr{Nothing},Bool), gr, rot)
end

function SetTickSkip(gr::mglGraph, skip::Bool)

	ccall((:mgl_set_tick_skip,mgllib_name), Nothing, (Ptr{Nothing},Bool), gr, skip)
end

function SetOriginTick(gr::mglGraph, orgt::Bool)

	ccall((:mgl_set_flag,mgllib_name), Nothing, (Ptr{Nothing},Bool,Cuint), gr, orgt, MGL_NO_ORIGIN)
end

function SetTimeUTC(gr::mglGraph, tutc::Bool)

	ccall((mgl_set_flag,mgllib_name), Nothing, (Ptr{Nothing},Bool,Cuint), gr, tutc, MGL_USE_GMTIME)
end

function SetTicksVal(ops::plotOpStack, dir::Char, val::Array, lbl::String, add::Bool=false)
	push!(ops, gr->SetTicksVal(gr, dir, val, lbl, add))
end

function SetTicksVal(gr::mglGraph, dir::Char, val::Array, lbl::String, add::Bool=false)
	valDat = mglData(val)
	ccall((:mgl_set_ticks_val,mgllib_name), Nothing, (Ptr{Nothing}, Cchar, Ptr{Nothing}, Ptr{Cchar}, Cint), gr, dir, valDat.data, pointer("$lbl\0"), add)
end

function SetTicksVal(gr::mglGraph, dir::Char, val::Dict{T, String}, add::Bool=false) where T<:Real
    lblStr = ""
    for str in values(val)
	 lblStr *= str*"\n"
    end
    SetTicksVal(gr, dir, collect(keys(val)), lblStr, add)
end

function SetAxisStl(ops::plotOpStack,stl::String="k",tck::String=0,sub::String=0)
	push!(ops, gr->SetAxisStl(gr,stl,tck,sub))
end

function SetAxisStl(gr::mglGraph,stl::String="k",tck::String=0,sub::String=0)

    	ccall((:mgl_set_axis_stl,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$stl\0"),pointer("$tck\0"),pointer("$sub\0"))
end

function SetTicks(ops::plotOpStack, dir::Char, d::Number=0.0, ns::Int=0, org::Number=mglNaN,factor::String="")
	push!(ops, gr->SetTicks(gr, dir, d, ns, org,factor))
end

function SetTicks(gr::mglGraph, dir::Char, d::Number=0.0, ns::Int=0, org::Number=mglNaN, factor::String="")

    	ccall((:mgl_set_ticks, mgllib_name), Nothing, (Ptr{Nothing},Cchar,Cdouble,Cint,Cdouble,Ptr{Cwchar_t}), gr, dir, d, ns, org, pointer("$factor\0"))
end

function Adjust(ops::plotOpStack,dir::String="xyzc")
	push!(ops, gr->Adjust(gr,dir))
end

function Adjust(gr::mglGraph,dir::String="xyzc")

    	ccall((:mgl_adjust_ticks,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar}), gr,pointer("$dir\0"))
end

function SetTuneTicks(ops::plotOpStack, tune::Int, fact_pos::Number=1.15)
	push!(ops, gr->SetTuneTicks(gr, tune, fact_pos))
end

function SetTuneTicks(gr::mglGraph, tune::Int, fact_pos::Number=1.15)

    	ccall((:mgl_tune_ticks,mgllib_name), Nothing, (Ptr{Nothing},Cint,Cdouble), gr, tune, fact_pos)
end

function SubPlot(ops::plotOpStack, nx::Int, ny::Int,m::Int,style::String="<>_^", dx::Number=0.0, dy::Number=0.0)
	push!(ops, gr->SubPlot(gr, nx, ny,m,style, dx, dy))
end

function SubPlot(gr::mglGraph, nx::Int, ny::Int,m::Int,style::String="<>_^", dx::Number=0.0, dy::Number=0.0)

    	ccall((:mgl_subplot_d,mgllib_name), Nothing, (Ptr{Nothing},Cint,Cint,Cint,Ptr{Cchar},Cdouble,Cdouble), gr, nx, ny, m, style, dx, dy)
end

function MultiPlot(ops::plotOpStack, nx::Int, ny::Int,m::Int, dx::Int, dy::Int,style::String="<>_^")
	push!(ops, gr->MultiPlot(gr, nx, ny,m, dx, dy,style))
end

function MultiPlot(gr::mglGraph, nx::Int, ny::Int,m::Int, dx::Int, dy::Int,style::String="<>_^")

    	ccall((:mgl_multiplot,mgllib_name), Nothing, (Ptr{Nothing},Cint,Cint,Cint,Cint,Cint,Ptr{Cchar}), gr, nx, ny, m, dx, dy,pointer("$style\0"))
end

function Aspect(ops::plotOpStack, Ax::Number, Ay::Number,Az::Number=1.0)
	push!(ops, gr->Aspect(gr, Ax, Ay,Az))
end

function Aspect(gr::mglGraph, Ax::Number, Ay::Number,Az::Number=1.0)

    	ccall((:mgl_aspect,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Cdouble,Cdouble), gr, Ax, Ay, Az)
end

function Rotate(ops::plotOpStack, TetX::Number, TetZ::Number=0.0,TetY::Number=0.0)
	push!(ops, gr->Rotate(gr, TetX, TetZ,TetY))
end

function Rotate(gr::mglGraph, TetX::Number, TetZ::Number=0.0,TetY::Number=0.0)

    	ccall((:mgl_rotate,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Cdouble,Cdouble), gr, TetX, TetZ, TetY)
end

function RotateN(ops::plotOpStack, Tet::Number, x::Number,y::Number,z::Number)
	push!(ops, gr->RotateN(gr, Tet, x,y,z))
end

function RotateN(gr::mglGraph, Tet::Number, x::Number,y::Number,z::Number)

    	ccall((:mgl_rotate_vector,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Cdouble,Cdouble,Cdouble), gr, Tet, x, y, z)
end

function Perspective(ops::plotOpStack, val::Number)
	push!(ops, gr->Perspective(gr, val))
end

function Perspective(gr::mglGraph, val::Number)

    	ccall((:mgl_perspective,mgllib_name), Nothing, (Ptr{Nothing},Cdouble), gr, val)
end

function View(ops::plotOpStack, TetX::Number, TetZ::Number=0.0,TetY::Number=0.0)
	push!(ops, gr->View(gr, TetX, TetZ,TetY))
end

function View(gr::mglGraph, TetX::Number, TetZ::Number=0.0,TetY::Number=0.0)

    	ccall((:mgl_view,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Cdouble,Cdouble), gr, TetX, TetZ, TetY)
end

function ViewAsRotate(ops::plotOpStack, TetZ::Number, TetX::Number,TetY::Number=0.0)
	push!(ops, gr->ViewAsRotate(gr, TetZ, TetX,TetY))
end

function ViewAsRotate(gr::mglGraph, TetZ::Number, TetX::Number,TetY::Number=0.0)

    	ccall((:mgl_view,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Cdouble,Cdouble), gr, -TetX, -TetZ, -TetY)
end

function Zoom(ops::plotOpStack, x1::Number, y1::Number, x2::Number, y2::Number)
	push!(ops, gr->Zoom(gr, x1, y1, x2, y2))
end

function Zoom(gr::mglGraph, x1::Number, y1::Number, x2::Number, y2::Number)

	ccall((:mgl_zoom, mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Cdouble,Cdouble,Cdouble), gr, x1, y1, x2, y2)
end

function SetSize(ops::plotOpStack, width::Int, height::Int)
	push!(ops, gr->SetSize(gr, width, height))
end

function SetSize(gr::mglGraph, width::Int, height::Int)

    	ccall((:mgl_set_size,mgllib_name), Nothing, (Ptr{Nothing},Cint,Cint), gr, width, height)
end

function SetQuality(ops::plotOpStack, qual::Int=MGL_DRAW_NORM)
	push!(ops, gr->SetQuality(gr, qual))
end

function SetQuality(gr::mglGraph, qual::Int=MGL_DRAW_NORM)

    	ccall((:mgl_set_quality,mgllib_name), Nothing, (Ptr{Nothing},Cint), gr, qual)
end

function StartGroup(ops::plotOpStack,name::String)
	push!(ops, gr->StartGroup(gr,name))
end

function StartGroup(gr::mglGraph,name::String)

    	ccall((:mgl_start_group,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar}), gr,pointer("$name\0"))
end

function Highlight(ops::plotOpStack, id::Int)
	push!(ops, gr->Highlight(gr, id))
end

function Highlight(gr::mglGraph, id::Int)

    	ccall((:mgl_highlight,mgllib_name), Nothing, (Ptr{Nothing},Cint), gr, id)
end

function ShowImage(ops::plotOpStack,viewer::String, keep::Bool=false)
	push!(ops, gr->ShowImage(gr,viewer, keep))
end

function ShowImage(gr::mglGraph,viewer::String, keep::Bool=false)

    	ccall((:mgl_show_image,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Bool), gr,pointer("$viewer\0"), keep)
end

function WriteFrame(ops::plotOpStack,fname::String=0,descr::String="")
	push!(ops, gr->WriteFrame(gr,fname,descr))
end

function WriteFrame(gr::mglGraph,fname::String=0,descr::String="")

    	ccall((:mgl_write_frame,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0"),pointer("$descr\0"))
end

function WritePNG(ops::plotOpStack,fname::String,descr::String="")
	push!(ops, gr->WritePNG(gr,fname,descr))
end

function WritePNG(gr::mglGraph,fname::String,descr::String="")

	ccall((:mgl_write_png,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0"),pointer("$descr\0"))
end

function WriteJPEG(ops::plotOpStack,fname::String,descr::String="")
	push!(ops, gr->WriteJPEG(gr,fname,descr))
end

function WriteJPEG(gr::mglGraph,fname::String,descr::String="")

    	ccall((:mgl_write_jpg,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0"),pointer("$descr\0"))
end

function WriteBMP(ops::plotOpStack,fname::String,descr::String="")
	push!(ops, gr->WriteBMP(gr,fname,descr))
end

function WriteBMP(gr::mglGraph,fname::String,descr::String="")

    	ccall((:mgl_write_bmp,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0"),pointer("$descr\0"))
end

function WriteTGA(ops::plotOpStack,fname::String,descr::String="")
	push!(ops, gr->WriteTGA(gr,fname,descr))
end

function WriteTGA(gr::mglGraph,fname::String,descr::String="")

    	ccall((:mgl_write_tga,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0"),pointer("$descr\0"))
end

function WriteEPS(ops::plotOpStack,fname::String,descr::String="")
	push!(ops, gr->WriteEPS(gr,fname,descr))
end

function WriteEPS(gr::mglGraph,fname::String,descr::String="")

    	ccall((:mgl_write_eps,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0"),pointer("$descr\0"))
end

function WriteTEX(ops::plotOpStack,fname::String,descr::String="")
	push!(ops, gr->WriteTEX(gr,fname,descr))
end

function WriteTEX(gr::mglGraph,fname::String,descr::String="")

    	ccall((:mgl_write_tex,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0"),pointer("$descr\0"))
end

function WriteBPS(ops::plotOpStack,fname::String,descr::String="")
	push!(ops, gr->WriteBPS(gr,fname,descr))
end

function WriteBPS(gr::mglGraph,fname::String,descr::String="")

    	ccall((:mgl_write_bps,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0"),pointer("$descr\0"))
end

function WriteSVG(ops::plotOpStack,fname::String,descr::String="")
	push!(ops, gr->WriteSVG(gr,fname,descr))
end

function WriteSVG(gr::mglGraph,fname::String,descr::String="")

    	ccall((:mgl_write_svg,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0"),pointer("$descr\0"))
end

function WriteGIF(ops::plotOpStack,fname::String,descr::String="")
	push!(ops, gr->WriteGIF(gr,fname,descr))
end

function WriteGIF(gr::mglGraph,fname::String,descr::String="")

    	ccall((:mgl_write_gif,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0"),pointer("$descr\0"))
end

function WriteJSON(ops::plotOpStack,fname::String="",descr::String="")
	push!(ops, gr->WriteJSON(gr,fname,descr))
end

function WriteJSON(gr::mglGraph,fname::String="",descr::String="")
			ccall((:mgl_write_json,mgllib_name), Nothing, (Ptr{Nothing}, Ptr{Cchar}, Ptr{Cchar}), gr, pointer("$fname\0"), pointer("$descr\0"))
end

function WriteOBJ(ops::plotOpStack,fname::String,descr::String="",use_png::Bool=true)
	push!(ops, gr->WriteOBJ(gr,fname,descr,use_png))
end

function WriteOBJ(gr::mglGraph,fname::String,descr::String="",use_png::Bool=true)

    	ccall((:mgl_write_obj,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar},Bool), gr,pointer("$fname\0"),pointer("$descr\0"), use_png)
end

function WriteOBJold(ops::plotOpStack,fname::String,descr::String="",use_png::Bool=true)
	push!(ops, gr->WriteOBJold(gr,fname,descr,use_png))
end

function WriteOBJold(gr::mglGraph,fname::String,descr::String="",use_png::Bool=true)

    	ccall((:mgl_write_obj_old,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar},Bool), gr,pointer("$fname\0"),pointer("$descr\0"), use_png)
end

function WriteXYZ(ops::plotOpStack,fname::String,descr::String="")
	push!(ops, gr->WriteXYZ(gr,fname,descr))
end

function WriteXYZ(gr::mglGraph,fname::String,descr::String="")

    	ccall((:mgl_write_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0"),pointer("$descr\0"))
end

function WriteSTL(ops::plotOpStack,fname::String,descr::String="")
	push!(ops, gr->WriteSTL(gr,fname,descr))
end

function WriteSTL(gr::mglGraph,fname::String,descr::String="")

    	ccall((:mgl_write_stl,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0"),pointer("$descr\0"))
end

function WriteOFF(ops::plotOpStack,fname::String,descr::String="", colored::Bool=false)
	push!(ops, gr->WriteOFF(gr,fname,descr, colored))
end

function WriteOFF(gr::mglGraph,fname::String,descr::String="", colored::Bool=false)

    	ccall((:mgl_write_off,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar},Bool), gr,pointer("$fname\0"),pointer("$descr\0"),colored)
end

function WritePRC(ops::plotOpStack,fname::String,descr::String="",make_pdf::Bool=true)
	push!(ops, gr->WritePRC(gr,fname,descr,make_pdf))
end

function WritePRC(gr::mglGraph,fname::String,descr::String="",make_pdf::Bool=true)

    	ccall((:mgl_write_prc,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar},Bool), gr,pointer("$fname\0"),pointer("$descr\0"), make_pdf)
end

function DelFrame(ops::plotOpStack, i::Int)
	push!(ops, gr->DelFrame(gr, i))
end

function DelFrame(gr::mglGraph, i::Int)

    	ccall((:mgl_del_frame,mgllib_name), Nothing, (Ptr{Nothing},Cint), gr, i)
end

function GetFrame(ops::plotOpStack, i::Int)
	push!(ops, gr->GetFrame(gr, i))
end

function GetFrame(gr::mglGraph, i::Int)

    	ccall((:mgl_get_frame,mgllib_name), Nothing, (Ptr{Nothing},Cint), gr, i)
end

function SetFrame(ops::plotOpStack, i::Int)
	push!(ops, gr->SetFrame(gr, i))
end

function SetFrame(gr::mglGraph, i::Int)

    	ccall((:mgl_set_frame,mgllib_name), Nothing, (Ptr{Nothing},Cint), gr, i)
end

function ShowFrame(ops::plotOpStack, i::Int)
	push!(ops, gr->ShowFrame(gr, i))
end

function ShowFrame(gr::mglGraph, i::Int)

    	ccall((:mgl_show_frame,mgllib_name), Nothing, (Ptr{Nothing},Cint), gr, i)
end

function StartGIF(ops::plotOpStack,fname::String, ms::Int=100)
	push!(ops, gr->StartGIF(gr,fname, ms))
end

function StartGIF(gr::mglGraph,fname::String, ms::Int=100)

    	ccall((:mgl_start_gif,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Cint), gr,pointer("$fname\0"),ms)
end

function ExportMGLD(ops::plotOpStack,fname::String,descr::String="")
	push!(ops, gr->ExportMGLD(gr,fname,descr))
end

function ExportMGLD(gr::mglGraph,fname::String,descr::String="")

    	ccall((:mgl_export_mgld,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fname\0"),pointer("$descr\0"))
end

function ImportMGLD(ops::plotOpStack,fname::String, add::Bool=false)
	push!(ops, gr->ImportMGLD(gr,fname, add))
end

function ImportMGLD(gr::mglGraph,fname::String, add::Bool=false)

    	ccall((:mgl_import_mgld,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Bool), gr,pointer("$fname\0"), add)
end

function Clf(ops::plotOpStack, r::Number, g::Number, b::Number)
	push!(ops, gr->Clf(gr, r, g, b))
end

function Clf(gr::mglGraph, r::Number, g::Number, b::Number)

    	ccall((:mgl_clf_rgb,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Cdouble,Cdouble), gr, r, g, b)
end

function Clf(ops::plotOpStack,col::String)
	push!(ops, gr->Clf(gr,col))
end

function Clf(gr::mglGraph,col::String)

    	ccall((:mgl_clf_str,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar}), gr,pointer("$col\0"))
end

function Clf(ops::plotOpStack, col::Char)
	push!(ops, gr->Clf(gr, col))
end

function Clf(gr::mglGraph, col::Char)

    	ccall((:mgl_clf_str,mgllib_name), Nothing, (Ptr{Nothing},Cchar), gr, col)
end

function Mark(ops::plotOpStack, p::mglPoint,mark::String)
	push!(ops, gr->Mark(gr, p,mark))
end

function Mark(gr::mglGraph, p::mglPoint,mark::String)

    	ccall((:mgl_mark,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,Ptr{Cchar}), gr, p.x, p.y, p.z,pointer("$mark\0"))
end

function Line(ops::plotOpStack, p1::mglPoint, p2::mglPoint, pen::String="B", n::Int=2)
	push!(ops, gr->Line(gr, p1, p2, pen, n))
end

function Line(gr::mglGraph, p1::mglPoint, p2::mglPoint, pen::String="B", n::Int=2)

    	ccall((:mgl_line,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,mreal,mreal,mreal,Ptr{Cchar},Cint), gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z,pointer("$pen\0"), n)
end

function Curve(ops::plotOpStack, p1::mglPoint, d1::mglPoint, p2::mglPoint, d2::mglPoint,pen::String="B", n::Int=100)
	push!(ops, gr->Curve(gr, p1, d1, p2, d2,pen, n))
end

function Curve(gr::mglGraph, p1::mglPoint, d1::mglPoint, p2::mglPoint, d2::mglPoint,pen::String="B", n::Int=100)

    	ccall((:mgl_curve,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,Ptr{Cchar},Cint), gr, p1.x, p1.y, p1.z, d1.x, d1.y, d1.z, p2.x, p2.y, p2.z, d2.x, d2.y, d2.z,pointer("$pen\0"), n)
end

function Error(ops::plotOpStack, p::mglPoint, e::mglPoint,pen::String="k")
	push!(ops, gr->Error(gr, p, e,pen))
end

function Error(gr::mglGraph, p::Tuple{mreal, mreal, mreal}, e::Tuple{mreal, mreal, mreal},pen::String="k")

    	ccall((:mgl_error_box,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,mreal,mreal,mreal,Ptr{Cchar}), gr, p[1], p[2], p[3], e[1], e[2], e[3],pointer("$pen\0"))
end

function Face(ops::plotOpStack, p1::mglPoint, p2::mglPoint, p3::mglPoint, p4::mglPoint,stl::String="r")
	push!(ops, gr->Face(gr, p1, p2, p3, p4,stl))
end

function Face(gr::mglGraph, p1::mglPoint, p2::mglPoint, p3::mglPoint, p4::mglPoint,stl::String="r")

    	ccall((:mgl_face,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,Ptr{Cchar}), gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z, p3.x, p3.y, p3.z, p4.x, p4.y, p4.z,pointer("$stl\0"))
end

function FaceX(ops::plotOpStack, p::mglPoint, wy::Number, wz::Number,stl::String="w", dx::Number=0.0, dy::Number=0.0)
	push!(ops, gr->FaceX(gr, p, wy, wz,stl, dx, dy))
end

function FaceX(gr::mglGraph, p::mglPoint, wy::Number, wz::Number,stl::String="w", dx::Number=0.0, dy::Number=0.0)

    	ccall((:mgl_facex,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,Cdouble,Cdouble,Ptr{Cchar},Cdouble,Cdouble), gr, p.x, p.y, p.z, wy, wz,pointer("$stl\0"), dx, dy)
end

function FaceY(ops::plotOpStack, p::mglPoint, wx::Number, wz::Number,stl::String="w", dx::Number=0.0, dy::Number=0.0)
	push!(ops, gr->FaceY(gr, p, wx, wz,stl, dx, dy))
end

function FaceY(gr::mglGraph, p::mglPoint, wx::Number, wz::Number,stl::String="w", dx::Number=0.0, dy::Number=0.0)

    	ccall((:mgl_facey,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,Cdouble,Cdouble,Ptr{Cchar},Cdouble,Cdouble), gr, p.x, p.y, p.z, wx, wz,pointer("$stl\0"), dx, dy)
end

function FaceZ(ops::plotOpStack, p::mglPoint, wx::Number, wy::Number,stl::String="w", dx::Number=0.0, dy::Number=0.0)
	push!(ops, gr->FaceZ(gr, p, wx, wy,stl, dx, dy))
end

function FaceZ(gr::mglGraph, p::mglPoint, wx::Number, wy::Number,stl::String="w", dx::Number=0.0, dy::Number=0.0)

    	ccall((:mgl_facez,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,Cdouble,Cdouble,Ptr{Cchar},Cdouble,Cdouble), gr, p.x, p.y, p.z, wx, wy,pointer("$stl\0"), dx, dy)
end

function Drop(ops::plotOpStack, p::mglPoint, d::mglPoint, r::Number,col::String="r", shift::Number=1.0, ap::Number=1.0)
	push!(ops, gr->Drop(gr, p, d, r,col, shift, ap))
end

function Drop(gr::mglGraph, p::mglPoint, d::mglPoint, r::Number,col::String="r", shift::Number=1.0, ap::Number=1.0)

    	ccall((:mgl_drop,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,mreal,mreal,mreal,Cdouble,Ptr{Cchar},Cdouble,Cdouble), gr, p.x, p.y, p.z, d.x, d.y, d.z, r,pointer("$col\0"), shift, ap)
end

function Sphere(ops::plotOpStack, p::mglPoint, r::Number,col::String="r")
	push!(ops, gr->Sphere(gr, p, r,col))
end

function Sphere(gr::mglGraph, p::mglPoint, r::Number,col::String="r")

    	ccall((:mgl_sphere,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,Cdouble,Ptr{Cchar}), gr, p.x, p.y, p.z, r,pointer("$col\0"))
end

function Cone(ops::plotOpStack, p1::mglPoint, p2::mglPoint, r1::Number, r2::Number=-1.0,stl::String="r@")
	push!(ops, gr->Cone(gr, p1, p2, r1, r2,stl))
end

function Cone(gr::mglGraph, p1::mglPoint, p2::mglPoint, r1::Number, r2::Number=-1.0,stl::String="r@")

    	ccall((:mgl_cone,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,mreal,mreal,mreal,Cdouble,Cdouble,Ptr{Cchar}), gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z,r1,r2,pointer("$stl\0"))
end

function Ellipse(ops::plotOpStack, p1::mglPoint, p2::mglPoint, r::Number,stl::String="r")
	push!(ops, gr->Ellipse(gr, p1, p2, r,stl))
end

function Ellipse(gr::mglGraph, p1::mglPoint, p2::mglPoint, r::Number,stl::String="r")

    	ccall((:mgl_ellipse,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,mreal,mreal,mreal,Cdouble,Ptr{Cchar}), gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z, r,pointer("$stl\0"))
end

function Circle(ops::plotOpStack, p::mglPoint, r::Number,stl::String="r")
	push!(ops, gr->Circle(gr, p, r,stl))
end

function Circle(gr::mglGraph, p::mglPoint, r::Number,stl::String="r")

    	ccall((:mgl_ellipse,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,Cdouble,Ptr{Cchar}), gr, p.x, p.y, p.z, r, pointer("$stl\0"))
end

function Rhomb(ops::plotOpStack, p1::mglPoint, p2::mglPoint, r::Number,stl::String="r")
	push!(ops, gr->Rhomb(gr, p1, p2, r,stl))
end

function Rhomb(gr::mglGraph, p1::mglPoint, p2::mglPoint, r::Number,stl::String="r")

    	ccall((:mgl_rhomb,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,mreal,mreal,mreal,Cdouble,Ptr{Cchar}), gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z, r,pointer("$stl\0"))
end

function Polygon(ops::plotOpStack, p1::mglPoint, p2::mglPoint, n::Int,stl::String="r")
	push!(ops, gr->Polygon(gr, p1, p2, n,stl))
end

function Polygon(gr::mglGraph, p1::mglPoint, p2::mglPoint, n::Int,stl::String="r")

    	ccall((:mgl_polygon,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,mreal,mreal,mreal,Cint,Ptr{Cchar}), gr, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z, n,pointer("$stl\0"))
end

function Arc(ops::plotOpStack, p0::mglPoint, pr::mglPoint, p1::mglPoint, a::Number,stl::String="r")
	push!(ops, gr->Arc(gr, p0, pr, p1, a,stl))
end

function Arc(gr::mglGraph, p0::mglPoint, pr::mglPoint, p1::mglPoint, a::Number,stl::String="r")

    	ccall((:mgl_arc_ext,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,mreal,Cdouble,Ptr{Cchar}), gr, p0.x,p0.y,p0.z, pr.x,pr.y,pr.z, p1.x,p1.y,p1.z, a,pointer("$stl\0"))
end

function Arc(ops::plotOpStack, p0::mglPoint, p1::mglPoint, a::Number,stl::String="r")
	push!(ops, gr->Arc(gr, p0, p1, a,stl))
end

function Arc(gr::mglGraph, p0::mglPoint, p1::mglPoint, a::Number,stl::String="r")

    	ccall((:mgl_arc_ext,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,mreal,mreal,mreal,Cdouble,Ptr{Cchar}), gr, p0.x,p0.y,p0.z, p1.x,p1.y,p0.z, a,pointer("$stl\0"))
end

function Logo(ops::plotOpStack, w::Clong, h::Clong, rgba::String, smooth::Bool=false,opt::String="")
	push!(ops, gr->Logo(gr, w, h, rgba, smooth,opt))
end

function Logo(gr::mglGraph, w::Clong, h::Clong, rgba::String, smooth::Bool=false,opt::String="")

    	ccall((:mgl_logo,mgllib_name), Nothing, (Ptr{Nothing},Clong,Clong,Ptr{Cchar},Bool,Ptr{Cchar}), gr, w, h, rgba, smooth,pointer("$opt\0"))
end

function Logo(ops::plotOpStack,fname::String, smooth::Bool=false,opt::String="")
	push!(ops, gr->Logo(gr,fname, smooth,opt))
end

function Logo(gr::mglGraph,fname::String, smooth::Bool=false,opt::String="")

    	ccall((:mgl_logo_file,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Bool,Ptr{Cchar}), gr,pointer("$fname\0"), smooth,pointer("$opt\0"))
end

function Puts(ops::plotOpStack, p::mglPoint,text::String,font::String=":C",size::Number=-1.0)
	push!(ops, gr->Puts(gr, p,text,font,size))
end

function Puts(gr::mglGraph, p::mglPoint, text::String,font::String=":C",size::Number=-1.0)
	test_ascii(font)

    	ccall((:mgl_putsw,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,Ptr{Cwchar_t},Ptr{Cchar},Cdouble), gr, p.x, p.y, p.z, pointer("$text\0"), pointer("$font\0"), size)
end

function Puts(ops::plotOpStack, x::Number, y::Number, text::String,font::String=":AC",size::Number=-1.0)
	push!(ops, gr->Putsw(gr, x, y, text,font,size))
end

function Puts(gr::mglGraph, x::Number, y::Number, text::String,font::String=":AC",size::Number=-1.0)
	#test_ascii(font)

	#ccall((:mgl_putsw,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Cdouble,Cdouble,Ptr{Cwchar_t},Ptr{Cchar},Cdouble), gr, x, y, 0.0, pointer("$text\0"), pointer("$font\0"), size)
	Puts(gr, (x, y, 0.0), (x+1, y, 0.0), text, font,  size)
end

function Puts(ops::plotOpStack, p::mglPoint, d::mglPoint, text::String,font::String=":L", size::Number=-1.0)
	push!(ops, gr->Putsw(gr, p, d, text,font, size))
end

function Puts(gr::mglGraph, p::Tuple{mreal, mreal, mreal}, d::Tuple{mreal, mreal, mreal}, text::String,font::String=":L", size::Number=-1.0)
	test_ascii(font)
    	ccall((:mgl_puts_dir,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,mreal,mreal,mreal,Ptr{Cwchar_t},Ptr{Cchar},Cdouble), gr, p[1], p[2], p[3], d[1], d[2], d[3], pointer("$text\0"),pointer("$font\0"), size)
end

function Text(ops::plotOpStack,x::Array,y::Array,z::Array, text::String,font::String="",opt::String="")
	push!(ops, gr->Text(gr,x,y,z, text,font,opt))
end

function Text(gr::mglGraph,x::Array,y::Array,z::Array, text::String,font::String="",opt::String="")
	test_ascii(font)
	test_ascii(opt)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_text_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, pointer("$text\0"),pointer("$font\0"),pointer("$opt\0"))
end

function Text(ops::plotOpStack,x::Array,y::Array, text::String,font::String="",opt::String="")
	push!(ops, gr->Text(gr,x,y, text,font,opt))
end

function Text(gr::mglGraph,x::Array,y::Array, text::String,font::String="",opt::String="")
	test_ascii(font)
	test_ascii(opt)
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_text_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, pointer("$text\0"),pointer("$font\0"),pointer("$opt\0"))
end

function Text(ops::plotOpStack,y::Array, text::String,font::String="",opt::String="")
	push!(ops, gr->Text(gr,y, text,font,opt))
end

function Text(gr::mglGraph,y::Array, text::String,font::String="",opt::String="")
	test_ascii(font,opt)
  yDat = mglData(y)

    	ccall((:mgl_text_y,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, pointer("$text\0"),pointer("$font\0"),pointer("$opt\0"))
end

function Box(ops::plotOpStack,col::String="", ticks::Bool=true)
	push!(ops, gr->Box(gr,col, ticks))
end

function Box(gr::mglGraph,col::String="", ticks::Bool=true)

    	ccall((:mgl_box_str,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Bool), gr,pointer("$col\0"), ticks)
end

function Axis(ops::plotOpStack,dir::String="xyzt",stl::String="",opt::String="")
	push!(ops, gr->Axis(gr,dir,stl,opt))
end

function Axis(gr::mglGraph,dir::String="xyzt",stl::String="",opt::String="")

    	ccall((:mgl_axis,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$dir\0"),pointer("$stl\0"),pointer("$opt\0"))
end

function Grid(ops::plotOpStack,dir::String="xyzt",pen::String="B",opt::String="")
	push!(ops, gr->Grid(gr,dir,pen,opt))
end

function Grid(gr::mglGraph,dir::String="xyzt",pen::String="B",opt::String="")

    	ccall((:mgl_axis_grid,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$dir\0"),pointer("$pen\0"),pointer("$opt\0"))
end

function Label(ops::plotOpStack, dir::Char, text::String, pos::Number=+1.0,opt::String="")
	push!(ops, gr->Label(gr, dir, text, pos,opt))
end

function Label(gr::mglGraph, dir::Char, text::String, pos::Number=+1.0,opt::String="")
	test_ascii(opt)

    	ccall((:mgl_label,mgllib_name), Nothing, (Ptr{Nothing},Cchar,Ptr{Cwchar_t},Cdouble,Ptr{Cchar}), gr, dir, pointer("$text\0"), pos,pointer("$opt\0"))
end

function Colorbar(ops::plotOpStack,sch::String="")
	push!(ops, gr->Colorbar(gr,sch))
end

function Colorbar(gr::mglGraph,sch::String="")

    	ccall((:mgl_colorbar,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar}), gr,pointer("$sch\0"))
end

function Colorbar(ops::plotOpStack,sch::String, x::Number,y::Number,w::Number=1.0,h::Number=1.0)
	push!(ops, gr->Colorbar(gr,sch, x,y,w,h))
end

function Colorbar(gr::mglGraph,sch::String, x::Number,y::Number,w::Number=1.0,h::Number=1.0)

    	ccall((:mgl_colorbar_ext,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Cdouble,Cdouble,Cdouble,Cdouble), gr,pointer("$sch\0"), x,y,w,h)
end

function Colorbar(ops::plotOpStack,val::Array,sch::String="")
	push!(ops, gr->Colorbar(gr,val,sch))
end

function Colorbar(gr::mglGraph,val::Array,sch::String="")
  valDat = mglData(val)

    	ccall((:mgl_colorbar_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar}), gr, valDat.data,pointer("$sch\0"))
end

function Colorbar(ops::plotOpStack,val::Array,sch::String, x::Number,y::Number,w::Number=1.0,h::Number=1.0)
	push!(ops, gr->Colorbar(gr,val,sch, x,y,w,h))
end

function Colorbar(gr::mglGraph,val::Array,sch::String, x::Number,y::Number,w::Number=1.0,h::Number=1.0)
  valDat = mglData(val)

    	ccall((:mgl_colorbar_val_ext,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Cdouble,Cdouble,Cdouble), gr, valDat.data,pointer("$sch\0"), x,y,w,h)
end

function AddLegend(ops::plotOpStack, text::String,style::String)
	push!(ops, gr->AddLegend(gr, text,style))
end

function AddLegend(gr::mglGraph, text::String,style::String)
	test_ascii(style)

    	ccall((:mgl_add_legend,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cwchar_t},Ptr{Cchar}), gr, pointer("$text\0"),pointer("$style\0"))
end

function Legend(ops::plotOpStack, x::Number, y::Number,font::String="#",opt::String="")
	push!(ops, gr->Legend(gr, x, y,font,opt))
end

function Legend(gr::mglGraph, x::Number, y::Number,font::String="#",opt::String="")

    	ccall((:mgl_legend_pos,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Cdouble,Ptr{Cchar},Ptr{Cchar}), gr, x, y,pointer("$font\0"),pointer("$opt\0"))
end

function Legend(ops::plotOpStack, where::Int=3,font::String="#",opt::String="")
	push!(ops, gr->Legend(gr, where,font,opt))
end

function Legend(gr::mglGraph, where::Int=3,font::String="#",opt::String="")

    	ccall((:mgl_legend,mgllib_name), Nothing, (Ptr{Nothing},Cint,Ptr{Cchar},Ptr{Cchar}), gr, where,pointer("$font\0"),pointer("$opt\0"))
end

function clearLegend(gr::mglGraph)
	ccall((:mgl_clear_legend,mgllib_name), Nothing, (Ptr{Nothing},), gr)
end

function SetLegendMarks(ops::plotOpStack, num::Int)
	push!(ops, gr->SetLegendMarks(gr, num))
end

function SetLegendMarks(gr::mglGraph, num::Int)

    	ccall((:mgl_set_legend_marks,mgllib_name), Nothing, (Ptr{Nothing},Cint), gr, num)
end

function Plot(ops::plotOpStack,x::Array,y::Array,z::Array,pen::String="",opt::String="")
	push!(ops, gr->Plot(gr,x,y,z,pen,opt))
end

function Plot(gr::mglGraph,x::Array,y::Array,z::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_plot_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Plot(ops::plotOpStack,x::Array,y::Array,pen::String="",opt::String="")
	push!(ops, gr->Plot(gr,x,y,pen,opt))
end

function Plot(gr::mglGraph,x::Array,y::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_plot_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Plot(y::Array, pen::String="", opt::String="")
        opStack=plotOpStack()
        push!(opStack, gr->SetRange(gr, 'y', minimum(y), maximum(y)), "Set y range")
	xMin = 0
	xMax = length(y)

	push!(opStack, gr->SetRange(gr, 'x', xMin, xMax), "Set x range")
        push!(opStack, gr->Box(gr), "Box")
        push!(opStack, gr->Axis(gr), "Axes")
        push!(opStack, gr->Plot(gr, y, pen, opt), "Plot")
        return opStack
end


function Plot(x::Array, y::Array, pen::String="", opt::String="")
	opStack = plotOpStack()
	push!(opStack, gr->SetRanges(gr, minimum(x), maximum(x), minimum(y), maximum(y)), "Set ranges")
	push!(opStack, gr->Box(gr), "Box")
        push!(opStack, gr->Axis(gr), "Axes")
        push!(opStack, gr->Plot(gr, x, y, pen, opt), "Plot")
        return opStack
end


function Plot(x::Array, y::Array, z::Array, pen::String="", opt::String="")
	opStack = plotOpStack()
	push!(opStack, gr->SetRanges(gr, minimum(x), maximum(x), minimum(y), maximum(y), minimum(z), maximum(z)), "Set ranges")
	push!(opStack, gr->Box(gr), "Box")
        push!(opStack, gr->Axis(gr), "Axes")
        push!(opStack, gr->Plot(gr, x, y, z, pen, opt), "Plot")
        return opStack
end


function func2array(y::Function, xmin::Number, xmax::Number)
	ydisc = Array{Float64, 1}(undef,1000)
	for (i, x) in enumerate(xmin:((xmax-xmin)/999):xmax)
		ydisc[i] = y(x)
	end

	return ydisc
end

function Plot(y::Function, xmin::Number, xmax::Number, pen::String="", opt::String="")
	opStack = plotOpStack()
	ydisc = func2array(y, xmin, xmax)
	push!(opStack, gr->SetRanges(gr, xmin, xmax, minimum(ydisc), maximum(ydisc)), "Set Ranges")
	push!(opStack, gr->Box(gr), "Box")
	push!(opStack, gr->Axis(gr), "Axis")
	push!(opStack, gr->Plot(gr, ydisc, pen, "xrange $xmin $xmax; $opt"), "Plot")
	return opStack
end

function Plot(x::Function, y::Function, xmin::Number, xmax::Number, ymin::Number, ymax::Number, pen::String="", opt::String="")
	opStack = plotOpStack()
	ydisc = func2array(y, ymin, ymax)
	xdisc = func2array(x, xmin, xmax)
	push!(opStack, gr->SetRanges(gr, minimum(xdisc), maximum(xdisc), minimum(ydisc), maximum(ydisc)), "Set Ranges")
	push!(opStack, gr->Box(gr), "Box")
	push!(opStack, gr->Axis(gr), "Axis")
	push!(opStack, gr->Plot(gr, xdisc, ydisc, pen, "xrange $(minimum(xdisc)) $(maximum(xdisc)); yrange $(minimum(ydisc)) $(maximum(ydisc)); $opt"), "Plot")
	return opStack
end

function Plot(x::Function, y::Function, z::Function, xmin::Number, xmax::Number, ymin::Number, ymax::Number, zmin::Number, zmax::Number, pen::String="", opt::String="")
	opStack = plotOpStack()
	ydisc = func2array(y, ymin, ymax)
	xdisc = func2array(x, xmin, xmax)
	zdisc = func2array(z, zmin, zmax)
	push!(opStack, gr->SetRanges(gr, minimum(xdisc), maximum(xdisc), minimum(ydisc), maximum(ydisc), minimum(zdisc), maximum(zdisc)), "Set Ranges")
	push!(opStack, gr->Box(gr), "Box")
	push!(opStack, gr->Axis(gr), "Axis")
	push!(opStack, gr->Plot(gr, xdisc, ydisc, zdisc, pen, "xrange $(minimum(xdisc)) $(maximum(xdisc)); yrange $(minimum(ydisc)) $(maximum(ydisc)); zrange $(minimum(zdisc)) $(maximum(zdisc));  $opt"), "Plot")
	return opStack
end


function Plot(ops::plotOpStack, y::Function, xmin::Number, xmax::Number, pen::String="", opt::String="")
	push!(ops, gr->Plot(gr, y, xmin, xmax, pen, opt))
end

function Plot(ops::plotOpStack, x::Function, y::Function, xmin::Number, xmax::Number, ymin::Number, ymax::Number, pen::String="", opt::String="")
	push!(ops, gr->Plot(gr, x, y, xmin, xmax, ymin, ymax, pen, opt))
end

function Plot(ops::plotOpStack, x::Function, y::Function, z::Function, xmin::Number, xmax::Number, ymin::Number, ymax::Number, zmin::Number, zmax::Number, pen::String="", opt::String="")
	push!(ops, gr->Plot(gr, x, y, z, xmin, xmax, ymin, ymax, zmin, zmax, pen, opt))
end

function Plot(gr::mglGraph, y::Function, xmin::Number, xmax::Number, pen::String="", opt::String="")
	Plot(gr, func2array(y, xmin, xmax), pen, "xrange $xmin $xmax; $opt")
end

function Plot(gr::mglGraph, x::Function, y::Function, xmin::Number, xmax::Number, ymin::Number, ymax::Number, pen::String="", opt::String="")
	Plot(gr, func2array(x, xmin, xmax), func2array(y, ymin, ymax), pen, "xrange $xmin $xmax; yrange $ymin $ymax; $opt")
end

function Plot(gr::mglGraph, x::Function, y::Function, z::Function, xmin::Number, xmax::Number, ymin::Number, ymax::Number, zmin::Number, zmax::Number, pen::String="", opt::String="")
	Plot(gr, func2array(x, xmin, xmax), func2array(y, ymin, ymax), func2array(z, zmin, zmax), pen, "xrange $xmin $xmax; yrange $ymin $ymax; $opt")
end

function Plot(ops::plotOpStack,y::Array,pen::String="",opt::String="")
	push!(ops, gr->Plot(gr,y,pen,opt))
end

function Plot(gr::mglGraph,y::Array,pen::String="",opt::String="")

  yDat = mglData(y)
    	ccall((:mgl_plot,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr,yDat.data,pointer("$pen\0"),pointer("$opt\0"))

end

function SpaghettiPlot(gr::mglGraph,spag::AbstractVector{Vector{T}},pen::String="",opt::String="") where {T}
    skip1 = false
    if ismatch(r"legend '[^']*'", opt)
        Plot(gr, spag[1], pen, opt)
	opt=replace(opt, r"legend '[^']*]'", "")
    elseif ismatch(r"legend [^ ]*", opt)
	Plot(gr, spag[1], pen, opt)
	opt = replace(opt, r"legend [^ ]*", "")
    end

    for strand in spag
	if skip1
	    skip1 = false
	    continue
	end

	Plot(gr, strand, pen, opt)
    end
end

function SpaghettiPlot(ops::plotOpStack, spag::AbstractVector{Vector{T}},pen::String="",opt::String="") where {T}
    push!(ops, gr->SpaghettiPlot(gr,spag,pen,opt))
end

function Tape(ops::plotOpStack,x::Array,y::Array,z::Array,pen::String="",opt::String="")
	push!(ops, gr->Tape(gr,x,y,z,pen,opt))
end

function Tape(gr::mglGraph,x::Array,y::Array,z::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_tape_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Tape(ops::plotOpStack,x::Array,y::Array,pen::String="",opt::String="")
	push!(ops, gr->Tape(gr,x,y,pen,opt))
end

function Tape(gr::mglGraph,x::Array,y::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_tape_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Tape(ops::plotOpStack,y::Array,pen::String="",opt::String="")
	push!(ops, gr->Tape(gr,y,pen,opt))
end

function Tape(gr::mglGraph,y::Array,pen::String="",opt::String="")
  yDat = mglData(y)

    	ccall((:mgl_tape,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Radar(ops::plotOpStack,a::Array,pen::String="",opt::String="")
	push!(ops, gr->Radar(gr,a,pen,opt))
end

function Radar(gr::mglGraph,a::Array,pen::String="",opt::String="")
  aDat = mglData(a)

    	ccall((:mgl_radar,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, aDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Step(ops::plotOpStack,x::Array,y::Array,z::Array,pen::String="",opt::String="")
	push!(ops, gr->Step(gr,x,y,z,pen,opt))
end

function Step(gr::mglGraph,x::Array,y::Array,z::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_step_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Step(ops::plotOpStack,x::Array,y::Array,pen::String="",opt::String="")
	push!(ops, gr->Step(gr,x,y,pen,opt))
end

function Step(gr::mglGraph,x::Array,y::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_step_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Step(ops::plotOpStack,y::Array,pen::String="",opt::String="")
	push!(ops, gr->Step(gr,y,pen,opt))
end

function Step(gr::mglGraph,y::Array,pen::String="",opt::String="")
  yDat = mglData(y)

    	ccall((:mgl_step,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Tens(ops::plotOpStack,x::Array,y::Array,z::Array,c::Array,pen::String="",opt::String="")
	push!(ops, gr->Tens(gr,x,y,z,c,pen,opt))
end

function Tens(gr::mglGraph,x::Array,y::Array,z::Array,c::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  cDat = mglData(c)

    	ccall((:mgl_tens_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, cDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Tens(ops::plotOpStack,x::Array,y::Array,c::Array,pen::String="",opt::String="")
	push!(ops, gr->Tens(gr,x,y,c,pen,opt))
end

function Tens(gr::mglGraph,x::Array,y::Array,c::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  cDat = mglData(c)

    	ccall((:mgl_tens_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, cDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Tens(ops::plotOpStack,y::Array,c::Array,pen::String="",opt::String="")
	push!(ops, gr->Tens(gr,y,c,pen,opt))
end

function Tens(gr::mglGraph,y::Array,c::Array,pen::String="",opt::String="")
  yDat = mglData(y)
  cDat = mglData(c)

    	ccall((:mgl_tens,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, cDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Area(ops::plotOpStack,x::Array,y::Array,z::Array,pen::String="",opt::String="")
	push!(ops, gr->Area(gr,x,y,z,pen,opt))
end

function Area(gr::mglGraph,x::Array,y::Array,z::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_area_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Area(y::Array, pen::String="", opt::String="")
        opStack=plotOpStack()
        push!(opStack, gr->SetRange(gr, 'y', minimum(y), maximum(y)), "Set y range")
	xMin = 0
	xMax = length(y)

	push!(opStack, gr->SetRange(gr, 'x', xMin, xMax), "Set x range")
        push!(opStack, gr->Box(gr), "Box")
        push!(opStack, gr->Axis(gr), "Axes")
        push!(opStack, gr->Area(gr, y, pen, opt), "Plot")
        return opStack
end

function Area(ops::plotOpStack,x::Array,y::Array,pen::String="",opt::String="")
	push!(ops, gr->Area(gr,x,y,pen,opt))
end

function Area(gr::mglGraph,x::Array,y::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_area_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Area(ops::plotOpStack,y::Array,pen::String="",opt::String="")
	push!(ops, gr->Area(gr,y,pen,opt))
end

function Area(gr::mglGraph,y::Array,pen::String="",opt::String="")
  yDat = mglData(y)

    	ccall((:mgl_area,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Region(ops::plotOpStack,y1::Array,y2::Array,pen::String="",opt::String="")
	push!(ops, gr->Region(gr,y1,y2,pen,opt))
end

function Region(gr::mglGraph,y1::Array,y2::Array,pen::String="",opt::String="")
  y1Dat = mglData(y1)
  y2Dat = mglData(y2)

    	ccall((:mgl_region,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, y1Dat.data, y2Dat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Region(ops::plotOpStack,x::Array,y1::Array,y2::Array,pen::String="",opt::String="")
	push!(ops, gr->Region(gr,x,y1,y2,pen,opt))
end

function Region(gr::mglGraph,x::Array,y1::Array,y2::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  y1Dat = mglData(y1)
  y2Dat = mglData(y2)

    	ccall((:mgl_region_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, y1Dat.data, y2Dat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Region(ops::plotOpStack,x1::Array,y1::Array,z1::Array,x2::Array,y2::Array,z2::Array,pen::String="",opt::String="")
	push!(ops, gr->Region(gr,x1,y1,z1,x2,y2,z2,pen,opt))
end

function Region(gr::mglGraph,x1::Array,y1::Array,z1::Array,x2::Array,y2::Array,z2::Array,pen::String="",opt::String="")
  x1Dat = mglData(x1)
  y1Dat = mglData(y1)
  z1Dat = mglData(z1)
  x2Dat = mglData(x2)
  y2Dat = mglData(y2)
  z2Dat = mglData(z2)

    	mgl_region_3ccall((:d,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, x1Dat.data, y1Dat.data, z1Dat.data, x2Dat.data, y2Dat.data, z2Dat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Region(ops::plotOpStack,x1::Array,y1::Array,x2::Array,y2::Array,pen::String="",opt::String="")
	push!(ops, gr->Region(gr,x1,y1,x2,y2,pen,opt))
end

function Region(gr::mglGraph,x1::Array,y1::Array,x2::Array,y2::Array,pen::String="",opt::String="")
  x1Dat = mglData(x1)
  y1Dat = mglData(y1)
  x2Dat = mglData(x2)
  y2Dat = mglData(y2)

    	mgl_region_3ccall((:d,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, x1Dat.data, y1Dat.data, NULL, x2Dat.data, y2Dat.data, NULL,pointer("$pen\0"),pointer("$opt\0"))
end

function Stem(ops::plotOpStack,x::Array,y::Array,z::Array,pen::String="",opt::String="")
	push!(ops, gr->Stem(gr,x,y,z,pen,opt))
end

function Stem(gr::mglGraph,x::Array,y::Array,z::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_stem_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Stem(ops::plotOpStack,x::Array,y::Array,pen::String="",opt::String="")
	push!(ops, gr->Stem(gr,x,y,pen,opt))
end

function Stem(gr::mglGraph,x::Array,y::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_stem_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Stem(ops::plotOpStack,y::Array,pen::String="",opt::String="")
	push!(ops, gr->Stem(gr,y,pen,opt))
end

function Stem(gr::mglGraph,y::Array,pen::String="",opt::String="")
  yDat = mglData(y)

    	ccall((:mgl_stem,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Bars(ops::plotOpStack,x::Array,y::Array,z::Array,pen::String="",opt::String="")
	push!(ops, gr->Bars(gr,x,y,z,pen,opt))
end

function Bars(gr::mglGraph,x::Array,y::Array,z::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_bars_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Bars(ops::plotOpStack,x::Array,y::Array,pen::String="",opt::String="")
	push!(ops, gr->Bars(gr,x,y,pen,opt))
end

function Bars(gr::mglGraph,x::Array,y::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_bars_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Bars(ops::plotOpStack,y::Array,pen::String="",opt::String="")
	push!(ops, gr->Bars(gr,y,pen,opt))
end

function Bars(y::Array,pen::String="",opt::String="")
	ops = plotOpStack()
	SetRanges(ops, 0, length(y), min(0, minimum(y)), maximum(y))
	Bars(ops, y, pen, opt)
	Axis(ops)
	MathGL.Box(ops)

	return ops
end


function Bars(gr::mglGraph,y::Array,pen::String="",opt::String="")
  yDat = mglData(y)

    	ccall((:mgl_bars,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Barh(ops::plotOpStack,y::Array,v::Array,pen::String="",opt::String="")
	push!(ops, gr->Barh(gr,y,v,pen,opt))
end

function Barh(gr::mglGraph,y::Array,v::Array,pen::String="",opt::String="")
  yDat = mglData(y)
  vDat = mglData(v)

    	ccall((:mgl_barh_yx,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, vDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Barh(ops::plotOpStack,v::Array,pen::String="",opt::String="")
	push!(ops, gr->Barh(gr,v,pen,opt))
end

function Barh(gr::mglGraph,v::Array,pen::String="",opt::String="")
  vDat = mglData(v)

    	ccall((:mgl_barh,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Chart(ops::plotOpStack,a::Array,colors::String="",opt::String="")
	push!(ops, gr->Chart(gr,a,colors,opt))
end

function Chart(gr::mglGraph,a::Array,colors::String="",opt::String="")
  aDat = mglData(a)

    	ccall((:mgl_Cchart,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, aDat.data,pointer("$colors\0"),pointer("$opt\0"))
end

function OHLC(ops::plotOpStack,x::Array,open::Array,high::Array,low::Array,close::Array,pen::String="",opt::String="")
	push!(ops, gr->OHLC(gr,x,open,high,low,close,pen,opt))
end

function OHLC(gr::mglGraph,x::Array,open::Array,high::Array,low::Array,close::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  openDat = mglData(open)
  highDat = mglData(high)
  lowDat = mglData(low)
  closeDat = mglData(close)

    	ccall((:mgl_ohlc_x,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, openDat.data, highDat.data, lowDat.data, closeDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function OHLC(ops::plotOpStack,open::Array,high::Array,low::Array,close::Array,pen::String="",opt::String="")
	push!(ops, gr->OHLC(gr,open,high,low,close,pen,opt))
end

function OHLC(gr::mglGraph,open::Array,high::Array,low::Array,close::Array,pen::String="",opt::String="")
  openDat = mglData(open)
  highDat = mglData(high)
  lowDat = mglData(low)
  closeDat = mglData(close)

    	ccall((:mgl_ohlc,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, openDat.data, highDat.data, lowDat.data, closeDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function BoxPlot(ops::plotOpStack,x::Array,y::Array,pen::String="",opt::String="")
	push!(ops, gr->BoxPlot(gr,x,y,pen,opt))
end

function BoxPlot(gr::mglGraph,x::Array,y::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_boxplot_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function BoxPlot(ops::plotOpStack,y::Array,pen::String="",opt::String="")
	push!(ops, gr->BoxPlot(gr,y,pen,opt))
end

function BoxPlot(gr::mglGraph,y::Array,pen::String="",opt::String="")
  yDat = mglData(y)

    	ccall((:mgl_boxplot,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Candle(ops::plotOpStack,x::Array,v1::Array,v2::Array,y1::Array,y2::Array,pen::String="",opt::String="")
	push!(ops, gr->Candle(gr,x,v1,v2,y1,y2,pen,opt))
end

function Candle(gr::mglGraph,x::Array,v1::Array,v2::Array,y1::Array,y2::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  v1Dat = mglData(v1)
  v2Dat = mglData(v2)
  y1Dat = mglData(y1)
  y2Dat = mglData(y2)

    	ccall((:mgl_candle_xyv,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, v1Dat.data, v2Dat.data, y1Dat.data, y2Dat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Candle(ops::plotOpStack,v1::Array,v2::Array,y1::Array,y2::Array,pen::String="",opt::String="")
	push!(ops, gr->Candle(gr,v1,v2,y1,y2,pen,opt))
end

function Candle(gr::mglGraph,v1::Array,v2::Array,y1::Array,y2::Array,pen::String="",opt::String="")
  v1Dat = mglData(v1)
  v2Dat = mglData(v2)
  y1Dat = mglData(y1)
  y2Dat = mglData(y2)

    	ccall((:mgl_candle_yv,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, v1Dat.data, v2Dat.data, y1Dat.data, y2Dat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Candle(ops::plotOpStack,v1::Array,v2::Array,pen::String="",opt::String="")
	push!(ops, gr->Candle(gr,v1,v2,pen,opt))
end

function Candle(gr::mglGraph,v1::Array,v2::Array,pen::String="",opt::String="")
  v1Dat = mglData(v1)
  v2Dat = mglData(v2)

    	ccall((:mgl_candle_yv,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Nothing,Nothing,Ptr{Cchar},Ptr{Cchar}), gr, v1Dat.data, v2Dat.data, NULL, NULL,pointer("$pen\0"),pointer("$opt\0"))
end

function Candle(ops::plotOpStack,y::Array,y1::Array,y2::Array,pen::String="",opt::String="")
	push!(ops, gr->Candle(gr,y,y1,y2,pen,opt))
end

function Candle(gr::mglGraph,y::Array,y1::Array,y2::Array,pen::String="",opt::String="")
  yDat = mglData(y)
  y1Dat = mglData(y1)
  y2Dat = mglData(y2)

    	ccall((:mgl_candle,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, y1Dat.data, y2Dat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Candle(ops::plotOpStack,y::Array,pen::String="",opt::String="")
	push!(ops, gr->Candle(gr,y,pen,opt))
end

function Candle(gr::mglGraph,y::Array,pen::String="",opt::String="")
  yDat = mglData(y)

    	ccall((:mgl_candle,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Nothing,Nothing,Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, NULL, NULL,pointer("$pen\0"),pointer("$opt\0"))
end

function Cones(ops::plotOpStack,x::Array,y::Array,z::Array,pen::String="@",opt::String="")
	push!(ops, gr->Cones(gr,x,y,z,pen,opt))
end

function Cones(gr::mglGraph,x::Array,y::Array,z::Array,pen::String="@",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_cones_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Cones(ops::plotOpStack,x::Array,z::Array,pen::String="@",opt::String="")
	push!(ops, gr->Cones(gr,x,z,pen,opt))
end

function Cones(gr::mglGraph,x::Array,z::Array,pen::String="@",opt::String="")
  xDat = mglData(x)
  zDat = mglData(z)

    	ccall((:mgl_cones_xz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, zDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Cones(ops::plotOpStack,z::Array,pen::String="@",opt::String="")
	push!(ops, gr->Cones(gr,z,pen,opt))
end

function Cones(gr::mglGraph,z::Array,pen::String="@",opt::String="")
  zDat = mglData(z)

    	ccall((:mgl_cones,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Error(ops::plotOpStack,y::Array,ey::Array,pen::String="",opt::String="")
	push!(ops, gr->Error(gr,y,ey,pen,opt))
end

function Error(gr::mglGraph,y::Array,ey::Array,pen::String="",opt::String="")
  yDat = mglData(y)
  eyDat = mglData(ey)

    	ccall((:mgl_error,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, eyDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Error(ops::plotOpStack,x::Array,y::Array,ey::Array,pen::String="",opt::String="")
	push!(ops, gr->Error(gr,x,y,ey,pen,opt))
end

function Error(gr::mglGraph,x::Array,y::Array,ey::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  eyDat = mglData(ey)

    	ccall((:mgl_error_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, eyDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Error(ops::plotOpStack,x::Array,y::Array,ex::Array,ey::Array,pen::String="",opt::String="")
	push!(ops, gr->Error(gr,x,y,ex,ey,pen,opt))
end

function Error(gr::mglGraph,x::Array,y::Array,ex::Array,ey::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  exDat = mglData(ex)
  eyDat = mglData(ey)

    	ccall((:mgl_error_exy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, exDat.data, eyDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Mark(ops::plotOpStack,x::Array,y::Array,z::Array,r::Array,pen::String="o",opt::String="")
	push!(ops, gr->Mark(gr,x,y,z,r,pen,opt))
end

function Mark(gr::mglGraph,x::Array,y::Array,z::Array,r::Array,pen::String="o",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  rDat = mglData(r)

    	ccall((:mgl_mark_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, rDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Mark(ops::plotOpStack,x::Array,y::Array,r::Array,pen::String="o",opt::String="")
	push!(ops, gr->Mark(gr,x,y,r,pen,opt))
end

function Mark(gr::mglGraph,x::Array,y::Array,r::Array,pen::String="o",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  rDat = mglData(r)

    	ccall((:mgl_mark_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, rDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Mark(ops::plotOpStack,y::Array,r::Array,pen::String="o",opt::String="")
	push!(ops, gr->Mark(gr,y,r,pen,opt))
end

function Mark(gr::mglGraph,y::Array,r::Array,pen::String="o",opt::String="")
  yDat = mglData(y)
  rDat = mglData(r)

    	ccall((:mgl_mark_y,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, rDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function TextMark(ops::plotOpStack,x::Array,y::Array,z::Array,r::Array, text::String,fnt::String="",opt::String="")
	push!(ops, gr->TextMark(gr,x,y,z,r, text,fnt,opt))
end

function TextMark(gr::mglGraph,x::Array,y::Array,z::Array,r::Array, text::String,fnt::String="",opt::String="")
	test_ascii(fnt,opt)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  rDat = mglData(r)

    	ccall((:mgl_textmark_xyzr,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, rDat.data, pointer("$text\0"),pointer("$fnt\0"),pointer("$opt\0"))
end

function TextMark(ops::plotOpStack,x::Array,y::Array,r::Array, text::String,fnt::String="",opt::String="")
	push!(ops, gr->TextMark(gr,x,y,r, text,fnt,opt))
end

function TextMark(gr::mglGraph,x::Array,y::Array,r::Array, text::String,fnt::String="",opt::String="")
	test_ascii(fnt,opt)
  xDat = mglData(x)
  yDat = mglData(y)
  rDat = mglData(r)

    	ccall((:mgl_textmark_xyr,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, rDat.data, pointer("$text\0"),pointer("$fnt\0"),pointer("$opt\0"))
end

function TextMark(ops::plotOpStack,y::Array,r::Array, text::String,fnt::String="",opt::String="")
	push!(ops, gr->TextMark(gr,y,r, text,fnt,opt))
end

function TextMark(gr::mglGraph,y::Array,r::Array, text::String,fnt::String="",opt::String="")
	test_ascii(fnt,opt)
  yDat = mglData(y)
  rDat = mglData(r)

    	ccall((:mgl_textmark_yr,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, rDat.data, pointer("$text\0"),pointer("$fnt\0"),pointer("$opt\0"))
end

function TextMark(ops::plotOpStack,y::Array, text::String,fnt::String="",opt::String="")
	push!(ops, gr->TextMark(gr,y, text,fnt,opt))
end

function TextMark(gr::mglGraph,y::Array, text::String,fnt::String="",opt::String="")
	test_ascii(fnt,opt)
  yDat = mglData(y)

    	ccall((:mgl_textmark,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, pointer("$text\0"),pointer("$fnt\0"),pointer("$opt\0"))
end

function Label(ops::plotOpStack,x::Array,y::Array,z::Array, text::String,fnt::String="",opt::String="")
	push!(ops, gr->Label(gr,x,y,z, text,fnt,opt))
end

function Label(gr::mglGraph,x::Array,y::Array,z::Array, text::String,fnt::String="",opt::String="")
	test_ascii(fnt,opt)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_label_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, pointer("$text\0"),pointer("$fnt\0"),pointer("$opt\0"))
end

function Label(ops::plotOpStack,x::Array,y::Array, text::String,fnt::String="",opt::String="")
	push!(ops, gr->Label(gr,x,y, text,fnt,opt))
end

function Label(gr::mglGraph,x::Array,y::Array, text::String,fnt::String="",opt::String="")
	test_ascii(fnt)
	test_ascii(opt)
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_label_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, pointer("$text\0"),pointer("$fnt\0"),pointer("$opt\0"))
end

function Label(ops::plotOpStack, y::Array, text::String, fnt::String="",opt::String="")
	push!(ops, gr->Label(gr, y, text, fnt,opt))
end

function Label(gr::mglGraph, y::Array, text::String, fnt::String="",opt::String="")
	test_ascii(fnt,opt)

    	ccall((:mgl_label,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, dir, pointer("$text\0"), pos,pointer("$opt\0"))
end

function Table(ops::plotOpStack,val::Array,text::String,fnt::String="#|",opt::String="")
	push!(ops, gr->Table(gr,val,text,fnt,opt))
end

function Table(gr::mglGraph,val::Array, text::String,fnt::String="#|",opt::String="")
	test_ascii(fnt)
	test_ascii(opt)
  valDat = mglData(val)

    	ccall((:mgl_table,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, valDat.data, pointer("$text\0"),pointer("$fnt\0"),pointer("$opt\0"))
end

function Table(ops::plotOpStack, x::Number, y::Number,val::Array,text::String,fnt::String="#|",opt::String="")
	push!(ops, gr->Table(gr, x, y,val,text,fnt,opt))
end

function Table(gr::mglGraph, x::Number, y::Number,val::Array, text::String,fnt::String="#|",opt::String="")
	test_ascii(fnt)
	test_ascii(opt)
  valDat = mglData(val)

    	ccall((:mgl_table,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Cdouble,Ptr{Nothing},Ptr{Cwchar_t},Ptr{Cchar},Ptr{Cchar}), gr, x, y, valDat.data, pointer("$text\0"),pointer("$fnt\0"),pointer("$opt\0"))
end

function Tube(ops::plotOpStack,x::Array,y::Array,z::Array,r::Array,pen::String="",opt::String="")
	push!(ops, gr->Tube(gr,x,y,z,r,pen,opt))
end

function Tube(gr::mglGraph,x::Array,y::Array,z::Array,r::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  rDat = mglData(r)

    	ccall((:mgl_tube_xyzr,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, rDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Tube(ops::plotOpStack,x::Array,y::Array,z::Array, r::Number,pen::String="",opt::String="")
	push!(ops, gr->Tube(gr,x,y,z, r,pen,opt))
end

function Tube(gr::mglGraph,x::Array,y::Array,z::Array, r::Number,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_tube_xyzr,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Cdouble,Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, r,pointer("$pen\0"),pointer("$opt\0"))
end

function Tube(ops::plotOpStack,x::Array,y::Array,r::Array,pen::String="",opt::String="")
	push!(ops, gr->Tube(gr,x,y,r,pen,opt))
end

function Tube(gr::mglGraph,x::Array,y::Array,r::Array,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  rDat = mglData(r)

    	ccall((:mgl_tube_xyr,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, rDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Tube(ops::plotOpStack,x::Array,y::Array, r::Number,pen::String="",opt::String="")
	push!(ops, gr->Tube(gr,x,y, r,pen,opt))
end

function Tube(gr::mglGraph,x::Array,y::Array, r::Number,pen::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_tube_xyr,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Cdouble,Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, r,pointer("$pen\0"),pointer("$opt\0"))
end

function Tube(ops::plotOpStack,y::Array,r::Array,pen::String="",opt::String="")
	push!(ops, gr->Tube(gr,y,r,pen,opt))
end

function Tube(gr::mglGraph,y::Array,r::Array,pen::String="",opt::String="")
  yDat = mglData(y)
  rDat = mglData(r)

    	ccall((:mgl_tube_r,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, rDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Tube(ops::plotOpStack,y::Array, r::Number,pen::String="",opt::String="")
	push!(ops, gr->Tube(gr,y, r,pen,opt))
end

function Tube(gr::mglGraph,y::Array, r::Number,pen::String="",opt::String="")
  yDat = mglData(y)

    	ccall((:mgl_tube_r,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Cdouble,Ptr{Cchar},Ptr{Cchar}), gr, yDat.data, r,pointer("$pen\0"),pointer("$opt\0"))
end

function Torus(ops::plotOpStack,r::Array,z::Array,pen::String="",opt::String="")
	push!(ops, gr->Torus(gr,r,z,pen,opt))
end

function Torus(gr::mglGraph,r::Array,z::Array,pen::String="",opt::String="")
  rDat = mglData(r)
  zDat = mglData(z)

    	ccall((:mgl_torus,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, rDat.data, zDat.data,pointer("$pen\0"),pointer("$opt\0"))
end

function Mesh(ops::plotOpStack,x::Array,y::Array,z::Array,stl::String="",opt::String="")
	push!(ops, gr->Mesh(gr,x,y,z,stl,opt))
end

function Mesh(gr::mglGraph,x::Array,y::Array,z::Array,stl::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_mesh_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Mesh(ops::plotOpStack,z::Array,stl::String="",opt::String="")
	push!(ops, gr->Mesh(gr,z,stl,opt))
end

function Mesh(gr::mglGraph,z::Array,stl::String="",opt::String="")
  zDat = mglData(z)

    	ccall((:mgl_mesh,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Fall(ops::plotOpStack,x::Array,y::Array,z::Array,stl::String="",opt::String="")
	push!(ops, gr->Fall(gr,x,y,z,stl,opt))
end

function Fall(gr::mglGraph,x::Array,y::Array,z::Array,stl::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_fall_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Fall(ops::plotOpStack,z::Array,stl::String="",opt::String="")
	push!(ops, gr->Fall(gr,z,stl,opt))
end

function Fall(gr::mglGraph,z::Array,stl::String="",opt::String="")
  zDat = mglData(z)

    	ccall((:mgl_fall,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Belt(ops::plotOpStack,x::Array,y::Array,z::Array,stl::String="",opt::String="")
	push!(ops, gr->Belt(gr,x,y,z,stl,opt))
end

function Belt(gr::mglGraph,x::Array,y::Array,z::Array,stl::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_belt_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Belt(ops::plotOpStack,z::Array,stl::String="",opt::String="")
	push!(ops, gr->Belt(gr,z,stl,opt))
end

function Belt(gr::mglGraph,z::Array,stl::String="",opt::String="")
  zDat = mglData(z)

    	ccall((:mgl_belt,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Surf(ops::plotOpStack,x::Array,y::Array,z::Array,stl::String="",opt::String="")
	push!(ops, gr->Surf(gr,x,y,z,stl,opt))
end

function Surf(gr::mglGraph,x::Array,y::Array,z::Array,stl::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_surf_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Surf(z::Array{mreal, 2}, stl::String="", opt::String="")
        opStack=plotOpStack()
	push!(opStack, gr->SetRange(gr, 'c', minimum(z), maximum(z)), "Set color range")
        push!(opStack, gr->SetRange(gr, 'z', minimum(z), maximum(z)), "Set z range")
	push!(opStack, gr->SetRange(gr, 'x', 0, size(z, 2)), "Set x range")
	push!(opStack, gr->SetRange(gr, 'y', 0, size(z, 1)), "Set y range")
#        push!(opStack, gr->Rotate(gr, 50, 60))
        push!(opStack, gr->Box(gr), "Draw box")
        push!(opStack, gr->Axis(gr), "Axis")
        push!(opStack, gr->Surf(gr, z, stl, opt), "Surface")
        return opStack
end

function Surf(ops::plotOpStack,z::Array,stl::String="",opt::String="")
	push!(ops, gr->Surf(gr,z,stl,opt))
end

function Surf(gr::mglGraph,z::Array,stl::String="",opt::String="")
        data=mglData(z)
    	ccall((:mgl_surf,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, data.data, pointer("$stl\0"),pointer("$opt\0"))

end

function Grid(ops::plotOpStack,x::Array,y::Array,z::Array,stl::String="",opt::String="")
	push!(ops, gr->Grid(gr,x,y,z,stl,opt))
end

function Grid(gr::mglGraph,x::Array,y::Array,z::Array,stl::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_grid_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Grid(ops::plotOpStack, z::Array, stl::String="",opt::String="")
	push!(ops, gr->Grid(gr, z, stl,opt))
end

function Grid(gr::mglGraph, z::Array, stl::String="",opt::String="")

    	ccall((:mgl_axis_grid,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, dir, pen,pointer("$opt\0"))
end

function Tile(ops::plotOpStack,x::Array,y::Array,z::Array,stl::String="",opt::String="")
	push!(ops, gr->Tile(gr,x,y,z,stl,opt))
end

function Tile(gr::mglGraph,x::Array,y::Array,z::Array,stl::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_tile_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Tile(ops::plotOpStack,z::Array,stl::String="",opt::String="")
	push!(ops, gr->Tile(gr,z,stl,opt))
end

function Tile(gr::mglGraph,z::Array,stl::String="",opt::String="")
  zDat = mglData(z)

    	ccall((:mgl_tile,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Dens(ops::plotOpStack,x::Array,y::Array,c::Array,stl::String="",opt::String="")
	push!(ops, gr->Dens(gr,x,y,c,stl,opt))
end

function Dens(gr::mglGraph,x::Array,y::Array,c::Array,stl::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  cDat = mglData(c)

    	ccall((:mgl_dens_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, cDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Dens(ops::plotOpStack,c::Array,stl::String="",opt::String="")
	push!(ops, gr->Dens(gr,c,stl,opt))
end

function Dens(gr::mglGraph,c::Array,stl::String="",opt::String="")
  cDat = mglData(c)

    	ccall((:mgl_dens,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, cDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Dens(c::Array, stl::String="", opt::String="")
	opStack = plotOpStack()
	push!(opStack, gr->SetRange(gr, 'c', minimum(c), maximum(c)), "Set colorbar range")
	push!(opStack, gr->SetRange(gr, 'x', 0, size(c)[2]), "Set X range")
	push!(opStack, gr->SetRange(gr, 'y', 0, size(c)[1]), "Set Y range")
	push!(opStack, gr->Box(gr), "Box")
	push!(opStack, gr->Axis(gr), "Axis")
	push!(opStack, gr->Dens(gr, c, stl, opt), "Dens")

	return opStack
end

function Boxs(ops::plotOpStack,x::Array,y::Array,z::Array,stl::String="",opt::String="")
	push!(ops, gr->Boxs(gr,x,y,z,stl,opt))
end

function Boxs(gr::mglGraph,x::Array,y::Array,z::Array,stl::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_boxs_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Boxs(ops::plotOpStack,z::Array,stl::String="",opt::String="")
	push!(ops, gr->Boxs(gr,z,stl,opt))
end

function Boxs(gr::mglGraph,z::Array,stl::String="",opt::String="")
  zDat = mglData(z)

    	ccall((:mgl_boxs,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Cont(ops::plotOpStack,v::Array,x::Array,y::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->Cont(gr,v,x,y,z,sch,opt))
end

function Cont(gr::mglGraph,v::Array,x::Array,y::Array,z::Array,sch::String="",opt::String="")
  vDat = mglData(v)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_cont_xy_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, xDat.data, yDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Cont(ops::plotOpStack,v::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->Cont(gr,v,z,sch,opt))
end

function Cont(gr::mglGraph,v::Array,z::Array,sch::String="",opt::String="")
  vDat = mglData(v)
  zDat = mglData(z)

    	ccall((:mgl_cont_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Cont(ops::plotOpStack,x::Array,y::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->Cont(gr,x,y,z,sch,opt))
end

function Cont(gr::mglGraph,x::Array,y::Array,z::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_cont_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Cont(ops::plotOpStack,z::Array,sch::String="",opt::String="")
	push!(ops, gr->Cont(gr,z,sch,opt))
end

function Cont(gr::mglGraph,z::Array,sch::String="",opt::String="")
  zDat = mglData(z)

    	ccall((:mgl_cont,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function ContF(ops::plotOpStack,v::Array,x::Array,y::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->ContF(gr,v,x,y,z,sch,opt))
end

function ContF(gr::mglGraph,v::Array,x::Array,y::Array,z::Array,sch::String="",opt::String="")
  vDat = mglData(v)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_contf_xy_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, xDat.data, yDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function ContF(ops::plotOpStack,v::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->ContF(gr,v,z,sch,opt))
end

function ContF(gr::mglGraph,v::Array,z::Array,sch::String="",opt::String="")
  vDat = mglData(v)
  zDat = mglData(z)

    	ccall((:mgl_contf_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function ContF(ops::plotOpStack,x::Array,y::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->ContF(gr,x,y,z,sch,opt))
end

function ContF(gr::mglGraph,x::Array,y::Array,z::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_contf_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function ContF(ops::plotOpStack,z::Array,sch::String="",opt::String="")
	push!(ops, gr->ContF(gr,z,sch,opt))
end

function ContF(gr::mglGraph,z::Array,sch::String="",opt::String="")
  zDat = mglData(z)

    	ccall((:mgl_contf,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function ContD(ops::plotOpStack,v::Array,x::Array,y::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->ContD(gr,v,x,y,z,sch,opt))
end

function ContD(gr::mglGraph,v::Array,x::Array,y::Array,z::Array,sch::String="",opt::String="")
  vDat = mglData(v)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_contd_xy_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, xDat.data, yDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function ContD(ops::plotOpStack,v::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->ContD(gr,v,z,sch,opt))
end

function ContD(gr::mglGraph,v::Array,z::Array,sch::String="",opt::String="")
  vDat = mglData(v)
  zDat = mglData(z)

    	ccall((:mgl_contd_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function ContD(ops::plotOpStack,x::Array,y::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->ContD(gr,x,y,z,sch,opt))
end

function ContD(gr::mglGraph,x::Array,y::Array,z::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_contd_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function ContD(ops::plotOpStack,z::Array,sch::String="",opt::String="")
	push!(ops, gr->ContD(gr,z,sch,opt))
end

function ContD(gr::mglGraph,z::Array,sch::String="",opt::String="")
  zDat = mglData(z)

    	ccall((:mgl_contd,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function ContV(ops::plotOpStack,v::Array,x::Array,y::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->ContV(gr,v,x,y,z,sch,opt))
end

function ContV(gr::mglGraph,v::Array,x::Array,y::Array,z::Array,sch::String="",opt::String="")
  vDat = mglData(v)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_contv_xy_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, xDat.data, yDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function ContV(ops::plotOpStack,v::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->ContV(gr,v,z,sch,opt))
end

function ContV(gr::mglGraph,v::Array,z::Array,sch::String="",opt::String="")
  vDat = mglData(v)
  zDat = mglData(z)

    	ccall((:mgl_contv_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function ContV(ops::plotOpStack,x::Array,y::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->ContV(gr,x,y,z,sch,opt))
end

function ContV(gr::mglGraph,x::Array,y::Array,z::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_contv_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function ContV(ops::plotOpStack,z::Array,sch::String="",opt::String="")
	push!(ops, gr->ContV(gr,z,sch,opt))
end

function ContV(gr::mglGraph,z::Array,sch::String="",opt::String="")
  zDat = mglData(z)

    	ccall((:mgl_contv,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Axial(ops::plotOpStack,v::Array,x::Array,y::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->Axial(gr,v,x,y,z,sch,opt))
end

function Axial(gr::mglGraph,v::Array,x::Array,y::Array,z::Array,sch::String="",opt::String="")
  vDat = mglData(v)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_axial_xy_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, xDat.data, yDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Axial(ops::plotOpStack,v::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->Axial(gr,v,z,sch,opt))
end

function Axial(gr::mglGraph,v::Array,z::Array,sch::String="",opt::String="")
  vDat = mglData(v)
  zDat = mglData(z)

    	ccall((:mgl_axial_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Axial(ops::plotOpStack,x::Array,y::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->Axial(gr,x,y,z,sch,opt))
end

function Axial(gr::mglGraph,x::Array,y::Array,z::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_axial_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Axial(ops::plotOpStack,z::Array,sch::String="",opt::String="")
	push!(ops, gr->Axial(gr,z,sch,opt))
end

function Axial(gr::mglGraph,z::Array,sch::String="",opt::String="")
  zDat = mglData(z)

    	ccall((:mgl_axial,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Grid3(ops::plotOpStack,x::Array,y::Array,z::Array,a::Array,stl::String="", sVal::Number=-1.0,opt::String="")
	push!(ops, gr->Grid3(gr,x,y,z,a,stl, sVal,opt))
end

function Grid3(gr::mglGraph,x::Array,y::Array,z::Array,a::Array,stl::String="", sVal::Number=-1.0,opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	mgl_grid3ccall((:_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function Grid3(ops::plotOpStack,a::Array,stl::String="", sVal::Number=-1.0,opt::String="")
	push!(ops, gr->Grid3(gr,a,stl, sVal,opt))
end

function Grid3(gr::mglGraph,a::Array,stl::String="", sVal::Number=-1.0,opt::String="")
  aDat = mglData(a)

    	mgl_grid3ccall((:,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function Dens3(ops::plotOpStack,x::Array,y::Array,z::Array,a::Array,stl::String="", sVal::Number=-1.0,opt::String="")
	push!(ops, gr->Dens3(gr,x,y,z,a,stl, sVal,opt))
end

function Dens3(gr::mglGraph,x::Array,y::Array,z::Array,a::Array,stl::String="", sVal::Number=-1.0,opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	mgl_dens3ccall((:_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function Dens3(ops::plotOpStack,a::Array,stl::String="", sVal::Number=-1.0,opt::String="")
	push!(ops, gr->Dens3(gr,a,stl, sVal,opt))
end

function Dens3(gr::mglGraph,a::Array,stl::String="", sVal::Number=-1.0,opt::String="")
  aDat = mglData(a)

    	mgl_dens3ccall((:,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function Surf3(ops::plotOpStack, Val::Number,x::Array,y::Array,z::Array,a::Array,stl::String="",opt::String="")
	push!(ops, gr->Surf3(gr, Val,x,y,z,a,stl,opt))
end

function Surf3(gr::mglGraph, Val::Number,x::Array,y::Array,z::Array,a::Array,stl::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	mgl_surf3ccall((:_xyz_val,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, Val, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Surf3(ops::plotOpStack, Val::Number,a::Array,stl::String="",opt::String="")
	push!(ops, gr->Surf3(gr, Val,a,stl,opt))
end

function Surf3(gr::mglGraph, Val::Number,a::Array,stl::String="",opt::String="")
  aDat = mglData(a)

    	mgl_surf3ccall((:_val,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, Val, aDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Surf3(ops::plotOpStack,x::Array,y::Array,z::Array,a::Array,stl::String="",opt::String="")
	push!(ops, gr->Surf3(gr,x,y,z,a,stl,opt))
end

function Surf3(gr::mglGraph,x::Array,y::Array,z::Array,a::Array,stl::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	mgl_surf3ccall((:_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Surf3(ops::plotOpStack,a::Array,stl::String="",opt::String="")
	push!(ops, gr->Surf3(gr,a,stl,opt))
end

function Surf3(gr::mglGraph,a::Array,stl::String="",opt::String="")
  aDat = mglData(a)

    	mgl_surf3ccall((:,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, aDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Cloud(ops::plotOpStack,x::Array,y::Array,z::Array,a::Array,stl::String="",opt::String="")
	push!(ops, gr->Cloud(gr,x,y,z,a,stl,opt))
end

function Cloud(gr::mglGraph,x::Array,y::Array,z::Array,a::Array,stl::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	ccall((:mgl_cloud_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Cloud(ops::plotOpStack,a::Array,stl::String="",opt::String="")
	push!(ops, gr->Cloud(gr,a,stl,opt))
end

function Cloud(gr::mglGraph,a::Array,stl::String="",opt::String="")
  aDat = mglData(a)

    	ccall((:mgl_cloud,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, aDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Cont3(ops::plotOpStack,v::Array,x::Array,y::Array,z::Array,a::Array,sch::String="", sVal::Number=-1.0,opt::String="")
	push!(ops, gr->Cont3(gr,v,x,y,z,a,sch, sVal,opt))
end

function Cont3(gr::mglGraph,v::Array,x::Array,y::Array,z::Array,a::Array,sch::String="", sVal::Number=-1.0,opt::String="")
  vDat = mglData(v)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	mgl_cont3ccall((:_xyz_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, vDat.data, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$sch\0"), sVal,pointer("$opt\0"))
end

function Cont3(ops::plotOpStack,v::Array,a::Array,sch::String="", sVal::Number=-1.0,opt::String="")
	push!(ops, gr->Cont3(gr,v,a,sch, sVal,opt))
end

function Cont3(gr::mglGraph,v::Array,a::Array,sch::String="", sVal::Number=-1.0,opt::String="")
  vDat = mglData(v)
  aDat = mglData(a)

    	mgl_cont3ccall((:_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, vDat.data, aDat.data,pointer("$sch\0"), sVal,pointer("$opt\0"))
end

function Cont3(ops::plotOpStack,x::Array,y::Array,z::Array,a::Array,sch::String="", sVal::Number=-1.0,opt::String="")
	push!(ops, gr->Cont3(gr,x,y,z,a,sch, sVal,opt))
end

function Cont3(gr::mglGraph,x::Array,y::Array,z::Array,a::Array,sch::String="", sVal::Number=-1.0,opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	mgl_cont3ccall((:_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$sch\0"), sVal,pointer("$opt\0"))
end

function Cont3(ops::plotOpStack,a::Array,sch::String="", sVal::Number=-1.0,opt::String="")
	push!(ops, gr->Cont3(gr,a,sch, sVal,opt))
end

function Cont3(gr::mglGraph,a::Array,sch::String="", sVal::Number=-1.0,opt::String="")
  aDat = mglData(a)

    	mgl_cont3ccall((:,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$sch\0"), sVal,pointer("$opt\0"))
end

function ContF3(ops::plotOpStack,v::Array,x::Array,y::Array,z::Array,a::Array,sch::String="", sVal::Number=-1.0,opt::String="")
	push!(ops, gr->ContF3(gr,v,x,y,z,a,sch, sVal,opt))
end

function ContF3(gr::mglGraph,v::Array,x::Array,y::Array,z::Array,a::Array,sch::String="", sVal::Number=-1.0,opt::String="")
  vDat = mglData(v)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	mgl_contf3ccall((:_xyz_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, vDat.data, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$sch\0"), sVal,pointer("$opt\0"))
end

function ContF3(ops::plotOpStack,v::Array,a::Array,sch::String="", sVal::Number=-1.0,opt::String="")
	push!(ops, gr->ContF3(gr,v,a,sch, sVal,opt))
end

function ContF3(gr::mglGraph,v::Array,a::Array,sch::String="", sVal::Number=-1.0,opt::String="")
  vDat = mglData(v)
  aDat = mglData(a)

    	mgl_contf3ccall((:_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, vDat.data, aDat.data,pointer("$sch\0"), sVal,pointer("$opt\0"))
end

function ContF3(ops::plotOpStack,x::Array,y::Array,z::Array,a::Array,sch::String="", sVal::Number=-1.0,opt::String="")
	push!(ops, gr->ContF3(gr,x,y,z,a,sch, sVal,opt))
end

function ContF3(gr::mglGraph,x::Array,y::Array,z::Array,a::Array,sch::String="", sVal::Number=-1.0,opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	mgl_contf3ccall((:_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$sch\0"), sVal,pointer("$opt\0"))
end

function ContF3(ops::plotOpStack,a::Array,sch::String="", sVal::Number=-1.0,opt::String="")
	push!(ops, gr->ContF3(gr,a,sch, sVal,opt))
end

function ContF3(gr::mglGraph,a::Array,sch::String="", sVal::Number=-1.0,opt::String="")
  aDat = mglData(a)

    	mgl_contf3ccall((:,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$sch\0"), sVal,pointer("$opt\0"))
end

function Beam(ops::plotOpStack,tr::Array,g1::Array,g2::Array,a::Array, r::Number,stl::String=0, flag::Int=0, num::Int=3)
	push!(ops, gr->Beam(gr,tr,g1,g2,a, r,stl, flag, num))
end

function Beam(gr::mglGraph,tr::Array,g1::Array,g2::Array,a::Array, r::Number,stl::String=0, flag::Int=0, num::Int=3)
  trDat = mglData(tr)
  g1Dat = mglData(g1)
  g2Dat = mglData(g2)
  aDat = mglData(a)

    	ccall((:mgl_beam,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Cdouble,Ptr{Cchar},Cint,Cint), gr, trDat.data, g1Dat.data, g2Dat.data, aDat.data,r,pointer("$stl\0"),flag,num)
end

function TileS(ops::plotOpStack,x::Array,y::Array,z::Array,r::Array,stl::String="",opt::String="")
	push!(ops, gr->TileS(gr,x,y,z,r,stl,opt))
end

function TileS(gr::mglGraph,x::Array,y::Array,z::Array,r::Array,stl::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  rDat = mglData(r)

    	ccall((:mgl_tiles_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, rDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function TileS(ops::plotOpStack,z::Array,r::Array,stl::String="",opt::String="")
	push!(ops, gr->TileS(gr,z,r,stl,opt))
end

function TileS(gr::mglGraph,z::Array,r::Array,stl::String="",opt::String="")
  zDat = mglData(z)
  rDat = mglData(r)

    	ccall((:mgl_tiles,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data, rDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function SurfC(ops::plotOpStack,x::Array,y::Array,z::Array,c::Array,sch::String="",opt::String="")
	push!(ops, gr->SurfC(gr,x,y,z,c,sch,opt))
end

function SurfC(gr::mglGraph,x::Array,y::Array,z::Array,c::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  cDat = mglData(c)

    	ccall((:mgl_surfc_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, cDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function SurfC(ops::plotOpStack,z::Array,c::Array,sch::String="",opt::String="")
	push!(ops, gr->SurfC(gr,z,c,sch,opt))
end

function SurfC(gr::mglGraph,z::Array,c::Array,sch::String="",opt::String="")
  zDat = mglData(z)
  cDat = mglData(c)

    	ccall((:mgl_surfc,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data, cDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function SurfA(ops::plotOpStack,x::Array,y::Array,z::Array,c::Array,sch::String="",opt::String="")
	push!(ops, gr->SurfA(gr,x,y,z,c,sch,opt))
end

function SurfA(gr::mglGraph,x::Array,y::Array,z::Array,c::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  cDat = mglData(c)

    	ccall((:mgl_surfa_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, cDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function SurfA(ops::plotOpStack,z::Array,c::Array,sch::String="",opt::String="")
	push!(ops, gr->SurfA(gr,z,c,sch,opt))
end

function SurfA(gr::mglGraph,z::Array,c::Array,sch::String="",opt::String="")
  zDat = mglData(z)
  cDat = mglData(c)

    	ccall((:mgl_surfa,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data, cDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function SurfCA(ops::plotOpStack,x::Array,y::Array,z::Array,c::Array,a::Array,sch::String="",opt::String="")
	push!(ops, gr->SurfCA(gr,x,y,z,c,a,sch,opt))
end

function SurfCA(gr::mglGraph,x::Array,y::Array,z::Array,c::Array,a::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  cDat = mglData(c)
  aDat = mglData(a)

    	ccall((:mgl_surfca_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, cDat.data, aDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function SurfCA(ops::plotOpStack,z::Array,c::Array,a::Array,sch::String="",opt::String="")
	push!(ops, gr->SurfCA(gr,z,c,a,sch,opt))
end

function SurfCA(gr::mglGraph,z::Array,c::Array,a::Array,sch::String="",opt::String="")
  zDat = mglData(z)
  cDat = mglData(c)
  aDat = mglData(a)

    	ccall((:mgl_surfca,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, zDat.data, cDat.data, aDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Map(ops::plotOpStack,x::Array,y::Array,a::Array,b::Array,sch::String="",opt::String="")
	push!(ops, gr->Map(gr,x,y,a,b,sch,opt))
end

function Map(gr::mglGraph,x::Array,y::Array,a::Array,b::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  aDat = mglData(a)
  bDat = mglData(b)

    	ccall((:mgl_map_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, aDat.data, bDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Map(ops::plotOpStack,a::Array,b::Array,sch::String="",opt::String="")
	push!(ops, gr->Map(gr,a,b,sch,opt))
end

function Map(gr::mglGraph,a::Array,b::Array,sch::String="",opt::String="")
  aDat = mglData(a)
  bDat = mglData(b)

    	ccall((:mgl_map,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, aDat.data, bDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function STFA(ops::plotOpStack,x::Array,y::Array,re::Array,im::Array, dn::Int,sch::String="",opt::String="")
	push!(ops, gr->STFA(gr,x,y,re,im, dn,sch,opt))
end

function STFA(gr::mglGraph,x::Array,y::Array,re::Array,im::Array, dn::Int,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  reDat = mglData(re)
  imDat = mglData(im)

    	ccall((:mgl_stfa_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Cint,Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, reDat.data, imDat.data, dn,pointer("$sch\0"),pointer("$opt\0"))
end

function STFA(ops::plotOpStack,re::Array,im::Array, dn::Int,sch::String="",opt::String="")
	push!(ops, gr->STFA(gr,re,im, dn,sch,opt))
end

function STFA(gr::mglGraph,re::Array,im::Array, dn::Int,sch::String="",opt::String="")
  reDat = mglData(re)
  imDat = mglData(im)

    	ccall((:mgl_stfa,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Cint,Ptr{Cchar},Ptr{Cchar}), gr, reDat.data, imDat.data, dn,pointer("$sch\0"),pointer("$opt\0"))
end

function Surf3A(ops::plotOpStack, Val::Number,x::Array,y::Array,z::Array,a::Array,b::Array,stl::String="",opt::String="")
	push!(ops, gr->Surf3A(gr, Val,x,y,z,a,b,stl,opt))
end

function Surf3A(gr::mglGraph, Val::Number,x::Array,y::Array,z::Array,a::Array,b::Array,stl::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)
  bDat = mglData(b)

    	mgl_surf3ccall((:a_xyz_val,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, Val, xDat.data, yDat.data, zDat.data, aDat.data, bDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Surf3A(ops::plotOpStack, Val::Number,a::Array,b::Array,stl::String="",opt::String="")
	push!(ops, gr->Surf3A(gr, Val,a,b,stl,opt))
end

function Surf3A(gr::mglGraph, Val::Number,a::Array,b::Array,stl::String="",opt::String="")
  aDat = mglData(a)
  bDat = mglData(b)

    	mgl_surf3ccall((:a_val,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, Val, aDat.data, bDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Surf3A(ops::plotOpStack,x::Array,y::Array,z::Array,a::Array,b::Array,stl::String="",opt::String="")
	push!(ops, gr->Surf3A(gr,x,y,z,a,b,stl,opt))
end

function Surf3A(gr::mglGraph,x::Array,y::Array,z::Array,a::Array,b::Array,stl::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)
  bDat = mglData(b)

    	mgl_surf3ccall((:a_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, aDat.data, bDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Surf3A(ops::plotOpStack,a::Array,b::Array,stl::String="",opt::String="")
	push!(ops, gr->Surf3A(gr,a,b,stl,opt))
end

function Surf3A(gr::mglGraph,a::Array,b::Array,stl::String="",opt::String="")
  aDat = mglData(a)
  bDat = mglData(b)

    	mgl_surf3ccall((:a,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, aDat.data, bDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Surf3C(ops::plotOpStack, Val::Number,x::Array,y::Array,z::Array,a::Array,c::Array,stl::String="",opt::String="")
	push!(ops, gr->Surf3C(gr, Val,x,y,z,a,c,stl,opt))
end

function Surf3C(gr::mglGraph, Val::Number,x::Array,y::Array,z::Array,a::Array,c::Array,stl::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)
  cDat = mglData(c)

    	mgl_surf3ccall((:c_xyz_val,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, Val, xDat.data, yDat.data, zDat.data, aDat.data, cDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Surf3C(ops::plotOpStack, Val::Number,a::Array,c::Array,stl::String="",opt::String="")
	push!(ops, gr->Surf3C(gr, Val,a,c,stl,opt))
end

function Surf3C(gr::mglGraph, Val::Number,a::Array,c::Array,stl::String="",opt::String="")
  aDat = mglData(a)
  cDat = mglData(c)

    	mgl_surf3ccall((:c_val,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, Val, aDat.data, cDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Surf3C(ops::plotOpStack,x::Array,y::Array,z::Array,a::Array,c::Array,stl::String="",opt::String="")
	push!(ops, gr->Surf3C(gr,x,y,z,a,c,stl,opt))
end

function Surf3C(gr::mglGraph,x::Array,y::Array,z::Array,a::Array,c::Array,stl::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)
  cDat = mglData(c)

    	mgl_surf3ccall((:c_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, aDat.data, cDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Surf3C(ops::plotOpStack,a::Array,c::Array,stl::String="",opt::String="")
	push!(ops, gr->Surf3C(gr,a,c,stl,opt))
end

function Surf3C(gr::mglGraph,a::Array,c::Array,stl::String="",opt::String="")
  aDat = mglData(a)
  cDat = mglData(c)

    	mgl_surf3ccall((:c,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, aDat.data, cDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Surf3CA(ops::plotOpStack, Val::Number,x::Array,y::Array,z::Array,a::Array,c::Array,b::Array,stl::String="",opt::String="")
	push!(ops, gr->Surf3CA(gr, Val,x,y,z,a,c,b,stl,opt))
end

function Surf3CA(gr::mglGraph, Val::Number,x::Array,y::Array,z::Array,a::Array,c::Array,b::Array,stl::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)
  cDat = mglData(c)
  bDat = mglData(b)

    	mgl_surf3ccall((:ca_xyz_val,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, Val, xDat.data, yDat.data, zDat.data, aDat.data, cDat.data, bDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Surf3CA(ops::plotOpStack, Val::Number,a::Array,c::Array,b::Array,stl::String="",opt::String="")
	push!(ops, gr->Surf3CA(gr, Val,a,c,b,stl,opt))
end

function Surf3CA(gr::mglGraph, Val::Number,a::Array,c::Array,b::Array,stl::String="",opt::String="")
  aDat = mglData(a)
  cDat = mglData(c)
  bDat = mglData(b)

    	mgl_surf3ccall((:ca_val,mgllib_name), Nothing, (Ptr{Nothing},Cdouble,Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, Val, aDat.data, cDat.data, bDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Surf3CA(ops::plotOpStack,x::Array,y::Array,z::Array,a::Array,c::Array,b::Array,stl::String="",opt::String="")
	push!(ops, gr->Surf3CA(gr,x,y,z,a,c,b,stl,opt))
end

function Surf3CA(gr::mglGraph,x::Array,y::Array,z::Array,a::Array,c::Array,b::Array,stl::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)
  cDat = mglData(c)
  bDat = mglData(b)

    	mgl_surf3ccall((:ca_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, aDat.data, cDat.data, bDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Surf3CA(ops::plotOpStack,a::Array,c::Array,b::Array,stl::String="",opt::String="")
	push!(ops, gr->Surf3CA(gr,a,c,b,stl,opt))
end

function Surf3CA(gr::mglGraph,a::Array,c::Array,b::Array,stl::String="",opt::String="")
  aDat = mglData(a)
  cDat = mglData(c)
  bDat = mglData(b)

    	mgl_surf3ccall((:ca,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, aDat.data, cDat.data, bDat.data,pointer("$stl\0"),pointer("$opt\0"))
end

function Dew(ops::plotOpStack,x::Array,y::Array,ax::Array,ay::Array,sch::String="",opt::String="")
	push!(ops, gr->Dew(gr,x,y,ax,ay,sch,opt))
end

function Dew(gr::mglGraph,x::Array,y::Array,ax::Array,ay::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	ccall((:mgl_dew_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, axDat.data, ayDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Dew(ops::plotOpStack,ax::Array,ay::Array,sch::String="",opt::String="")
	push!(ops, gr->Dew(gr,ax,ay,sch,opt))
end

function Dew(gr::mglGraph,ax::Array,ay::Array,sch::String="",opt::String="")
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	mgl_dew_2ccall((:d,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, axDat.data, ayDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Traj(ops::plotOpStack,x::Array,y::Array,ax::Array,ay::Array,sch::String="",opt::String="")
	push!(ops, gr->Traj(gr,x,y,ax,ay,sch,opt))
end

function Traj(gr::mglGraph,x::Array,y::Array,ax::Array,ay::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	ccall((:mgl_traj_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, axDat.data, ayDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Traj(ops::plotOpStack,x::Array,y::Array,z::Array,ax::Array,ay::Array,az::Array,sch::String="",opt::String="")
	push!(ops, gr->Traj(gr,x,y,z,ax,ay,az,sch,opt))
end

function Traj(gr::mglGraph,x::Array,y::Array,z::Array,ax::Array,ay::Array,az::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	ccall((:mgl_traj_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, axDat.data, ayDat.data, azDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Vect(ops::plotOpStack,x::Array,y::Array,ax::Array,ay::Array,sch::String="",opt::String="")
	push!(ops, gr->Vect(gr,x,y,ax,ay,sch,opt))
end

function Vect(gr::mglGraph,x::Array,y::Array,ax::Array,ay::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	ccall((:mgl_vect_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, axDat.data, ayDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Vect(ops::plotOpStack,ax::Array,ay::Array,sch::String="",opt::String="")
	push!(ops, gr->Vect(gr,ax,ay,sch,opt))
end

function Vect(gr::mglGraph,ax::Array,ay::Array,sch::String="",opt::String="")
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	mgl_vect_2ccall((:d,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, axDat.data, ayDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Vect(ops::plotOpStack,x::Array,y::Array,z::Array,ax::Array,ay::Array,az::Array,sch::String="",opt::String="")
	push!(ops, gr->Vect(gr,x,y,z,ax,ay,az,sch,opt))
end

function Vect(gr::mglGraph,x::Array,y::Array,z::Array,ax::Array,ay::Array,az::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	ccall((:mgl_vect_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, axDat.data, ayDat.data, azDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Vect(ops::plotOpStack,ax::Array,ay::Array,az::Array,sch::String="",opt::String="")
	push!(ops, gr->Vect(gr,ax,ay,az,sch,opt))
end

function Vect(gr::mglGraph,ax::Array,ay::Array,az::Array,sch::String="",opt::String="")
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	mgl_vect_3ccall((:d,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, axDat.data, ayDat.data, azDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Vect3(ops::plotOpStack,x::Array,y::Array,z::Array,ax::Array,ay::Array,az::Array,stl::String="", sVal::Number=-1.0,opt::String="")
	push!(ops, gr->Vect3(gr,x,y,z,ax,ay,az,stl, sVal,opt))
end

function Vect3(gr::mglGraph,x::Array,y::Array,z::Array,ax::Array,ay::Array,az::Array,stl::String="", sVal::Number=-1.0,opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	mgl_vect3ccall((:_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, axDat.data, ayDat.data, azDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function Vect3(ops::plotOpStack,ax::Array,ay::Array,az::Array,stl::String="", sVal::Number=-1.0,opt::String="")
	push!(ops, gr->Vect3(gr,ax,ay,az,stl, sVal,opt))
end

function Vect3(gr::mglGraph,ax::Array,ay::Array,az::Array,stl::String="", sVal::Number=-1.0,opt::String="")
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	mgl_vect3ccall((:,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, axDat.data, ayDat.data, azDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function Flow(ops::plotOpStack,x::Array,y::Array,ax::Array,ay::Array,sch::String="",opt::String="")
	push!(ops, gr->Flow(gr,x,y,ax,ay,sch,opt))
end

function Flow(gr::mglGraph,x::Array,y::Array,ax::Array,ay::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	ccall((:mgl_flow_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, axDat.data, ayDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Flow(ops::plotOpStack,ax::Array,ay::Array,sch::String="",opt::String="")
	push!(ops, gr->Flow(gr,ax,ay,sch,opt))
end

function Flow(gr::mglGraph,ax::Array,ay::Array,sch::String="",opt::String="")
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	ccall((:mgl_flow_2d,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, axDat.data, ayDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Flow(ops::plotOpStack,x::Array,y::Array,z::Array,ax::Array,ay::Array,az::Array,sch::String="",opt::String="")
	push!(ops, gr->Flow(gr,x,y,z,ax,ay,az,sch,opt))
end

function Flow(gr::mglGraph,x::Array,y::Array,z::Array,ax::Array,ay::Array,az::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	ccall((:mgl_flow_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, axDat.data, ayDat.data, azDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Flow(ops::plotOpStack,ax::Array,ay::Array,az::Array,sch::String="",opt::String="")
	push!(ops, gr->Flow(gr,ax,ay,az,sch,opt))
end

function Flow(gr::mglGraph,ax::Array,ay::Array,az::Array,sch::String="",opt::String="")
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	mgl_flow_3ccall((:d,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, axDat.data, ayDat.data, azDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function FlowP(ops::plotOpStack, p::mglPoint,x::Array,y::Array,ax::Array,ay::Array,sch::String="",opt::String="")
	push!(ops, gr->FlowP(gr, p,x,y,ax,ay,sch,opt))
end

function FlowP(gr::mglGraph, p::mglPoint,x::Array,y::Array,ax::Array,ay::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	ccall((:mgl_flowp_xy,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, p.x, p.y, p.z, xDat.data, yDat.data, axDat.data, ayDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function FlowP(ops::plotOpStack, p::mglPoint,ax::Array,ay::Array,sch::String="",opt::String="")
	push!(ops, gr->FlowP(gr, p,ax,ay,sch,opt))
end

function FlowP(gr::mglGraph, p::mglPoint,ax::Array,ay::Array,sch::String="",opt::String="")
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	mgl_flowp_2ccall((:d,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, p.x, p.y, p.z, axDat.data, ayDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function FlowP(ops::plotOpStack, p::mglPoint,x::Array,y::Array,z::Array,ax::Array,ay::Array,az::Array,sch::String="",opt::String="")
	push!(ops, gr->FlowP(gr, p,x,y,z,ax,ay,az,sch,opt))
end

function FlowP(gr::mglGraph, p::mglPoint,x::Array,y::Array,z::Array,ax::Array,ay::Array,az::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	ccall((:mgl_flowp_xyz,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, p.x, p.y, p.z, xDat.data, yDat.data, zDat.data, axDat.data, ayDat.data, azDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function FlowP(ops::plotOpStack, p::mglPoint,ax::Array,ay::Array,az::Array,sch::String="",opt::String="")
	push!(ops, gr->FlowP(gr, p,ax,ay,az,sch,opt))
end

function FlowP(gr::mglGraph, p::mglPoint,ax::Array,ay::Array,az::Array,sch::String="",opt::String="")
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	mgl_flowp_3ccall((:d,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, p.x, p.y, p.z, axDat.data, ayDat.data, azDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Pipe(ops::plotOpStack,x::Array,y::Array,ax::Array,ay::Array,sch::String="", r0::Number=0.05,opt::String="")
	push!(ops, gr->Pipe(gr,x,y,ax,ay,sch, r0,opt))
end

function Pipe(gr::mglGraph,x::Array,y::Array,ax::Array,ay::Array,sch::String="", r0::Number=0.05,opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	ccall((:mgl_pipe_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, xDat.data, yDat.data, axDat.data, ayDat.data,pointer("$sch\0"), r0,pointer("$opt\0"))
end

function Pipe(ops::plotOpStack,ax::Array,ay::Array,sch::String="", r0::Number=0.05,opt::String="")
	push!(ops, gr->Pipe(gr,ax,ay,sch, r0,opt))
end

function Pipe(gr::mglGraph,ax::Array,ay::Array,sch::String="", r0::Number=0.05,opt::String="")
  axDat = mglData(ax)
  ayDat = mglData(ay)

    	ccall((:mgl_pipe_2d,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, axDat.data, ayDat.data,pointer("$sch\0"), r0,pointer("$opt\0"))
end

function Pipe(ops::plotOpStack,x::Array,y::Array,z::Array,ax::Array,ay::Array,az::Array,sch::String="", r0::Number=0.05,opt::String="")
	push!(ops, gr->Pipe(gr,x,y,z,ax,ay,az,sch, r0,opt))
end

function Pipe(gr::mglGraph,x::Array,y::Array,z::Array,ax::Array,ay::Array,az::Array,sch::String="", r0::Number=0.05,opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	ccall((:mgl_pipe_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, axDat.data, ayDat.data, azDat.data,pointer("$sch\0"), r0,pointer("$opt\0"))
end

function Pipe(ops::plotOpStack,ax::Array,ay::Array,az::Array,sch::String="", r0::Number=0.05,opt::String="")
	push!(ops, gr->Pipe(gr,ax,ay,az,sch, r0,opt))
end

function Pipe(gr::mglGraph,ax::Array,ay::Array,az::Array,sch::String="", r0::Number=0.05,opt::String="")
  axDat = mglData(ax)
  ayDat = mglData(ay)
  azDat = mglData(az)

    	mgl_pipe_3ccall((:d,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, axDat.data, ayDat.data, azDat.data,pointer("$sch\0"), r0,pointer("$opt\0"))
end

function DensX(ops::plotOpStack,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
	push!(ops, gr->DensX(gr,a,stl, sVal,opt))
end

function DensX(gr::mglGraph,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
  aDat = mglData(a)

    	ccall((:mgl_dens_x,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function DensY(ops::plotOpStack,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
	push!(ops, gr->DensY(gr,a,stl, sVal,opt))
end

function DensY(gr::mglGraph,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
  aDat = mglData(a)

    	ccall((:mgl_dens_y,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function DensZ(ops::plotOpStack,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
	push!(ops, gr->DensZ(gr,a,stl, sVal,opt))
end

function DensZ(gr::mglGraph,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
  aDat = mglData(a)

    	ccall((:mgl_dens_z,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function ContX(ops::plotOpStack,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
	push!(ops, gr->ContX(gr,a,stl, sVal,opt))
end

function ContX(gr::mglGraph,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
  aDat = mglData(a)

    	ccall((:mgl_cont_x,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function ContX(ops::plotOpStack,v::Array,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
	push!(ops, gr->ContX(gr,v,a,stl, sVal,opt))
end

function ContX(gr::mglGraph,v::Array,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
  vDat = mglData(v)
  aDat = mglData(a)

    	ccall((:mgl_cont_x_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, vDat.data, aDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function ContY(ops::plotOpStack,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
	push!(ops, gr->ContY(gr,a,stl, sVal,opt))
end

function ContY(gr::mglGraph,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
  aDat = mglData(a)

    	ccall((:mgl_cont_y,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function ContY(ops::plotOpStack,v::Array,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
	push!(ops, gr->ContY(gr,v,a,stl, sVal,opt))
end

function ContY(gr::mglGraph,v::Array,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
  vDat = mglData(v)
  aDat = mglData(a)

    	ccall((:mgl_cont_y_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, vDat.data, aDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function ContZ(ops::plotOpStack,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
	push!(ops, gr->ContZ(gr,a,stl, sVal,opt))
end

function ContZ(gr::mglGraph,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
  aDat = mglData(a)

    	ccall((:mgl_cont_z,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function ContZ(ops::plotOpStack,v::Array,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
	push!(ops, gr->ContZ(gr,v,a,stl, sVal,opt))
end

function ContZ(gr::mglGraph,v::Array,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
  vDat = mglData(v)
  aDat = mglData(a)

    	ccall((:mgl_cont_z_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, vDat.data, aDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function ContFX(ops::plotOpStack,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
	push!(ops, gr->ContFX(gr,a,stl, sVal,opt))
end

function ContFX(gr::mglGraph,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
  aDat = mglData(a)

    	ccall((:mgl_contf_x,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function ContFX(ops::plotOpStack,v::Array,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
	push!(ops, gr->ContFX(gr,v,a,stl, sVal,opt))
end

function ContFX(gr::mglGraph,v::Array,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
  vDat = mglData(v)
  aDat = mglData(a)

    	ccall((:mgl_contf_x_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, vDat.data, aDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function ContFY(ops::plotOpStack,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
	push!(ops, gr->ContFY(gr,a,stl, sVal,opt))
end

function ContFY(gr::mglGraph,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
  aDat = mglData(a)

    	ccall((:mgl_contf_y,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function ContFY(ops::plotOpStack,v::Array,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
	push!(ops, gr->ContFY(gr,v,a,stl, sVal,opt))
end

function ContFY(gr::mglGraph,v::Array,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
  vDat = mglData(v)
  aDat = mglData(a)

    	ccall((:mgl_contf_y_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, vDat.data, aDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function ContFZ(ops::plotOpStack,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
	push!(ops, gr->ContFZ(gr,a,stl, sVal,opt))
end

function ContFZ(gr::mglGraph,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
  aDat = mglData(a)

    	ccall((:mgl_contf_z,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, aDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function ContFZ(ops::plotOpStack,v::Array,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
	push!(ops, gr->ContFZ(gr,v,a,stl, sVal,opt))
end

function ContFZ(gr::mglGraph,v::Array,a::Array,stl::String="", sVal::Number=mglNaN,opt::String="")
  vDat = mglData(v)
  aDat = mglData(a)

    	ccall((:mgl_contf_z_val,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Cdouble,Ptr{Cchar}), gr, vDat.data, aDat.data,pointer("$stl\0"), sVal,pointer("$opt\0"))
end

function FPlot(ops::plotOpStack,fy::String,stl::String="",opt::String="")
	push!(ops, gr->FPlot(gr,fy,stl,opt))
end

function FPlot(gr::mglGraph,fy::String,stl::String="",opt::String="")

    	ccall((:mgl_fplot,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fy\0"),pointer("$stl\0"),pointer("$opt\0"))
end

function FPlot(ops::plotOpStack,fx::String,fy::String,fz::String,stl::String,opt::String="")
	push!(ops, gr->FPlot(gr,fx,fy,fz,stl,opt))
end

function FPlot(gr::mglGraph,fx::String,fy::String,fz::String,stl::String,opt::String="")

    	ccall((:mgl_fplot_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fx\0"),pointer("$fy\0"),pointer("$fz\0"),pointer("$stl\0"),pointer("$opt\0"))
end

function FSurf(ops::plotOpStack,fz::String,stl::String="",opt::String="")
	push!(ops, gr->FSurf(gr,fz,stl,opt))
end

function FSurf(gr::mglGraph,fz::String,stl::String="",opt::String="")

    	ccall((:mgl_fsurf,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fz\0"),pointer("$stl\0"),pointer("$opt\0"))
end

function FSurf(ops::plotOpStack,fx::String,fy::String,fz::String,stl::String,opt::String="")
	push!(ops, gr->FSurf(gr,fx,fy,fz,stl,opt))
end

function FSurf(gr::mglGraph,fx::String,fy::String,fz::String,stl::String,opt::String="")

    	ccall((:mgl_fsurf_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar},Ptr{Cchar}), gr,pointer("$fx\0"),pointer("$fy\0"),pointer("$fz\0"),pointer("$stl\0"),pointer("$opt\0"))
end

function TriPlot(ops::plotOpStack,nums::Array,x::Array,y::Array,z::Array,c::Array,sch::String="",opt::String="")
	push!(ops, gr->TriPlot(gr,nums,x,y,z,c,sch,opt))
end

function TriPlot(gr::mglGraph,nums::Array,x::Array,y::Array,z::Array,c::Array,sch::String="",opt::String="")
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  cDat = mglData(c)

    	ccall((:mgl_triplot_xyzc,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, numsDat.data, xDat.data, yDat.data, zDat.data, cDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function TriPlot(ops::plotOpStack,nums::Array,x::Array,y::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->TriPlot(gr,nums,x,y,z,sch,opt))
end

function TriPlot(gr::mglGraph,nums::Array,x::Array,y::Array,z::Array,sch::String="",opt::String="")
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_triplot_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, numsDat.data, xDat.data, yDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function TriPlot(ops::plotOpStack,nums::Array,x::Array,y::Array,sch::String="",opt::String="")
	push!(ops, gr->TriPlot(gr,nums,x,y,sch,opt))
end

function TriPlot(gr::mglGraph,nums::Array,x::Array,y::Array,sch::String="",opt::String="")
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_triplot_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, numsDat.data, xDat.data, yDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function QuadPlot(ops::plotOpStack,nums::Array,x::Array,y::Array,z::Array,c::Array,sch::String="",opt::String="")
	push!(ops, gr->QuadPlot(gr,nums,x,y,z,c,sch,opt))
end

function QuadPlot(gr::mglGraph,nums::Array,x::Array,y::Array,z::Array,c::Array,sch::String="",opt::String="")
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  cDat = mglData(c)

    	ccall((:mgl_quadplot_xyzc,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, numsDat.data, xDat.data, yDat.data, zDat.data, cDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function QuadPlot(ops::plotOpStack,nums::Array,x::Array,y::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->QuadPlot(gr,nums,x,y,z,sch,opt))
end

function QuadPlot(gr::mglGraph,nums::Array,x::Array,y::Array,z::Array,sch::String="",opt::String="")
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_quadplot_xyz,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, numsDat.data, xDat.data, yDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function QuadPlot(ops::plotOpStack,nums::Array,x::Array,y::Array,sch::String="",opt::String="")
	push!(ops, gr->QuadPlot(gr,nums,x,y,sch,opt))
end

function QuadPlot(gr::mglGraph,nums::Array,x::Array,y::Array,sch::String="",opt::String="")
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)

    	ccall((:mgl_quadplot_xy,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, numsDat.data, xDat.data, yDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function TriCont(ops::plotOpStack,nums::Array,x::Array,y::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->TriCont(gr,nums,x,y,z,sch,opt))
end

function TriCont(gr::mglGraph,nums::Array,x::Array,y::Array,z::Array,sch::String="",opt::String="")
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_tricont_xyc,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, numsDat.data, xDat.data, yDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function TriContV(ops::plotOpStack,v::Array,nums::Array,x::Array,y::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->TriContV(gr,v,nums,x,y,z,sch,opt))
end

function TriContV(gr::mglGraph,v::Array,nums::Array,x::Array,y::Array,z::Array,sch::String="",opt::String="")
  vDat = mglData(v)
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_tricont_xycv,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, numsDat.data, xDat.data, yDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function TriCont(ops::plotOpStack,nums::Array,x::Array,y::Array,z::Array,a::Array,sch::String="",opt::String="")
	push!(ops, gr->TriCont(gr,nums,x,y,z,a,sch,opt))
end

function TriCont(gr::mglGraph,nums::Array,x::Array,y::Array,z::Array,a::Array,sch::String="",opt::String="")
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	ccall((:mgl_tricont_xyzc,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, numsDat.data, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function TriContV(ops::plotOpStack,v::Array,nums::Array,x::Array,y::Array,z::Array,a::Array,sch::String="",opt::String="")
	push!(ops, gr->TriContV(gr,v,nums,x,y,z,a,sch,opt))
end

function TriContV(gr::mglGraph,v::Array,nums::Array,x::Array,y::Array,z::Array,a::Array,sch::String="",opt::String="")
  vDat = mglData(v)
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	ccall((:mgl_tricont_xyzcv,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, numsDat.data, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function TriCont(ops::plotOpStack,v::Array,nums::Array,x::Array,y::Array,z::Array,a::Array,sch::String="",opt::String="")
	push!(ops, gr->TriCont(gr,v,nums,x,y,z,a,sch,opt))
end

function TriCont(gr::mglGraph,v::Array,nums::Array,x::Array,y::Array,z::Array,a::Array,sch::String="",opt::String="")
  vDat = mglData(v)
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	ccall((:mgl_tricont_xyzcv,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, numsDat.data, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function TriContVt(ops::plotOpStack,nums::Array,x::Array,y::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->TriContVt(gr,nums,x,y,z,sch,opt))
end

function TriContVt(gr::mglGraph,nums::Array,x::Array,y::Array,z::Array,sch::String="",opt::String="")
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_tricontv_xyc,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, numsDat.data, xDat.data, yDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function TriContVt(ops::plotOpStack,nums::Array,x::Array,y::Array,z::Array,a::Array,sch::String="",opt::String="")
	push!(ops, gr->TriContVt(gr,nums,x,y,z,a,sch,opt))
end

function TriContVt(gr::mglGraph,nums::Array,x::Array,y::Array,z::Array,a::Array,sch::String="",opt::String="")
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	ccall((:mgl_tricontv_xyzc,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, numsDat.data, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function TriContVt(ops::plotOpStack,v::Array,nums::Array,x::Array,y::Array,z::Array,a::Array,sch::String="",opt::String="")
	push!(ops, gr->TriContVt(gr,v,nums,x,y,z,a,sch,opt))
end

function TriContVt(gr::mglGraph,v::Array,nums::Array,x::Array,y::Array,z::Array,a::Array,sch::String="",opt::String="")
  vDat = mglData(v)
  numsDat = mglData(nums)
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	ccall((:mgl_tricontv_xyzcv,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, vDat.data, numsDat.data, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Dots(ops::plotOpStack,x::Array,y::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->Dots(gr,x,y,z,sch,opt))
end

function Dots(gr::mglGraph,x::Array,y::Array,z::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_dots,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Dots(ops::plotOpStack,x::Array,y::Array,z::Array,a::Array,sch::String="",opt::String="")
	push!(ops, gr->Dots(gr,x,y,z,a,sch,opt))
end

function Dots(gr::mglGraph,x::Array,y::Array,z::Array,a::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  aDat = mglData(a)

    	ccall((:mgl_dots_a,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, aDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Dots(ops::plotOpStack,x::Array,y::Array,z::Array,c::Array,a::Array,sch::String="",opt::String="")
	push!(ops, gr->Dots(gr,x,y,z,c,a,sch,opt))
end

function Dots(gr::mglGraph,x::Array,y::Array,z::Array,c::Array,a::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)
  cDat = mglData(c)
  aDat = mglData(a)

    	ccall((:mgl_dots_ca,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data, cDat.data, aDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function Crust(ops::plotOpStack,x::Array,y::Array,z::Array,sch::String="",opt::String="")
	push!(ops, gr->Crust(gr,x,y,z,sch,opt))
end

function Crust(gr::mglGraph,x::Array,y::Array,z::Array,sch::String="",opt::String="")
  xDat = mglData(x)
  yDat = mglData(y)
  zDat = mglData(z)

    	ccall((:mgl_crust,mgllib_name), Nothing, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Nothing},Ptr{Cchar},Ptr{Cchar}), gr, xDat.data, yDat.data, zDat.data,pointer("$sch\0"),pointer("$opt\0"))
end

function PutsFit(ops::plotOpStack, p::mglPoint,prefix::String=0,font::String="", size::Number=-1.0)
	push!(ops, gr->PutsFit(gr, p,prefix,font, size))
end

function PutsFit(gr::mglGraph, p::mglPoint,prefix::String=0,font::String="", size::Number=-1.0)

    	ccall((:mgl_puts_fit,mgllib_name), Nothing, (Ptr{Nothing},mreal,mreal,mreal,Ptr{Cchar},Ptr{Cchar},Cdouble), gr, p.x, p.y, p.z,pointer("$prefix\0"),pointer("$font\0"), size)
end

function Title(ops::plotOpStack, text::String, stl::String="", size::mreal=-2.0)
	push!(ops, gr->Title(gr, text, stl, size))
end

function Title(gr::mglGraph, text::String, stl::String="", size::mreal=-2.0)
        ccall((:mgl_title, mgllib_name), Nothing, (Ptr{Nothing}, Ptr{Cchar}, Ptr{Cchar}, Cdouble), gr, pointer("$text\0"), pointer("$stl\0"), size)
end

export mglGraph
export mglPoint
#export MglData
export draw
export draw!
export view

export SetWarn
export Message
export GetWarn
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
export clearLegend
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
export SetTickShift
export SetTicksVal
export SetTicks
export SetTranspType
export SetTuneTicks
export SetTickRotate
export SetTickSkip
export SetOriginTick
export SetTimeUTC
export ShowFrame
export ShowImage
export SpaghettiPlot
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
export WriteJSON
export WriteJPEG
export WriteOBJ
export WriteOBJold
export WriteOFF
export WritePRC
export WritePNG
export WriteSTL
export WriteSVG
export WriteTEX
export WriteTGA
export WriteXYZ
export Zoom
export ZoomAxis
export Title
end # module
