import Base.show
import Base64.stringmime

# Methods for mglGraph
function show(io::IO, m::MIME"text/html", gr::mglGraph)
    htm = """<img id="blah" src="data:image/png;base64,"""
    htm *= stringmime("image/png", gr)
    htm *= "\" />\n"
    show(io, m, HTML(htm))
end

function show(io::IO, m::MIME"text/latex", gr::mglGraph)
    name = "MathGL-"*randstring()*".png"
    WriteFrame(gr, name)
    write(io, "\\includegraphics[width=\\textwidth]{$name}")
end

function show(io::IO, m::MIME"application/x-latex", gr::mglGraph)
    name = "MathGL-"*randstring()*".png"
    WriteFrame(gr, name)
    show(io, m, "\\includegraphics[width=\\textwidth]{$name}")
end

function show(io::IO, m::MIME"image/png", gr::mglGraph)
    name = tempname()*".png"
    WriteFrame(gr, name)
    file = open(name, "r")
    write(io, readbytes(file))
    close(file)
    rm(name)
end


# Methods for a plotOpStack
function show(io::IO, m::MIME"text/html", ops::MathGL.plotOpStack)
    htm = """<img id="blah" src="data:image/png;base64,"""
    htm *= stringmime("image/png", ops)
    htm *= "\" />\n"
    show(io, m, HTML(htm))
end


function show(io::IO, m::MIME"text/latex", ops::MathGL.plotOpStack)
    name = "MathGL-"*randstring()*".png"
    WriteFrame(draw(ops), name)
    write(io, "\\includegraphics[width=\\textwidth]{$name}")
end


function show(io::IO, m::MIME"application/x-latex", ops::MathGL.plotOpStack)
    name = "MathGL-"*randstring()*".png"
    WriteFrame(draw(ops), name)
    show(io, m, "\\includegraphics[width=\\textwidth]{$name}")
end

function show(io::IO, m::MIME"image/png", ops::MathGL.plotOpStack)
    name = tempname()*".png"
    WriteFrame(draw(ops), name)
    file = open(name, "r")
    write(io, read(file))
    close(file)
    rm(name)
end
