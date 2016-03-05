import Base.Multimedia: writemime

# Methods for mglGraph
function writemime(io::IO, m::MIME"text/html", gr::mglGraph)
    name = "MathGL-"*randstring()*".png"
    WriteFrame(gr, name)
    writemime(io, m, HTML("<img src='$name'>"))
end

function writemime(io::IO, m::MIME"text/latex", gr::mglGraph)
    name = "MathGL-"*randstring()*".png"
    WriteFrame(gr, name)
    write(io, "\\includegraphics[width=\\textwidth]{$name}")
end

function writemime(io::IO, m::MIME"application/x-latex", gr::mglGraph)
    name = "MathGL-"*randstring()*".png"
    WriteFrame(gr, name)
    writemime(io, m, "\\includegraphics[width=\\textwidth]{$name}")
end

function writemime(io::IO, m::MIME"image/png", gr::mglGraph)
    name = tempname()"*.png"
    WriteFrame(gr, name)
    file = open(name, "r")
    write(io, readbytes(file))
    close(file)
end


# Methods for a plotOpStack
function writemime(io::IO, m::MIME"text/html", ops::MathGL.plotOpStack)
    name = "MathGL-"*randstring()*".png"
    WriteFrame(draw(ops), name)
    writemime(io, m, HTML("<img src='$name'>"))
end 


function writemime(io::IO, m::MIME"text/latex", ops::MathGL.plotOpStack)
    name = "MathGL-"*randstring()*".png"
    WriteFrame(draw(ops), name)
    write(io, "\\includegraphics[width=\\textwidth]{$name}")
end 


function writemime(io::IO, m::MIME"application/x-latex", ops::MathGL.plotOpStack)
    name = "MathGL-"*randstring()*".png"
    WriteFrame(draw(ops), name)
    writemime(io, m, "\\includegraphics[width=\\textwidth]{$name}")
end 

function writemime(io::IO, m::MIME"image/png", ops::MathGL.plotOpStack)
    name = tempname()*".png"
    WriteFrame(draw(ops), name)
    file = open(name, "r")
    write(io, readbytes(file))
    close(file)
end

export writemime
