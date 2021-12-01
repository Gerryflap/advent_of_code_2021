
function readLines(type::Type, path::String) :: Array{type}
    lines::Array{Int} = open(path) do file
    lines = Array(map(
        (line) -> parse(type, line),
        eachline(file)
    ))
    end
    return lines
end
