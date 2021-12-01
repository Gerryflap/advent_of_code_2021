module JuliaCode
    # Days will be included and functions exported like this
    include("days/Day1.jl")
    export day1q1, day1q2

    # Warning: very complicated!
    greet() = print("Hello World!")

    function main()
        day1q1()
        day1q2()
    end
end # module
