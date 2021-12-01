include("../Util.jl")

function countIncreases(numbers::Array{Int}) :: Int
    increases::Int = 0
    for i in 1:length(numbers)-1
        if numbers[i] < numbers[i+1]
            increases += 1
        end
    end
    return increases
end

function conv3(numbers::Array{Int}) :: Array{Int}
    conved :: Array{Int} = zeros(length(numbers) - 2)
    for i in 1:length(conved)
        conved[i] = sum(numbers[i:i+2])
    end

    return conved
end

function day1q1()
    lines = readLines(Int, "data/2021_01/data")
    println("D1 Q1: Number of increases: ")
    println(countIncreases(lines))
end

function day1q2()
    lines = readLines(Int, "data/2021_01/data")
    println("D1 Q2: Number of increases in conv3-ed list: ")
    println(countIncreases(conv3(lines)))
end