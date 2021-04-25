# Display function for types defined in Julia standard library
using Dates
function display(d::ObJuliaDisplay, ::MIME"text/org",
                 i::Date; kwargs...) where T <: Any
    print(d.io, Dates.format(i, "[yyyy-mm-dd e]"))
end
function display(d::ObJuliaDisplay, ::MIME"text/org",
                 i::DateTime; kwargs...) where T <: Any
    print(d.io, Dates.format(i, "[yyyy-mm-dd e HH:MM]"))
end

function display(d::ObJuliaDisplay, ::MIME"text/org",
                 i::Array{T,2}; kwargs...) where T <: Any
    out = eachrow(i) |> x -> join([join(l, ',') for l in x], '\n')
    print(d.io, out)
end
