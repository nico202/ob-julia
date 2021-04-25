# Display function for types defined in Julia standard library
function display(d::ObJuliaDisplay, ::MIME"text/org",
                 i::Array{T,2}; kwargs...) where T <: Any
    out = eachrow(i) |> x -> join([join(l, ',') for l in x], '\n')
    print(d.io, out)
end

display(d::ObJuliaDisplay, ::MIME"text/org", t::Tuple; kwargs...) =
    print(d.io, join(t, ','))
display(d::ObJuliaDisplay, ::MIME"text/org", t::Tuple; kwargs...) =
    print(d.io, join(t, ','))
display(d::ObJuliaDisplay, ::MIME"text/org", ::Nothing; kwargs...) =
    print(d.io, "")
display(d::ObJuliaDisplay, ::MIME"text/org", a::Array{T,1}; kwargs...) where T <: Any =
    print(d.io, join(a, '\n'))

function display(d::ObJuliaDisplay, ::MIME"text/html", i::Array{T,2};
                 kwargs...) where T <: Any
    width = get(Dict(kwargs), :width, "100")
    println(d.io, """<table style="width:$width%">""")
    content = eachrow(i) |> x -> string("<tr>",
                                        join([string("<th>", join(l, "</th><th>"))
                                              for l in x], "</tr><tr>\n"))
    print(d.io, content, "</table>")
end

function display(d::ObJuliaDisplay, ::MIME"text/org", i::Array{T,2};
                 kwargs...) where T <: Any
    out = eachrow(i) |> x -> join([join(l, ',') for l in x], '\n')
    print(d.io, out)
end

function display(d::ObJuliaDisplay, ::MIME"text/csv", i::Array{T,2};
                 kwargs...) where T <: Any
    orgshow(d.io, MIME("text/org"), i; kwargs...)
end

function display(d::ObJuliaDisplay, ::MIME"text/org", t::NamedTuple)
    print(d.io,
          join([string(k, ",", string(t[k])) for k in keys(t)],
               "\n"))
end

"""Format a named vector of named tuple as a table."""
function display(d::ObJuliaDisplay, ::MIME"text/org", nt::Vector{<:NamedTuple})
    length(nt) == 0 && return ""
    # This assume keys are the same.
    # check that all the keys are equal
    if length(nt) > 1 && length(unique([keys(x) for x in nt])) == 1
        # Format as a table
        println(d.io, join(keys(first(nt)), ','))
        for t in nt
            print(d.io, join(string.(values(t)), ','))
            println(d.io)
        end
    else
        for t in nt
            ks = keys(t)
            print(d.io,
                  join([string(k, ",", string(t[k])) for k in ks],
                       ","))
            println(d.io)
        end
    end
end
