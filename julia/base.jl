# Display functions for types defined in Julia base
display(d::ObJuliaDisplay, ::MIME"text/org", t::Tuple; kwargs...) =
        table(d, sexp(t))
display(d::ObJuliaDisplay, ::MIME"text/org", ::Nothing; kwargs...) =
    result_is_auto(kwargs) ? verbatim(d, "") : verbatim(d, "()")

display(d::ObJuliaDisplay, ::MIME"text/org", a::AbstractArray{T,1};
        kwargs...) where T <: Any = table(d, sexp(a))
display(d::ObJuliaDisplay, ::MIME"text/org", s::AbstractString;
        kwargs...) where T <: Any = verbatim(d, s)

function display(d::ObJuliaDisplay, ::MIME"text/html", i::AbstractArray{T,2};
                 kwargs...) where T <: Any
    width = param(:width, "100")(kwargs)
    verbatim(d)
    println(d.io, """<table style="width:$width%">""")
    content = string("<tr>\n",
                     join([string("<th>", join(l, "</th><th>"))
                           for l in eachrow(i)], "</tr><tr>\n"))
    print(d.io, content, "</table>")
end

display(d::ObJuliaDisplay, ::MIME"text/org", i::AbstractArray{T,2};
                 kwargs...) where T <: Any = table(d, sexp(i))

function display(d::ObJuliaDisplay, ::MIME"text/csv", i::AbstractArray{T,2};
                 kwargs...) where T <: Any
    out = eachrow(i) |> x -> join([join(l, ',') for l in x], '\n')
    write(d.io, out)
end

display(d::ObJuliaDisplay, ::MIME"text/org", t::NamedTuple; kwargs...) =
    table(d, sexp(t))

"""Format a vector of named tuple as a table."""
function display(d::ObJuliaDisplay, ::MIME"text/org", nt::Vector{<:NamedTuple};
                 kwargs...)
    length(nt) == 0 && return table(d, "")
    # This assume keys are the same.
    # check that all the keys are equal
    if length(nt) > 1 && length(unique([keys(x) for x in nt])) == 1
        # Format as a table
        table(d, "(")
        print(d.io, lst(keys(first(nt))))
        for t in nt
            print(d.io, lst(values(t)))
            println(d.io)
        end
        print(d.io, ")")
    else
        table(d, "(")
        for t in nt
            ks = keys(t)
            print(d.io, lst(Iterators.flatten([(k, t[k]) for k in ks])))
        end
        print(d.io, ")")
    end
end

function display(d::ObJuliaDisplay, ::MIME"text/org", a::AbstractDict;
                 kwargs...) where T <: Any
    table(d, wrap(join(wrap.(reverse(join.([[lisp(stringify(k)), lisp(stringify(v))] for (k, v) in a], " "))), " ")))
end
