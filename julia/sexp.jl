# The heavy-lifting of converting types to elisp happens here
"`string()` wrapper which escapes unsupported characters:
- `|`s are replaced by `\\vert{}`
- newlines are replaced by ` `"
function stringify(text)
    # To my understanding, newline is not supported inside cells, so we drop it
    reduce(replace,
           ('|' => "\\vert{}", "\n" => " "),
           init=string(text))
end


# TODO: instead of converting to string, convert them to a Sexp type,
# so that we can modify it before converting to string
wrap(x) = string("(", stringify(x), ")")
wrap(t::Tuple) = lst(t)
lisp(i) = i
lisp(i::Number) = i
lisp(n::Nothing) = "nil"
lisp(s::AbstractString) = string("\"", reduce(replace, ('\\' => "\\\\", '"' => "\\\""), init=s), "\"")
lst(x) = wrap(join(lisp.(x), " "))
lst(s::AbstractString) = wrap(lisp(s))

sexp(t::Tuple) = lst(t)
sexp(a::AbstractVector) = wrap(join(wrap.(lisp.(stringify.(a))), " "))
sexp(a::AbstractMatrix) =
    wrap(join([wrap(join(ObJulia.lisp.(ObJulia.stringify.(r)), " "))
               for r in eachrow(a)], " "))
sexp(s::StepRange) = wrap(join(wrap.(s), " "))
sexp(nt::NamedTuple) = wrap(join([wrap((k, nt[k])) for k in keys(nt)], " ")) 
sexp(a::Any) = "\"WARNING: Type $(typeof(a)) cannot be converted to sexp by ob-julia.\""
