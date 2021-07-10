# Display utilities
import Base.display
struct ObJuliaDisplay <: AbstractDisplay
    io::IO
end
# Display fallback for types we do not support
function display(d::ObJuliaDisplay, Any, x; kwargs...)
    verbatim(d)
    show(d.io, x)
end
function display(d::ObJuliaDisplay, ::MIME"text/org", x; kwargs...)
    verbatim(d)
    show(d.io, MIME("text/plain"), x)
end
displayable(d::ObJuliaDisplay, M::MIME) = true

display(d::ObJuliaDisplay, x) = display(d, MIME("text/org"), x)

# Auto-Latexify

function display(d::ObJuliaDisplay, ::MIME"text/org+latexify", x; kwargs...)
    if ! isdefined(Main, :Latexify)
        try
            @eval using Latexify
            OrgBabelReload()
        catch
            return display(d, MIME"text/org", "\"Error: Latexify is not installed. Run `import Pkg; Pkg.add(\\\"Latexify\\\")` to rectify.\"")
        end
    end
    try
        verbatim(d, Main.latexify(x))
    catch
        display(d, MIME("text/org"), x; kwargs...)
    end
end

# types it doesn't make sense to Latexify
function display(d::ObJuliaDisplay, ::MIME"text/org+latexify", x::T; kwargs...) where
    { T <: Union{Number, String} }
    display(d, MIME("text/org"), x; kwargs...)
end
