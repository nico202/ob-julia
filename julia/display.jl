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
