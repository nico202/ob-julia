# Display utilities
import Base.display
struct ObJuliaDisplay <: AbstractDisplay
    io::IO
end
# Display fallback for types we do not support
function display(d::ObJuliaDisplay, Any, x; kwargs...)
    show(d.io, x)
end
displayable(d::ObJuliaDisplay, M::MIME) = true

display(d::ObJuliaDisplay, x) = display(d, MIME("text/org"), x)
