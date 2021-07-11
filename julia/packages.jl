# External (non-Stdlib) packages support
# To add support for new packages:
# 1. Add the package name as a Symbol in the supported_packages array
# 2. Add a define_$pkg function
# 3. That function should @eval the required display() functions
# 4. Test it

"""List of symbols of package names supported by ob-julia.

Packages already included in a session get removed from this list."""
const supported_packages = [
    :LinearAlgebra,
    :DataFrames,
    :Latexify, :LaTeXStrings,
    :Gadfly, :Plots]

"Call define_\$pkg function."
define_package_functions(pkg::Symbol) = (@eval $pkg)()

"Defines show methods based on packages loaded by the user in the
current session."
function OrgBabelReload()
    for pkg in supported_packages
        if isdefined(Main, pkg) && (isa(getfield(Main, pkg), Module) ||
                                    isa(getfield(Main, pkg), UnionAll))
            define_package_functions(Symbol("define_", pkg))
            # Remove loaded packages from list to prevent multiple execution
            filter!(x -> x != pkg, supported_packages)
        end
    end
end

function define_LinearAlgebra()
    @eval sexp(a::Main.LinearAlgebra.Adjoint{AbstractMatrix}) = sexp(collect(a))
    @eval sexp(a::Main.LinearAlgebra.Adjoint{AbstractVector}) = wrap(join(lisp.(stringify.(a)), " "))
end

function define_LaTeXStrings()
    @eval function display(d::ObJuliaDisplay, ::MIME"text/org",
                           l::Main.LaTeXString; kwargs...)
        verbatim(d, String(l))
    end
end

function define_Latexify()
    # Latexify outputs LaTeXStrings.LaTeXString objects, but
    # LaTeXStrings is not included into Main now.  That's why we have
    # to define this method here
    @eval function display(d::ObJuliaDisplay, ::MIME"text/org",
                           l::Main.Latexify.LaTeXStrings.LaTeXString; kwargs...)
        verbatim(d, String(l))
    end
    # We want to latexify anything we can if the output is a tex file
    @eval function display(d::ObJuliaDisplay, ::MIME"application/x-tex",
                           obj::Any; kwargs...)
        verbatim(d, Main.latexify(obj))
    end
end

function define_DataFrames()
    @eval function display(d::ObJuliaDisplay, ::MIME"text/csv",
                           df::Main.DataFrame; kwargs...)
        # text/org is just a csv which org parses automatically, so we
        # can fallback to it
        out = join(string.(names(df)), ',') * '\n'
        out *= join([join(x, ',') for x in eachrow(df) .|> collect],'\n')
        write(d.io, out)
    end
    @eval function display(d::ObJuliaDisplay, ::MIME"text/org",
                           df::Main.DataFrame; kwargs...)
        table(d, string("((", join(lisp.(names(df)), " "), ") hline ",
                        sexp(Matrix(df))[2:end]))
    end
    @eval function display(d::ObJuliaDisplay, ::MIME"text/org+latexify", df::Main.DataFrame; kwargs...)
        display(d, MIME("text/org"), df; kwargs...)
    end
end

function define_Gadfly()
    @eval interpretlength(_::Nothing, default) = default
    @eval interpretlength(length::Int, default) = length * Main.Gadfly.inch
    @eval function interpretlength(length::String, default)
        m = match(r"^([\d.]+)([a-z]+)$", length)
        units = Dict("mm" => Main.Gadfly.mm,
                     "cm" => Main.Gadfly.cm,
                     "pt" => Main.Gadfly.pt,
                     "px" => Main.Gadfly.px,
                     "in" => Main.Gadfly.inch,
                     "inch" => Main.Gadfly.inch)
        if ! isnothing(m)
            parse(Float64, m.captures[1]) * get(units, m.captures[2], Main.Gadfly.inch)
        else
            default
        end
    end
    @eval function display(d::ObJuliaDisplay, ::MIME"text/org",
                           p::Main.Gadfly.Plot; height=nothing, width=nothing, output_dir=nothing, file_ext=nothing, kwargs...)
        filename = string(tempname(if ! isnothing(output_dir) output_dir else tempdir() end),
                          if isnothing(file_ext) ".svg" else "." * file_ext end)
        width = interpretlength(width, √200*Main.Gadfly.cm)
        height = interpretlength(height, 10*Main.Gadfly.cm)
        Main.Gadfly.draw(Main.Gadfly.SVG(filename, width, height), p)
        verbatim(d, filename)
    end
    @eval function saveplot(d::ObJuliaDisplay, formatter, p::Main.Gadfly.Plot; height=nothing, width=nothing)
        width = interpretlength(width, √200*Main.Gadfly.cm)
        height = interpretlength(height, 10*Main.Gadfly.cm)
        Main.Gadfly.draw(formatter(d.io, width, height), p)
    end
    @eval display(d::ObJuliaDisplay, ::MIME"image/svg+xml", p::Main.Gadfly.Plot; height=nothing, width=nothing, kwargs...) =
        saveplot(d, Main.Gadfly.SVG, p; height, width)
    @eval display(d::ObJuliaDisplay, ::MIME"image/png", p::Main.Gadfly.Plot; height=nothing, width=nothing, kwargs...) =
        saveplot(d, Main.Gadfly.PNG, p; height, width)
    @eval display(d::ObJuliaDisplay, ::MIME"image/png", p::Main.Gadfly.Plot; height=nothing, width=nothing, kwargs...) =
        saveplot(d, Main.Gadfly.PNG, p; height, width)
    @eval display(d::ObJuliaDisplay, ::MIME"application/pdf", p::Main.Gadfly.Plot; height=nothing, width=nothing, kwargs...) =
        saveplot(d, Main.Gadfly.PDF, p; height, width)
    @eval display(d::ObJuliaDisplay, ::MIME"application/postscript", p::Main.Gadfly.Plot; height=nothing, width=nothing, kwargs...) =
        saveplot(d, Main.Gadfly.PS, p; height, width)
end

function define_Plots()
    @eval display(d::ObJuliaDisplay, mime::M, p::Main.Plots.Plot; kwargs...) where
    { M <: Union{MIME"image/png", MIME"image/svg+xml", MIME"application/pdf", MIME"application/postscript",
                 MIME"image/eps", MIME"application/x-tex", MIME"text/html"}}=
                     show(d.io, mime, p)
    @eval display(d::ObJuliaDisplay, mime::MIME"text/org", p::Main.Plots.Plot; kwargs...) =
        (verbatim(d); show(d.io, MIME("text/plain"), p))
end
