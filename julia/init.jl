module ObJulia

include("display.jl")
include("sexp.jl")
include("base.jl")
include("stdlib.jl")
include("packages.jl")

# Simple params accessors with fallback for src block params
param(name, fallback) = p -> something(get(p, name, fallback), fallback)
pure_p = param(:pure, false)
working_dir = param(:dir, pwd())
result(p) = get(p, :results, "") |> split
result_is_output(p) = "output" in result(p)
result_is_raw(p) = "raw" in result(p)
result_is_matrix(p) = "matrix" in result(p)
result_is_table(p) = "table" in result(p)
result_is_list(p) = "list" in result(p)
result_is_auto(p) = all(.![result_is_raw(p),
                           result_is_list(p),
                           result_is_matrix(p),
                           result_is_table(p)])
file_name = param(:file, nothing)

# Result available result types

"Return a function which takes two arguments, the display and the
content to write to.  That function writes the content to the display,
prepended by the type `t`."
function write_type(t)
    "Writes `t` followed by content to `d`."
    function wrt(d::ObJuliaDisplay, content="")
        println(d.io, t)
        write(d.io, content)
    end
    return wrt
end
raw = write_type("raw")
table = write_type("table")
matrix = write_type("matrix")
verbatim = write_type("verbatim")
list = write_type("list")

const MIMES = Dict(
    # keep those sorted :)
    ""     => MIME("text/org"),
    "csv"  => MIME("text/csv"),
    "eps"  => MIME("image/eps"),
    "html" => MIME("text/html"),
    "org"  => MIME("text/org"),
    "pdf"  => MIME("application/pdf"),
    "png"  => MIME("image/png"),
    "ps"   => MIME("application/postscript"),
    "svg"  => MIME("image/svg+xml"),
    "tex"  => MIME("application/x-tex"))

"""Remove from the stacktrace info about ob-julia, to have a cleaner
output."""
function drop_useless_trace(trace)
    idx = findfirst(f -> f.func == Symbol("top-level scope"), trace)
    trace[idx:end]
end

"""Safely evaluate `code'.  Store stdout and stderr to `output_stream'
and return a tuple with the outcome of the evaluation and its result.
If an exception is thrown, the outcome is false and the trace is
returned.  Directory during evaluation is chanded `dir', which
defaults to the current directory.  The output will be printed to a
display with mime type `mime'."""
function org_eval(src, output_stream, dir=pwd(), mime=MIMES[""])
    # Meta.parse parses only one expression, so we wrap the code in a
    # block.  It can either be a let block or a begin block.
    return cd(expanduser(dir)) do
        # TODO: support mime time on print calls?
        pushdisplay(ObJuliaDisplay(output_stream))
        cd(expanduser(dir)) do
            redirect_stdout(output_stream) do
                redirect_stderr(output_stream) do
                    try
                        (true, Base.include(Main, src))
                    catch e
                        # There's an evaluation error, store it both
                        # as output and return as result
                        errbuf = IOBuffer()
                        showerror(errbuf, e)
                        err = String(take!(errbuf))
                        (false, [err, drop_useless_trace(stacktrace())...])
                    finally
                        popdisplay()
                    end
                end
            end
        end
    end
end

"""Determine the output MIME type based on filename.  Fallback to
`fallback`."""
function output_mime(filename; fallback="")
    # ext might either be an empty string or an extension with a "."
    # prefix
    ext = splitext(filename)[end]
    # Remove the prefix if present, and return the correct mimetype.
    # If the the desired extension is not present in our MIMES dict,
    # fallback.
    get(MIMES, isempty(ext) ? fallback : ext[2:end], MIMES[fallback])
end

"""ob-julia entry point.  Run the code contained in `src-file`,
wrapped in a block where variables defined in `vars-file` are set.
The output is written to `output_file`, according to config
options defined in `params`."""
function OrgBabelEval(src_file, output_file, params, async_uuid=nothing)
    "Return a temporary file in the same dir as `output`.
     Create the dir if it does not exists."
    function safe_mktemp(output)
        dir = dirname(output)
        mkpath(dir)
        mktemp(dir)
    end
    # Create a new output file where Julia will store its results in
    # the same dir where ob-julia expect its ouput file
    temporary_output, temporary_stream = safe_mktemp(output_file)
    # Parse the params (named tuple passed by ob-julia)
    params = Main.eval(Meta.parse(params))
    mime = output_mime(output_file)
    latexify = something(params[:latexify], "nil") != "nil"
    # If results is output, running the code will start writing data
    # directly on the output file.  That's ok, but we need to tell
    # ob-julia the way this data is formatted.  We have no idea, so
    # let's use raw.  We might use header arguments for things like
    # images
    if result_is_output(params)
        println(temporary_stream, "raw")
    end
    success, result = org_eval(src_file, temporary_stream, working_dir(params), mime)
    # Now the code has been executed and imports have been imported.
    # We can reload supported display function so maybe one of them will be used
    OrgBabelReload()
    if ! success
        # Execution failed, write stacktrace file
        trace_file = string(output_file, ".trace")
        write(output_file, "")
        write(trace_file, join(result, "\n"))
    end
    if result_is_output(params)
        # Data has already been written to temporary_stream, close it
        # and move the file
        close(temporary_stream)
        # Write the result to the temporary file
        # replace the output file with the file in which we wrote our results
        mv(temporary_output, output_file, force=true)
    elseif result_is_raw(params)
        write(output_file, string("raw\n", result))
    else
        # We need to write the output results to the output file
        io = IOBuffer()
        # Since display function might get re-defined during the
        # execution of this function (because of OrgBabelReload) we
        # want to be sure to call the latest version
        Base.invokelatest(display, ObJuliaDisplay(io),
                          if mime == MIME("text/org") && latexify
                              MIME("text/org+latexify")
                          else mime end, result; params...)
        write(output_file, take!(io))
    end
    if async_uuid !== nothing
        println("ob_julia_async_$(async_uuid)")
    end
end

end # module
