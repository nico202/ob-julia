include("display.jl")
include("packages.jl")

# Simple params accessors with fallback for src block params
param(name, fallback) = p -> something(get(p, name, fallback), fallback)
pure_p = param(:pure, false)
working_dir = param(:dir, pwd())
result(p) = get(p, :results, "") |> split
result_is_output(p) = "output" in result(p)
file_name = param(:file, nothing)

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

function drop_useless_trace(trace)
    "Remove from the stacktrace info about ob-julia, to have a cleaner
    output."
    idx = findfirst(f -> f.func == Symbol("top-level scope"), trace)
    trace[idx:end]
end

function org_eval(src, output_stream, dir=pwd(), mime=MIMES[""])
    """Safely evaluate `code'.  Store stdout and stderr to
       `output_stream' and return a tuple with the outcome of the
       evaluation and its result.  If an exception is thrown, the
       outcome is false and the trace is returned.
       Directory during evaluation is chanded `dir', which defaults 
       to the current directory.  
       The output will be printed to a display with mime type `mime'."""
    # Meta.parse parses only one expression, so we wrap the code in a
    # block.  It can either be a let block or a begin block.
    return cd(expanduser(dir)) do
        # TODO: support mime time on print calls?
        pushdisplay(ObJuliaDisplay(output_stream))
        cd(expanduser(dir)) do
            redirect_stdout(output_stream) do
                redirect_stderr(output_stream) do
                    try
                        (true, include(src))
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

function output_mime(filename; fallback="")
    "Determine the output MIME type based on filename.  Fallback to
    `fallback'."
    # ext might either be an empty string or an extension with a "."
    # prefix
    ext = splitext(filename)[end]
    # Remove the prefix if present, and return the correct mimetype.
    # If the the desired extension is not present in our MIMES dict,
    # fallback.
    get(MIMES, isempty(ext) ? fallback : ext[2:end], MIMES[fallback])
end

function OrgBabelEval(src_file, output_file, params, async_uuid=nothing)
    """ob-julia entry point.  Run the code contained in `src-file',
    wrapped in a block where variables defined in `vars-file' are set.
    The output is written to `output_file', according to config
    options defined in `params'."""
    function safe_mktemp(output)
        "Return a temporary file in the same dir as `output'.  
         Create the dir if it does not exists."
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
    success, result = org_eval(src_file, temporary_stream, working_dir(params), mime)
    # Now the code has been executed and imports have been imported.
    # We can reload supported display function so maybe one of them will be used
    OrgBabelReload()
    if ! success
        # Execution failed, write stacktrace file
        trace_file = string(output_file, ".trace")
        write(trace_file, join(result, "\n"))
    end
    if result_is_output(params)
        # Data has already been written to temporary_stream, close it
        # and move the file
        close(temporary_stream)
        # Write the result to the temporary file
        # replace the output file with the file in which we wrote our results
        mv(temporary_output, output_file, force=true)
    else
        # We need to write the output results to the output file
        io = IOBuffer()
        # Since display function might get re-defined during the
        # execution of this function (because of OrgBabelReload) we
        # want to be sure to call the latest version
        Base.invokelatest(display, ObJuliaDisplay(io), mime, result)
        write(output_file, take!(io))
    end
    if async_uuid !== nothing
        println("ob_julia_async_$(async_uuid)")
    end
end