# External (non-Stdlib) packages support
# To add support for new packages:
# 1. Add the package name as a Symbol in the supported_packages array
# 2. Add a define_$pkg function
# 3. That function should Main.@eval the required display() functions
# 4. Test it

"""List of symbols of package names supported by ob-julia.

Packages already included in a session get removed from this list."""
const supported_packages = []

"Call define_\$pkg function."
define_package_functions(pkg::Symbol) = (@eval $pkg)()

function OrgBabelReload()
    "Defines show methods based on packages loaded by the user in the
    current session."
    for pkg in supported_packages
        if isdefined(Main, pkg) && (isa(getfield(Main, pkg), Module) ||
                                    isa(getfield(Main, pkg), UnionAll))
            define_package_functions(Symbol("define_", pkg))
            # Remove loaded packages from list to prevent multiple execution
            filter!(x -> x != pkg, supported_packages)
        end
    end
end