# Display function for types defined in Julia standard library
using Dates

fmt(date::Date) = Dates.format(date, "yyyy-mm-dd e")
fmt(date::DateTime) = Dates.format(date, "yyyy-mm-dd e HH:MM")

inactive(d) = string("[", fmt(d), "]")
inactive(d::String) = string("[", d, "]")

stringify(d::Union{Date,DateTime}) = inactive(d)

display(d::ObJuliaDisplay, ::MIME"text/org", i::T;
        kwargs...) where T <: Union{Date,DateTime} =
            verbatim(d, inactive(i))

# days (d), weeks (w), months (m), or years (y)
unit(x::Day) = "d"; unit(x::Week) = "w"; unit(x::Month) = "m"; unit(x::Year) = "y";
