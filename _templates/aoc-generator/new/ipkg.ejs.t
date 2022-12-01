---
to: "<%= day ? `day${String(day).padStart(2, '0')}/${name}.ipkg` : null %>"
---
package day<%=day%>

brief = "AoC 2022 Day <%=day%>: <%=name%>"
authors = "Viktor Pavlov"

modules = <%=name%>

main = <%=name%>
executable = interpret
outputdir = "."

depends = contrib
