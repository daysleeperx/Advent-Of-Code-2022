---
to: day0<%=day%>/<%=name%>.ipkg
---
package day<%=day%>

brief = "AoC 2022 Day <%=day%>: <%=name%>"
authors = "Viktor Pavlov"

modules = <%=name%>

main = <%=name%>
executable = interpret
outputdir = "."

depends = contrib
