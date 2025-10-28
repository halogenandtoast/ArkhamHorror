find ./arkham-api -path "*/.stack-work/*" -name "*.dump-timings" -print0 \
| xargs -0 awk '
  function parse_num_after(tag,   i,j,c,n) {
    i = index($0, tag); if (i == 0) return ""
    j = i + length(tag); n = ""
    while (j <= length($0)) {
      c = substr($0, j, 1)
      if (c ~ /[0-9.]/) { n = n c; j++ } else break
    }
    return n
  }
  {
    # pass name: prefer text before " [", else before first ":"
    pass = ""
    i = index($0, " [")
    if (i > 1) { pass = substr($0, 1, i-1) }
    else {
      k = index($0, ":")
      if (k > 1) pass = substr($0, 1, k-1)
    }
    if (pass == "") next

    tn = parse_num_after("time=")
    an = parse_num_after("alloc=")
    if (tn != "") time[pass] += (tn + 0)      # seconds
    if (an != "") alloc[pass] += (an + 0)     # bytes
  }
  END {
    for (p in time) printf "%s\t%.1f\t%.0f\n", p, time[p]*1000, alloc[p]
  }' \
| sort -k2,2nr | head -n 20 | awk 'BEGIN{print "=== Top passes (ms, bytes) ==="} {printf "%-24s %10s ms   %12s bytes\n",$1,$2,$3}'
