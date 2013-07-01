: '
  memstats.sh

  CREATED:  1 JULY 2013
  UPDATED:  1 JULY 2013
'

echo "System-reported free mem vs physical mem"
kstat -c pages -m unix -n system_pages -s freemem
kstat -c pages -m unix -n system_pages -s physmem

echo "Swap used vs swap cap"
kstat -c zone_memory_cap -s swap
kstat -c zone_memory_cap -s swapcap

echo "Memory used by kernel vs available"
kstat -c pages -m unix -n system_pages -s pp_kernel
kstat -c pages -m unix -n system_pages -s physmem

echo "Resident set size vs total"
kstat -c zone_memory_cap -s rss
kstat -c zone_memory_cap -s physcap
