-ifndef(PACKET_PB_H).
-define(PACKET_PB_H, true).
-record(packet, {
    host = erlang:error({required, host}),
    zone = erlang:error({required, zone}),
    time = erlang:error({required, time}),
    ticks,
    threads,
    processes,
    cpu = [],
    mem = [],
    net = [],
    disk = [],
    process = [],
    callfreq = [],
    zonenames = [],
    cpuqueue = []
}).
-endif.

-ifndef(PACKET_CPUQUEUE_PB_H).
-define(PACKET_CPUQUEUE_PB_H, true).
-record(packet_cpuqueue, {
    core,
    length,
    amount
}).
-endif.

-ifndef(PACKET_CALLFREQ_PB_H).
-define(PACKET_CALLFREQ_PB_H, true).
-record(packet_callfreq, {
    name = erlang:error({required, name}),
    time,
    value
}).
-endif.

-ifndef(PACKET_PROCESS_PB_H).
-define(PACKET_PROCESS_PB_H, true).
-record(packet_process, {
    pid,
    execname,
    usage,
    cpu
}).
-endif.

-ifndef(PACKET_DISK_PB_H).
-define(PACKET_DISK_PB_H, true).
-record(packet_disk, {
    instance = erlang:error({required, instance}),
    nread,
    nwritten,
    reads,
    writes,
    rtime,
    wtime,
    rlentime,
    wlentime,
    harderror,
    softerror,
    tranerror
}).
-endif.

-ifndef(PACKET_NET_PB_H).
-define(PACKET_NET_PB_H, true).
-record(packet_net, {
    instance,
    obytes64,
    rbytes64,
    opackets,
    ipackets,
    oerrors,
    ierrors
}).
-endif.

-ifndef(PACKET_MEM_PB_H).
-define(PACKET_MEM_PB_H, true).
-record(packet_mem, {
    rss,
    physcap,
    swap,
    swapcap,
    physmem,
    pp_kernel,
    freemem,
    nalloc_calls,
    nfree_calls,
    maj_fault,
    as_fault,
    pgin
}).
-endif.

-ifndef(PACKET_CPU_PB_H).
-define(PACKET_CPU_PB_H, true).
-record(packet_cpu, {
    core,
    usage
}).
-endif.

