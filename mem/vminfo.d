#!/usr/sbin/dtrace -s
#pragma D option quiet

/*
 *  vminfo.d
 *
 *  Counts the number of VM calls (per process) and
 *  documents their occurrence. Displays PID, VM
 *  function, probe name, and counts.
 *
 *  USAGE: vminfo.d [INTERVAL] [COUNT]
 *
 *  [INTERVAL]  Length of interval in seconds
 *  [COUNT]     Number of intervals. 0 for unlimited
 *
 *  CREATED: 14 JUNE 2013
 *  UPDATED:  1 JULY 2013
 */

dtrace:::BEGIN
{
  printf("\nUsing intervals of length %d seconds.\n", $1);
  $2 ? printf("Running for %d intervals.\n", $2) :
    printf("Unlimited number of intervals. Press Ctrl-C to kill.\n");
  repeat = $2 ? $2 : -1;  /* Number of intervals        */
  interval = $1;          /* In seconds                 */
  CPU = $1;               /* Consider CPU?              */

  countdown = interval;
  rate = 0;
}

/* Every time there's a vm call, make note */
vminfo:::
{
  @P[pid, probefunc, probename] = count();
}

/* Decrement counters */
tick-1s 
{
  countdown--;
}

/* Print after each interval */
tick-1s
/countdown == 0/
{
  repeat--;
  countdown = interval;

  rate = 0;

  printf("\n%-7s %-17s %-12s %11s\n", "PID", "VM FUNCTION", "PROBE NAME", "COUNTS");
  printa("%-7d %-17s %-12s %11@d\n", @P);
  
  trunc(@P);
}

/* Exit program */
tick-1s
/repeat == 0/
{
  exit(0);
}

/* Cleanup */
dtrace:::END
{
  trunc(@P);
  printf("\nDone\n");
}