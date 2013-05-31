#!/usr/sbin/dtrace -s
#pragma D option quiet

/*
 *  thread_cpu.d
 *
 */

dtrace:::BEGIN
{
	printf("\nConsidering CPU %d\n", $1);
	printf("Using intervals of length %d seconds.\n", $2);
	$3 ? printf("Running for %d intervals.\n", $3) :
		printf("Unlimited number of intervals. Press Ctrl-C to kill.\n");
	interval = $2; 	/* In seconds           */
	repeat = $3;  	/* Number of intervals  */
	CPU = $1;				/* Consider CPU? 				*/

	countdown = interval;
}

/* Make note of running programs */
profile-4999
/cpu == CPU/
{
	@P[pid,curthread,execname] = count();
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

	normalize(@P, 50);

	printf("\n%-6s %-18s %-16s %11s\n", "PID", "TID", "PROCESS NAME", "USAGE(CPU)");
	printa("%-6d %-18p %-16s %11@d\n", @P);
	
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

