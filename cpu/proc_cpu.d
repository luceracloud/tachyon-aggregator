#!/usr/sbin/dtrace -s
#pragma D option quiet

/*
 *  proc_cpu.d
 *
 */

dtrace:::BEGIN
{
	printf("Looking at CPU: %d\n", $1);
	printf("Running for %d intervals of %d seconds each.\n", $3, $2);
	interval = $2; /* In seconds           */
	repeat = $3;   /* Number of intervals  */
	CPU = $1;			/* Consider CPU? 				*/

	countdown = interval;
}

/* Make note of running programs */
profile-4999
/cpu == CPU/
{
  /*printf("Process hit on CPU %d\n", cpu);*/
	@P[pid,execname] = count();
}

/* Decrement counters */
tick-1s 
{
	countdown--;
}

/* Print when user so wishes */
tick-1s
/countdown == 0/
{
	repeat--;
	countdown = interval;

	normalize(@P, 50);

	printf("\n%-6s %-14s %11s\n", "PID", "PROCESS NAME", "USAGE(CPU)");
	printa("%-6d %-14s %11@d\n", @P);
	
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


