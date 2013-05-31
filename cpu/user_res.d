#!/usr/sbin/dtrace -s
#pragma D option quiet

/*
 *  user_dist.d
 *
 *
 * 	CREATED: 30 MAY 2013
 * 	UPDATED: 30 MAY 2013
 */

dtrace:::BEGIN
{
/*
	$1 ? printf("\nConsidering different CPUs\n") :
		printf("\nLumping all CPUs together\n");
	printf("Using intervals of length %d seconds.\n", $2);
	$3 ? printf("Running for %d intervals.\n", $3) :
		printf("Unlimited number of intervals. Press Ctrl-C to kill.\n"); */
	repeat = 0;  	/* Number of intervals  			*/
	interval = 1;					/* In seconds           			*/

	UID = 0;

	countdown = interval;
}

/* Make note of syscalls */
syscall:::entry
/uid == UID/
{
	@P[probefunc] = count();
	@Pproc[probefunc, pid, execname] = count();
	self->start = timestamp;
}

/* Record completion of syscall */
syscall:::return 
/uid == UID/
{
	self->stop = timestamp;
	@Qtot[probefunc] = sum(self->stop - self->start);
	@Qavg[probefunc] = avg(self->stop - self->start);
	@Qmin[probefunc] = avg(self->stop - self->start);
	@Qmax[probefunc] = avg(self->stop - self->start);
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

	printf("\n%-14s %6s | %12s %10s %10s %10s\n", "SYSCALL", "COUNT", "TIME: TOTAL", "AVERAGE", "MAX", "MIN");
	printa("%-14s %6@d | %12@d %10@d %10@d %10@d\n", @P, @Qtot, @Qavg, @Qmax, @Qmin);
	
	trunc(@P);
	trunc(@Pproc);
	trunc(@Qtot);
	trunc(@Qavg);
	trunc(@Qmax);
	trunc(@Qmin);
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
	trunc(@Qtot);
	trunc(@Qavg);
	trunc(@Qmax);
	printf("\nDone\n");
}
