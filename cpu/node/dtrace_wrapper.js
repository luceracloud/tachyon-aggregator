var libdtrace = require('libdtrace');


module.exports.trace = function (type, socket, dtp_by_id, int_by_id, vars) {

	var script = {};

	/* Different dtrace scripts are stored here */
	script['lumpcpu'] = 'profile:::profile-4999\n{\n@P[pid,execname] = count();\n}';
	script['percpu'] = 'profile:::profile-4999\n{\n@P[pid,execname,cpu] = count();\n}';
	script['loaddist'] = 'profile:::profile-4999\n{\n@P[cpu] = count();\n}';
	script['syscall_proc'] = 'syscall:::entry\n/pid == ' + vars[0] + '/\n{\n@P[probefunc,execname] = ' +
	'count();\nself->start = timestamp;\n}\n\nsyscall:::return\n /pid == ' + vars[0] + '/\n{\n' +
	'self->stop = timestamp;\n@Qtot[probefunc,execname] = sum(self->stop - self->start);\n' +
	'@Qavg[probefunc,execname] = avg(self->stop - self->start);\n@Qmin[probefunc,execname] = ' +
	'min(self->stop - self->start);\n@Qmax[probefunc,execname] = max(self->stop - self->start);\n}';
	script['user_dist'] = '{\n@P[gid, uid] = count();\n}\n\nsyscall:::return\n{\n@Q[gid, uid] = count();\n}';
	script['heat'] = 'syscall:::entry\n{\nself->syscall_entry_ts[probefunc] = ' +
		'vtimestamp;\n}\nsyscall:::return\n/self->syscall_entry_ts[probefunc]/' +
		'\n{\n\n@time[probefunc] = lquantize((vtimestamp - self->' +
		'syscall_entry_ts[probefunc] ) / 1000, 0, 63, 2);\nself->' +
		'syscall_entry_ts[probefunc] = 0;\n}';

	/* Do stuff */
	var dtp = new libdtrace.Consumer();
	dtp.strcompile( script[type] );		
	dtp.go();
	dtp_by_id[socket.sessionId] = dtp;

	int_by_id[socket.sessionId] = setInterval(function () {
		try {
			var aggdata = {};
			dtp.aggwalk(function (id, key, val) {
				aggdata[key] = val;
			});
			socket.emit( 'message', aggdata);
		} catch( err ) {
			console.log(err);
		}
	}, 1001);
}