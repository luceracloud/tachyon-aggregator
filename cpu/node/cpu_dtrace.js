/*	cpu_dtrace.lumped()
 *
 *		returns { [pid, execname], USE/5000 }
 */
module.exports.lumped = function (dtp) {

	var script = "profile:::profile-4999\n{\n@P[pid,execname] = count();\n}";
	var aggdata = {};

	try {
		dtp.aggwalk(function (id, key, val) {
			aggdata[key] = val;
		});
	} catch( err ) {
		console.log(err);
	}

	console.log(".lumped");

	return aggdata;

}

/*	cpu_dtrace.perCPU()
 *
 *		returns { [pid, execname], USE/5000 }
 */
module.exports.perCPU = function (dtp) {

	var script = "profile:::profile-4999\n{\n@P[pid,execname,cpu] = count();\n}";
	var aggdata = {};

	try {
		dtp.aggwalk(function (id, key, val) {
			aggdata[key] = val;
		});
	} catch( err ) {
		console.log(err);
	}

	console.log(".perCPU");

	return aggdata;

}
