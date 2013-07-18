var rdb = require('rdb-parser');
var fs = require('fs');

var parser = new rdb.Parser();

parser.on('entity', function(e) {
  console.log(e);
});

parser.on('error', function(err) {
 throw err;
});

parser.on('end', function() {
  console.log('done');
});

var s = fs.createReadStream('/opt/tools/redis/redis-2.6.14/src/dump.rdb');
s.pipe(parser);
