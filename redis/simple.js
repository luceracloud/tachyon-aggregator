require('crontab').load(cronLoaded);
 
function cronLoaded(err, tab) {
    if (err) { console.log(err); process.exit(1); }
 
    var command = '/usr/bin/date';
    tab.remove(tab.findCommand(command));
    console.log("We Made It 1");
    console.log(command);
 
    var item = tab.create(command);
    console.log(item);
    item.everyReboot();
    console.log(item);
    tab.save(cronSaved);
    console.log("We Made it 4");
}
function cronSaved(err, tab) {
    if (err) { console.log(err);  process.exit(1); }
 
    console.log('saved');
}
