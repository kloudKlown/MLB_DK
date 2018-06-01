var fs = require('fs');
var casper = require('casper').create({
    // other options here
    viewportSize: {
        width: 1200,
        height: 1500
    }
}); 

var utils = require('utils');

utils.dump(casper.cli.get(0));


var url  = "" + casper.cli.get(0);

// var pos = "" + casper.cli.get(1);

// Will be replaced by date
console.log(url)
casper.start(url, function() {
    this.echo('First Page: ' + this.getTitle());
});

casper.thenOpen(url, function() {	
	casper.wait(6000, function (){
  	this.capture('batters.jpg', undefined, {
	         format: 'jpg',
	         quality: 150
    	});
	var data = this.page.content;
	fs.write('C:/users/suhas/documents/Sports/MLB/parser/bin/BBSavantBatters.html', data, 'w');  
	});

}).wait(1).then(function() {

});


casper.run();