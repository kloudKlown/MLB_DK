var fs = require('fs');
var casper = require('casper').create({
    // other options here
    viewportSize: {
        width: 1920,
        height: 8780
    }
}); 

var utils = require('utils');

utils.dump(casper.cli.get('date'));
var date = "" + casper.cli.get('date');

// console.log(date.length);
if (date.length == 7){
	date = '0' + date
}
console.log(date);

var url = 'https://www.fantasylabs.com/mlb/player-models/?date=' + date

console.log(url)

casper.start('http://www.fantasylabs.com/account/login/', function() {
    this.echo('First Page: ' + this.getTitle());
});


casper.then(function() {
	    this.fillSelectors('form', {
        'input[name="input"]':    'suhas.servesh@gmail.com',
        'input[name="password"]':    '30102996Kross1'
    }, true);	    
	this.click('button[ng-click="login()"]');
	this.wait(2000);
});    

// console.log(d);
// d.setDate(d.getDate()-5)

casper.thenOpen(url, function() {
	var i = 1;

	
	casper.wait(6000, function () {

  	this.capture('foo.jpg', undefined, {
	         format: 'jpg',
	         quality: 150
    	});

	 var data = this.page.content;
	 fs.write('C:/users/suhas/documents/Sports/MLB/parser/bin/Batters.html', data, 'w');  
	});

     


	console.log('test1');

}).wait(1000).then(function() {

var x = require('casper').selectXPath;
casper.thenClick(x('//*[@id="models-filters"]/div/nav/ul/li[2]/a'), function () {
casper.wait(6000, function () {

  	this.capture('foo2.jpg', undefined, {
	         format: 'jpg',
	         quality: 150
    	});
  });

	 var data = this.page.content;
	 fs.write('C:/users/suhas/documents/Sports/MLB/parser/bin/Pitchers.html', data, 'w');  

});

});


casper.run();