var url ='https://www.realclearpolitics.com/epolls/2020/president/us/2020_democratic_presidential_nomination-6730.html';
var page = new WebPage();
var fs = require('fs');

page.open(url, function (status) {
        just_wait();
});

function just_wait() {
    setTimeout(function() {
               fs.write('1.html', page.content, 'w');
            phantom.exit();
    }, 2500);
}

