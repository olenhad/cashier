var async = require('async');

var arr = [1,2,3,4,5,6,7,8,9,10];

async.forEachLimit(arr,3,
    function (item, callback){
        console.log(item);
        callback();
    },
    function(err){
        console.log(err);
    });
