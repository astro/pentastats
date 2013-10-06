var app = angular.module('pentastats', []);

app.controller('MainController', function($scope, $http) {
    $scope.panes = [];
    $scope.addPane = function() {
	var last = $scope.panes[$scope.panes.length - 1];
	fileKey = last && last.fileKey;
	$scope.panes.push({
	    fileKey: fileKey
	});
    };
    $scope.addPane();

    $scope.paths = [];
    $http({
	method: 'GET',
	url: "data/index.json"
    }).success(function(data) {
	$scope.groups = {};
	var k;
	for(k in data)
	    if (data.hasOwnProperty(k)) {
		var ps = k.split(/\//g);
		var pLast = ps.pop();
		var xs = pLast.split(/\./);
		var base = ps.join("/") + "/" + xs[0];
		var ext = "" + xs[1];
		if (!$scope.groups.hasOwnProperty(base))
		    $scope.groups[base] = [];
		data[k].ext = ext;
		$scope.groups[base].push(data[k]);
	    }
	var paths = [];
	for(k in $scope.groups) {
	    var g = $scope.groups[k];
	    var downloads = 0, peak;
	    peak = null;
	    g.forEach(function(path) {
		downloads += path.downloads;
		if (!peak || peak > path.peak)
		    peak = path.peak;
	    });
	    paths.push({
		k: k,
		peak: peak,
		title: k + " (" + Math.ceil(downloads) + ")"
	    });
	}
	$scope.paths = paths.sort(function(p1, p2) {
	    if (p1.k < p2.k)
		return -1;
	    else if (p1.k > p2.k)
		return 1;
	    else
		return 0;
	});
    });
    // TODO: http error handling
});

app.directive('chartContainer', function() {
    return {
	link: function(scope, element, attrs) {
		// element.attr('width', window.innerWidth);
		// element.attr('height', 200);
	    var plot;

	    scope.$watch('data', function() {
		if (!scope.data)
		    return;
		if (plot) {
		    plot.shutdown();
		    $(element[0]).empty();
		}

		console.log("plot", scope.data);
		plot = $.plot(element[0], scope.data, {
		    xaxis: {
			mode: 'time',
			timeformat: "%Y-%m-%d"
		    }
		});
	    });
	}
    };
});

app.controller('PaneController', function($scope, $http) {
    $scope.$watch('pane.fileKey', function() {
	var g = $scope.groups && $scope.groups[$scope.pane.fileKey];
	if (!g)
	    return;

	$scope.data = [];
	g.forEach(function(path) {
	    $http({
		method: 'GET',
		url: "data/" + path.json + ".json"
	    }).success(function(data) {
		var series = {
		    bars: {
			show: true,
			barWidth: 86400 * 1000
		    },
		    data: []
		};
		for(var k in data)
		    if (/^\d{4}-\d{2}-\d{2}$/.test(k)) {
			var time = new Date(k).getTime();
			series.data.push([time, data[k]]);
		    }

		//series.data = series.data.sort();
		$scope.data.push(series);
		console.log("pushed", path.json);
		$scope.$emit('data');
	    });
	});
	// TODO: http error handling
    });
});

function pad(s, padding, len) {
    if (typeof s !== 'string')
	s = "" + s;

    while(s.length < len)
	s = padding + s;

    return s;
}

function fmtDate(d) {
    return pad(d.getUTCFullYear(), "0", 4) + "-" +
	pad(d.getUTCMonth(), "0", 2) + "-" +
	pad(d.getUTCDate(), "0", 2);
}
