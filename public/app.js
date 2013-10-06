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

	$scope.pane.divKey = 'geo';
	var datas = {
	    ext: {},
	    user_agents: {},
	    geo: {}
	};
	g.forEach(function(path) {
	    $http({
		method: 'GET',
		url: "data/" + path.json + ".json"
	    }).success(function(data) {
		/* Transpose & merge into datas.* */

		if (!datas.ext.hasOwnProperty(path.ext))
		    datas.ext[path.ext] = {};
		for(var day in data.downloads) {
		    if (!datas.ext[path.ext].hasOwnProperty(day))
			datas.ext[path.ext][day] = 0;
		    datas.ext[path.ext][day] += data.downloads[day];
		}

		for(var day in data.user_agents) {
		    for(var ua in data.user_agents[day]) {
			if (!datas.user_agents.hasOwnProperty(ua))
			    datas.user_agents[ua] = {};
			if (!datas.user_agents[ua].hasOwnProperty(day))
			    datas.user_agents[ua][day] = 0;
			datas.user_agents[ua][day] += data.user_agents[day][ua];
		    }
		}

		for(var day in data.geo) {
		    for(var country in data.geo[day]) {
			if (!datas.geo.hasOwnProperty(country))
			    datas.geo[country] = {};
			if (!datas.geo[country].hasOwnProperty(day))
			    datas.geo[country][day] = 0;
			datas.geo[country][day] += data.geo[day][country];
		    }
		}

		$scope.rerender();
	    });
	    // TODO: http error handling
	});

	$scope.rerender = function() {
	    $scope.data = [];
	    var ks = datas[$scope.pane.divKey];
	    for(var key in ks) {
		var series = {
		    bars: {
			show: true,
			barWidth: 86400 * 1000
		    },
		    data: []
		};
		for(var day in ks[key]) {
		    if (/^\d{4}-\d{2}-\d{2}$/.test(day)) {
			var time = new Date(day).getTime();
			series.data.push([time, ks[key][day]]);
		    } else
			console.warn("Inv k", k);
		}
		$scope.data.push(series);
	    }
	};
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
