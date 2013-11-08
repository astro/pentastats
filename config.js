function mapPath(path) {
    path = path.
	replace(/^https:\/\//, "http://").
	replace(/^http:\/\/www\./, "http://");

    if (/^http:\/\/ftp\.c3d2\.de\//.test(path))
	return path;
};
