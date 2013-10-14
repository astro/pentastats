function mapPath(path) {
    path = path.
	replace(/^https:\/\//, "http://").
	replace(/^http:\/\/www\./, "http://");

    return path;
};
