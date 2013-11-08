# Pentastats

[Demo](http://astro.github.io/pentastats/)

Download statistics made for podcasts. Splits data by:

* File type
* Geographic origin
* User-Agent

Maintains a state database so that you can even rotate your log files.


## Dependencies

	# pv is optional
    apt-get install -y ghc cabal-install libffi6 libgeoip1 libleveldb1 pv

Get [http://dev.maxmind.com/geoip/legacy/geolite/](GeoCityLite.dat)
and put it under `/usr/share/GeoIP/`


## Installation

```
cabal update
cabal install --only-dependencies .
cabal configure
cabal build
```


## Usage

**Step 1:** Pipe your logs into `pentalog`, eg:
```
pv -per < /var/log/apache2/access_log | ./pentalog
```

This will update a LevelDB in `state/`. Log entries don't have to be
ordered at this point. The database is persistent, so you may rotate
and delete log files after time, as long as you keep the `state/`
directory.


**Step 2:** `extract` aggregated data into `.json` files:
```
./extract
```

This will iterate the `state/` database in order of filenames and
dates to create the data files by the front-end in `public/data/`.


**Step 3:** serve the `public/` directory through a Web server. Go there
 with a browser and click through your new statistics.

Still too much resources to click? You can filter and aggregate them
in `public/config.js`

## Using Pentastats?

Show me your statistics! Also, you may contribute, especially to
`user-agent.filters`
