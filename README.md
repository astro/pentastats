# Pentastats

Download statistics made for podcasts. Splits data by:

* File type
* Geographic origin
* User-Agent

Maintains a state database so that you can even rotate your log files.


## Dependencies

    apt-get install -y libffi6 libgeoip1 libleveldb1
    # pv comes in handy too

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

*Step 1:* Pipe your logs into `pentalog`, eg:
```
pv -per < /var/log/apache2/access_log | ./pentalog
```

This will update a LevelDB in `state/`. Log entries don't have to be
ordered at this point.


*Step 2:* `extract` aggregated data into `.json` files:
```
./extract
```

This will iterate the `state/` database in order of filenames and
dates to create the data files by the front-end in `public/data/`.


*Step 3:* serve the `public/` directory through a Web server. Go there
 with a browser and click through your new statistics.
