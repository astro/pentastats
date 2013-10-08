# Install

    apt-get install -y libffi6 libgeoip1 libleveldb1
    # pv comes in handy too

Get [http://dev.maxmind.com/geoip/legacy/geolite/](GeoCityLite.dat)
and put it under `/usr/share/GeoIP/`

## Data store


## pentalog TODO:
* document
* package
* referrer in keys

## extract TODO:
* refererrer

## frontend TODO:
* config: merge & rewrite
* order by mostrecent peak
* navigable time


## JSON schemas

### public/data/index.json

```json
{ "/pentaradio/pentaradio-2013-08-27":
  { json: "EB2614777390DCE637C6CE53CFB5CCF1",
    peak: "2013-08-28",
	downloads: 1275.3 } }
```

### public/data/????????????????????????????????.json

```json
{ downloads: {
    "2013-08-28": 281.5 },
  "user-agents": {
  },
  "geo": {
    "2013-08-28": {
	  "DE": 200,
	  "AT": 30,
	  "CH": 25
	}
  }
}
```
