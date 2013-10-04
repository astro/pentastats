## Data store

* By filename
* By day

## Visualization

* Section: group by show
* Graph: group by episode
  * Choose downloads, U-A, GeoIP
* Line: group by format

## pentalog TODO:
* full url
* referrer in keys

## extract TODO:
* refererrer

## frontend TODO:
* order by mostrecent peak
* group by ext, geoip, u-a


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
