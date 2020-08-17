
# ES Rally ggplot2 report

```
# get ES Rally results
curl -X GET http://localhost:9200/rally-races-\*/_search -H 'Content-type: application/json' -d '
{
  "query": {
    "match": { "user-tags.tournament": "aslfd-jkl239203rlafsdfj2093" }
  }
}' > races.json
# get an environment with the necessary R packages
nix-shell -p "rWrapper.override { packages = with rPackages; [ ggplot2 tidyr ggpubr ]; }"
# convert ES data into CSV
./es-to-csv.sh < races.json > data.csv
# generate plots
Rscript ./rnotebook.r
```
