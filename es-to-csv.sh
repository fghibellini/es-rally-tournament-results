cat races.json | jq -r '
  def object_flatten: [(paths(scalars) as $p | { key: ($p | join(".")), value: getpath($p) })] | from_entries;

  .hits.hits |
    map(._source | { track_name: .["user-tags"].track_name, results: .results | object_flatten | to_entries } | .track_name as $track_name | .results | map([$track_name, .key, .value])) | flatten(1) | .[] | select((.[2] | type)=="number") | @csv
'
