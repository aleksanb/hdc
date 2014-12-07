x = $id_high & 63
y = $id_low >> 6

edge_offset = $constants[10]
other_edge_offset = 64 - edge_offset

$mask |=
  x in [edge_offset, other_edge_offset]
  and edge_offset < y
  and y < other_edge_offset

$mask |=
  y in [edge_offset, other_edge_offset]
  and edge_offset < x
  and x < other_edge_offset

$mask |=
  x in [y, -y]

$address_low = $id_low
$address_high = $id_high

#$data = $mask ? 0xffff : 0xeeee #`rgb(0, 0, 255)`

store!
