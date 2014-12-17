resX = 512
resY = 256

$address_low = $id_low
$address_high = $id_high

x = ($id_low & (resX - 1)) - 256
y = ($id_low >>> 9) + ($id_high << 7) - 128

boxOne = 40
boxTwo = 120

$mask =
  x in [y, -y]

$mask |=
  x in [boxOne, -boxOne] and (y < boxOne) and (-boxOne < y)
  or y in [boxOne, -boxOne] and (x < boxOne) and (-boxOne < x)

$mask |=
  x in [boxTwo, -boxTwo] and (y < boxTwo) and (-boxTwo < y)
  or y in [boxTwo, -boxTwo] and (x < boxTwo) and (-boxTwo < x)

$data = $mask ? 0xE000 : 0x0000

store!
