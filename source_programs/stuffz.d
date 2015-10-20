resX = 512
resY = 256

$address_low = $id_low
$address_high = $id_high

x = ($id_low & (resX - 1)) - (($constants[0] & 511) >> 3)
y = ($id_low >>> 9) + ($id_high << 7) - (($constants[0] & 255) >> 3)

$mask = (x * y) & ($constants[0] >> 4 & 127)
$data = 1 - $mask ? 0xFF00 : 0x0000

store!
