$address_low = $id_low
$address_high = $id_high

x = ($id_low & 511) - 256
y = (($id_low >>> 9) + ($id_high << 7)) - 128

w1 = @within(x, y, 0, 0, 50)
$data = w1 ? 0x000E : 0x0000

w2 = @within(x, y, 100, -100, 50)
$data += w2 ? 0x0E00 : 0x0000

w3 = @within(x, y, -100, 100, 50)
$data += w3 ? 0x00E0 : 0x0000

store!
