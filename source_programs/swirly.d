t = $constants[0]
y = ($id_low >>> 9) + ($id_high << 7)

rot = @sinus(2 * t + y * 3)
irotation = (t * 2 + rot * 3 + 1303) & 0x7FF

icosparams_x = @sinus(irotation + 5118)
icosparams_y = @sinus(irotation + 7164)
icosparams_z = @sinus(irotation + 9211)

params_x = (9 * icosparams_x) >> 7
params_y = (9 * icosparams_y) >> 7
params_z = (9 * icosparams_z) >> 7

colorparams_x = (3 * (1303 + icosparams_x)) >> 8
colorparams_y = (3 * (1303 + icosparams_y)) >> 8
colorparams_z = (3 * (1303 + icosparams_z)) >> 8

color = 0
altcolor = 0
coord_x = ($id_low & (511)) - 256
$mask = (coord_x < params_x) or (coord_x > params_z)
altcolor = $mask ? (2 << 5) : 0x26
$mask = $mask and coord_x < params_y
color = $mask ? colorparams_y : color
$mask = coord_x > params_x and coord_x < params_z
$mask = $mask and coord_x > params_y
color = $mask ? (31 - colorparams_y) : color
$mask = (coord_x < params_x) or (coord_x > params_z)
color = $mask ? 0 : color

$address_low = $id_low
$address_high = $id_high
$data = (color << 5) | altcolor
store!
