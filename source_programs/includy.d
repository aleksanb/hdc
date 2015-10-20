__x       = __param0
__y       = __param1
__x_start = __param2
__x_end   = __param3
__y_start = __param4
__slope   = __param5

__dx = __x - __x_start
__y_end = __y_start + __dx * __slope

__return =
  __x > __x_start - 1 and __x < __x_end + 1
  and __y > __y_start + 1
  and __y < __y_end + 1
  and __y > __y_end - __slope ?
    0xffff : 0x0000
