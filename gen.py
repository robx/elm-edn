print("module Unicode exposing (unicode)")
print()
print("unicode : Int -> Char")
print("unicode x =")
print("  let high = x // 0x100")
print("      low = x % 0x100")
print("  in case high of")
for high in range(0x100):
  print("  {} -> unicode{} x".format(hex(high), hex(high)))
print("  _ -> '\\xfffe'")

def one(high):
  print()
  print("unicode{} : Int -> Char".format(hex(high)))
  print("unicode{} x = case x of".format(hex(high)))
  for low in range(0x100):
    val = high*0x100 + low
    if val == 0x2028 or val == 0x2029:
      continue
    print("  {} -> '\\{}'".format(hex(val), hex(val)[1:]))
  print("  _ -> '\\xfffe'")

def none(high):
  print()
  print("unicode{} : Int -> Char".format(hex(high)))
  print("unicode{} x = case x of".format(hex(high)))
  print("  _ -> '\\xfffe'")

for high in range(0x100):
  if high >= 0xd8 and high < 0xe0:
    none(high)
  else:
    one(high)
