# Comparison of slot <-> time conversion made by Ledger with SlotConfig and custom ones with node queries

```
Starting tip: Right (Tip {epoch = 1, hash = "fca4108ac2356c479ca21621bd4531a88760ac02e9a9e748301b62710865a5f1", slot = 100, block = 13, era = "Alonzo", syncProgress = "100.00"})
Converting POSIXTime {getPOSIXTime = 1652945917999} to slot:
- with node queries: Slot {getSlot = 105}
- with ledger stuff: Slot {getSlot = 105}
Converting Slot {getSlot = 108} to time:
- with node queries: POSIXTime {getPOSIXTime = 1652945920999}
- with ledger stuff: POSIXTime {getPOSIXTime = 1652945920999}
Current tip: Right (Tip {epoch = 1, hash = "50e92ee38c01745eb0862fd7e1938faab6d11f9b86405da948a385f950152547", slot = 108, block = 14, era = "Alonzo", syncProgress = "100.00"})
Ledger SlotRange: Interval {ivFrom = LowerBound (Finite (Slot {getSlot = 100})) False, ivTo = UpperBound (Finite (Slot {getSlot = 102})) True}
Query  SlotRange: Interval {ivFrom = LowerBound (Finite (Slot {getSlot = 100})) False, ivTo = UpperBound (Finite (Slot {getSlot = 102})) True}
```


## Querries result

```
Slot length: POSIXTime {getPOSIXTime = 1000}
Ledger SlotRange: Interval {ivFrom = LowerBound (Finite (Slot {getSlot = 148})) True, ivTo = UpperBound (Finite (Slot {getSlot = 158})) False}
Query  SlotRange: Interval {ivFrom = LowerBound (Finite (Slot {getSlot = 148})) True, ivTo = UpperBound (Finite (Slot {getSlot = 158})) True}

[\\\"Upper bounds not same\\\",\\\"Ranges not equal\\\",\\\"PT5\\\"]
```

## Ledger result

```
Interval {ivFrom = LowerBound (Finite (Slot {getSlot = 80})) True, ivTo = UpperBound (Finite (Slot {getSlot = 90})) False}


[\\\"End not in range\\\",\\\"Upper bounds not same\\\",\\\"Ranges not equal\\\",\\\"PT5\\\"]
```


Query  SlotRange: Interval {ivFrom = LowerBound (Finite (Slot {getSlot = 416})) False, ivTo = UpperBound (Finite (Slot {getSlot = 426})) False}
Query  SlotRange: Interval {ivFrom = LowerBound (Finite (Slot {getSlot = 480})) True, ivTo = UpperBound (Finite (Slot {getSlot = 490})) True}