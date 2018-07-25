pro convert_TAI93_to_utc, tai93, year, mon, day, utc

caldat, tai93/60./60./24. + julday(1,1,1993,0,0), mon, day, year, utc, mm, sec

utc  = utc + (mm+ sec/60.)/60.

end
