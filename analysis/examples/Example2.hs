data Bool = True | False;

f x = True;
main = let g k = if f 0 then k else (\y -> False)
       in  g f;