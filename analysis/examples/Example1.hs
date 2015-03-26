
data Bool = True | False;

id x = x;

isZero n = case n of {
	0 -> True;
	_ -> False;
	};

twice f x = f (f x)

main = isZero (twice id 3);
