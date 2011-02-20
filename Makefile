mockface: Mockface.hs Gates.hs IDraw.hs
	ghc --make Mockface.hs -o mockface

clean:
	rm *.o *.hi mockface
