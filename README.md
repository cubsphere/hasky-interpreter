# hasky-interpreter
se tiver stack, que facilitará para baixar o pacote Parsec:
```
stack setup && \
stack build
```
para rodar com stack:
```
stack exec hasky-interpreter-exe
```
se não tiver, então instala como quiser o Text.Parsec >= 3.1.13.0 e roda o `app/Main.hs`

# Como usar
entra com o programa.
exemplos:
```
Bem-vindo ao hasky-compiler


declare x=150 in declare y = 200 in { while x < 300 do { x:= x+1; y := y - 1};print y}
```
```
Bem-vindo ao hasky-compiler


declare x=150 in declare y = 200 in { if y = x then { x:= x+1; y := y - 1} else x:=2 ;print y}
```

```
Bem-vindo ao hasky-compiler


declare x=150 in declare y = 150 in { if y = x then { x:= x+1; y := y - 1} else x:=2 ;print y+x}
```

```
Bem-vindo ao hasky-compiler


declare x=150 in declare y = 200 in { if y = x then { x:= x+1; y := y - 1} else x:=2 ;print y+x}
```