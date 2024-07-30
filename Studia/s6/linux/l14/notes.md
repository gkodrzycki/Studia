# $pingwinowanie \ 4^2 - 2$

## Zadanie 5
1. Stwórz plik np. example.py
```py=
def greet(name):
    return f"Hello, {name}!"

if __name__ == "__main__":
    print(greet("World"))
```
2. cp example.py example2.py
3. Wprowadź jakieś modyfikacje
```py=
def greet(name):
    return f"Hi, {name}!"

if __name__ == "__main__":
    print(greet("World"))
```
4. diff -u example.py example2.py > example.patch
5. Wprowadź następne zmiany w oryginalnym
```py=
def greet(name):
    return f"Hello, {name}!"

def bye(name):
    return f"Bye, {name}!"

if __name__ == "__main__":
    print(greet("World"))
    print(bye("World!))
```
6. patch example.py < example.patch
7. Bonjour fajrancik
