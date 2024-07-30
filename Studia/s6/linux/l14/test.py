import sys

def suma(a: int, b: int):
    return a + b


print(f"First number: {sys.argv[2]}")
print(f"Second number: {sys.argv[2]}")
print(f"Sum: {suma(int(sys.argv[1]), int(sys.argv[2]))}")
