from sys import argv, stdin
from re import findall, DOTALL

res = stdin.read()

rules = findall(r'\,\'([\w-]+)', res)
states = [map(lambda x: x.strip(), s.split('\n')) \
          for s in findall(r'\{(.*?)\}', res, DOTALL)]

for r, (s, t) in zip(rules, zip(states, states[1:])):
    diff = [x for x in t if x not in s and x is not '']
    print '[' + r + '] :' + '\n\t' + '\n\t'.join(diff) + '\n'
