from sys import argv, stdin
from re import findall, DOTALL

res = stdin.read()

rules = findall(r'\,\'([\w-]+)', res)
states = [s.split('\n') for s in findall(r"\{(.*?)[-\w,']+\}", res, DOTALL)]

for r, (s, t) in zip(rules, zip(states, states[1:])):
    diff = [x for i,x in enumerate(t) if x is not '' and x != s[i] or '< u' in x]
    print '[' + r + '] :' + '\n' + '\n'.join(diff) + '\n'
