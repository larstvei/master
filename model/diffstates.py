from sys import argv, stdin
from re import findall, DOTALL

content = stdin.read()

# gather rule labels in a list
rules = findall(r'\,\'([\w-]+)', content)
# gather states in a list
states = [s.split('\n') for s in findall(r"\{(.*?)[-\w,']+\}", content, DOTALL)]

# iterate the states pairwise (along with the rule labels)
for r, (s, t) in zip(rules, zip(states, states[1:])):
    # gather the lines from s and t, that differ
    diff = [x for i, x in enumerate(t) if x is not '' and x != s[i]]
    print '[' + r + '] :' + '\n' + '\n'.join(diff) + '\n'
