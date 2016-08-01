from sys import argv
from glob import glob
from difflib import ndiff

if __name__ == "__main__":
    prefix = argv[1] if len(argv) > 1 else ''
    contents = []
    for file in glob('sim_results/' + prefix + '*'):
        with open(file, 'r') as f:
            contents.append(f.read())

    cases = list(set(contents))
    cases = zip(cases, cases[1:])
    for a,b in cases:
        additions = 0
        deletions = 0
        for i,s in enumerate(ndiff(a, b)):
            if s[0]==' ': continue
            elif s[0]=='-':
                #print u'Delete "{}" from position {}'.format(s[-1],i)
                additions += 1
            elif s[0]=='+':
                deletions += 1
                #print u'Add "{}" to position {}'.format(s[-1],i)
        print additions, 'additions and ', deletions, 'deletions away'
