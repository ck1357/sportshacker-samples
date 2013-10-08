import math

def default_activation(q1, q2):
    e1, e2 = math.exp(q1), math.exp(q2)
    return e1/(e1+e2)

def calc_survival_factors(q, fn=default_activation):
    rounds=int(math.log(len(q), 2))
    if rounds!=int(rounds):
        raise RuntimeError("N is not a factor of 2")
    n, sf = len(q), []
    def new_row(val):
        sf.append([val for i in range(n)])
    new_row(float(1))
    for rnd in range(rounds):
        new_row(float(0))
        groupsz=2**(rnd+1)
        quadsz, ngroups =(groupsz/2,
                          n/groupsz)                          
        for grp in range(ngroups):
            for i in range(groupsz):
                for j in range(groupsz):
                    iquad, jquad = (i/quadsz, 
                                    j/quadsz)
                    if iquad==jquad: # exit if in same quadrant
                        continue
                    x, y = (i+grp*groupsz, 
                            j+grp*groupsz)
                    sf[-1][x]+=sf[-2][x]*sf[-2][y]*fn(q[x], q[y])
    return sf
           
if __name__=="__main__":
    try:
        import random
        q=[random.gauss(0, 1) 
           for i in range(128)]
        sf=calc_survival_factors(q)
        for row in sf:
            print sum(row)
        print sf
    except RuntimeError, error:
        print "Error: %s" % str(error)
