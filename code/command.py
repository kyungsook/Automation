import re
import copy
import sys

STRING = 1
EXPRESSION = 2
ANDED = 3
ORED = 4

expressions = dict()
p = re.compile('e[0-9]+')           # Regular Expression
p_d = re.compile('e[0-9]+:\n')      # Regular Expression Definition

def read_file(filename):
    fp = open(filename, "r")

    stage = -1
    eCurrent = None
    eCurrentOR = None
    eCurrentAND = None

    line = fp.readline()

    while line:        
        # remove comments that begin with "#"
        comment_index = line.find('#')        
        if comment_index >= 0:
            line = line[:comment_index] + '\n'
        line = line.rstrip() + '\n' # remove trailing whitespaces. heading whitespaces remain since indentation is important in python
        #line = line.strip() + '\n'                
        
        #print("[" + line + "]")
        
        #if len(line) > 0:        
        if p_d.fullmatch(line):         # definition (ex: "e1: ")
            stage = expressionNum(line)
            eCurrentOR = Expression()
            eCurrentOR.type = ORED
            eCurrentAND = Expression()
            eCurrentAND.type = ANDED
            eCurrentOR.OR.append(eCurrentAND)
            eCurrentOR.size += 1
            expressions[expressionName(line)] = eCurrentOR

        elif line != '\n':
            if eCurrentOR == None:  # e0 begins without "e0:"
                stage = 0
                eCurrentOR = Expression()
                eCurrentOR.type = ORED
                eCurrentAND = Expression()
                eCurrentAND.type = ANDED
                eCurrentOR.OR.append(eCurrentAND)
                eCurrentOR.size += 1
                expressions['e0'] = eCurrentOR

            if line.startswith('----'):
                eCurrentAND = Expression()
                eCurrentAND.type = ANDED
                eCurrentOR.OR.append(eCurrentAND)
                eCurrentOR.size += 1
                
            else:    
                parseCode(line, eCurrentAND)

        line = fp.readline()

    fp.close()
    return expressions


class Expression:
    def __init__(self, expressionString=None, expressionId=None, expressionType=STRING):
        self.type = expressionType              # string, exp, ANDED, ORED, 디폴트는 string
        self.size = 0                           # 몇 개가 있는지
        self.STR = expressionString
        self.EXP = expressionId
        self.AND = list()
        self.OR = list()
        self.selection = 0
        self.parent = None

    def toString(self):
        if self.STR:
            return self.STR

        elif self.EXP:
            return self.EXP

        elif self.AND:            
            total = ''
            for a in self.AND:
                temp = a.toString()
                total += temp
                if temp[-1] != '\n':
                    total += ' '
                #total += (a.toString() + ' ')
            return total[:-1]       # return except the last '\n'
            #print(total)

        elif self.OR:            
            total = ''
            for o in self.OR[:-1]:                
                total += o.toString() + '\n----\n'
                #print(o.toString())
            total += self.OR[-1].toString() + '\n'
            return total[:-1]       # return except the last '\n'


def parseCode(line, cursor):        # codeSketch 파싱하기. expression 파싱이랑 합쳐보기
    code = ''
    tokens = line.split(" ")

    cursor.type = ANDED
    for token in tokens:
        #print('[',token,']')
        token_s = token.strip() # last token in a line contains a trailing '\n', so remove it
        if p.fullmatch(token_s):  # recursive한 경우 (ex: 1 + e1)
            #print('[',token_s,']')
            if len(code) != 0:
                cursor.AND.append(Expression(expressionString=code))
                cursor.size += 1

            cursor.AND.append(Expression(expressionId=token_s, expressionType=EXPRESSION))
            cursor.size += 1
            if token[-1] == '\n':
                cursor.AND.append(Expression(expressionString='\n'))
            cursor.size += 1
            code = ''

        elif not code:          # code is empty
            if len(token) == 0: # empty token means a space
                code += ' ' # indentation is important for python
            else:
                code += token

        else:                   # code is not empty
            code += (' ' + token)

        #print(code)
        
    if len(code) != 0:  # 그렇지 않은 경우 (ex: input[i])
        cursor.AND.append(Expression(expressionString=code))
        cursor.size += 1


def expressionNum(line):        # eN일 경우 N return
    return int(line[1:-2])      # remove heading "e" and trailing ":\n"    


def expressionName(line):
    return line[:-2]            # remove trailing ":\n"    


selections = list()             # list of ORed expressions in tree, which will be used to generate distinct codes
def deepcopyExpression(e, depth, depthMax=None):      # create a deep copy, where expression IDs are replaced with expression objects    
    if depthMax != None and depth > depthMax:
        return None    
    elif e.type == STRING:
        return e
    elif e.type == EXPRESSION:
        return deepcopyExpression(expressions[e.EXP], depth, depthMax)
    elif e.type == ANDED:
        cursor = Expression()
        cursor.type = ANDED
        for esub in e.AND:
            temp = deepcopyExpression(esub, depth+1, depthMax)
            if (temp == None):
                return None     # if any one term is nullified in ANDed expression, then entire ANDed expression is nullified
            temp.parent = cursor
            cursor.AND.append(temp)
            cursor.size += 1
        return cursor
    elif e.type == ORED:
        cursor = Expression()
        cursor.type = ORED
        for esub in e.OR:
            temp = deepcopyExpression(esub, depth+1, depthMax)
            if (temp != None):  # if one term is nullified in ORed expression, then this term is trashed, while others remain
                temp.parent = cursor
                cursor.OR.append(temp)
                cursor.size += 1
        if cursor.size > 0:
            selections.append(cursor)            
            return cursor
        else:
            return None


distinctCodeCount = 0    
def generateNextDistinctCode(e):    # generate next distinct code by incrementing selections
    global distinctCodeCount
    if distinctCodeCount == 0:
        distinctCodeCount += 1
        return generate(e)

    for i in selections:
        if not isSelected(i):   # skip this ORed expression if it is not currently selected
            i.selection = 0
            continue
        i.selection += 1
        if i.selection >= i.size:
            i.selection = 0
            continue
        distinctCodeCount += 1
        return generate(e)

    return None     # no more code to explore


def generate(e):    # generate code according to current selections for ORed expressions
    if e.type == STRING:
        return e.STR
    elif e.type == ANDED:
        total = ""
        for esub in e.AND:
            temp = generate(esub)
            total += temp
            if temp[-1] != '\n':
                total += ' '            
        return total[:-1]   # return except the last '\n'
    elif e.type == ORED:
        #print(e.selection,"/",e.size)
        return generate(e.OR[e.selection])

    
def isSelected(e):  # return whether e and all of its parents are currently selected
    while e.parent != None:
        if e.parent.type == ORED and e.parent.OR[e.parent.selection] != e:
            return False
        e = e.parent

    return True
        
    
