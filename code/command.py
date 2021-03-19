import re
import copy
import sys

CODE = 0        # when stage == CODE, main expression e0 is being parsed
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

    line = fp.readline()

    while line:        
        # remove comments that begin with "//"
        comment_index = line.find('//')        
        if comment_index >= 0:
            line = line[:comment_index]
        line = line.strip() + '\n'
        
        #print("[" + line + "]")
        
        #if len(line) > 0:        
        if p_d.fullmatch(line):         # definition (ex: "e1: ")
            stage = expressionNum(line)
            eCurrent = Expression()
            expressions[expressionName(line)] = eCurrent

        elif stage == CODE:             # code sketch split for e0
            parseCode(line, eCurrent)

        elif line != '\n' and eCurrent == None: # e0 begins without "e0:"
            stage = CODE
            eCurrent = Expression()
            expressions['e0'] = eCurrent
            parseCode(line, eCurrent)

        elif line != '\n':              # ORED expressions split
            temp = Expression()
            parseAdd(line, temp)        # ANDED expressions split
            eCurrent.OR.append(temp)
            eCurrent.type = ORED
            eCurrent.size += 1

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
                total += (a.toString() + ' ')
            return total[:-1]       # return except the last ' '
            #print(total)

        elif self.OR:            
            total = ''
            for o in self.OR:                
                total += o.toString() + '\n'
                #print(o.toString())
            return total[:-1]       # return except the last '\n'

def parseCode(line, cursor):        # codeSketch 파싱하기. expression 파싱이랑 합쳐보기
    code = ''
    tokens = line.split()

    cursor.type = ANDED
    for token in tokens:        
        if p.fullmatch(token):  # recursive한 경우 (ex: 1 + e1)
            if len(code) != 0:
                cursor.AND.append(Expression(expressionString=code))
                cursor.size += 1

            cursor.AND.append(Expression(expressionId=token, expressionType=EXPRESSION))
            cursor.size += 1
            code = ''

        elif not code:          # code is empty
            code += token

        else:                   # code is not empty
            code += (' ' + token)

    if len(code) != 0:  # 그렇지 않은 경우 (ex: input[i])
        cursor.AND.append(Expression(expressionString=code))
        cursor.size += 1


def parseAdd(line, cursor):         # parse sub-expressions (those except e0)
    code = ''
    tokens = line.split()

    if len(tokens) == 1:
        if p.fullmatch(tokens[0]):
            cursor.type = EXPRESSION
            cursor.EXP = tokens[0]

        else:
            cursor.type = STRING
            cursor.STR = tokens[0]

    else:
        cursor.type = ANDED
        for token in tokens:
            if p.fullmatch(token):      # recursive한 경우 (ex: 1 + e1)
                if len(code) != 0:
                    cursor.AND.append(Expression(expressionString=code))
                    cursor.size += 1

                cursor.AND.append(Expression(expressionId=token, expressionType=EXPRESSION))
                cursor.size += 1
                code = ''

            elif not code:
                code += token

            else:
                code += (' ' + token)

        if len(code) != 0:              # 그렇지 않은 경우 (ex: input[i])
            cursor.AND.append(Expression(expressionString=code))
            cursor.size += 1


def expressionNum(line):        # eN일 경우 N return
    return int(line[1:-2])      # remove heading "e" and trailing ":\n"


def expressionName(line):
    return line[:-2]            # remove trailing ":\n"


def isExpression(token):
    if p.fullmatch(token):
        print()


def makeTree(depthMax=None):
    codeSketch = copyExpression(expressions['e0'])
    cursor = codeSketch

    for e in codeSketch.AND:
        if e.type == EXPRESSION:
            for i in expressions[e.EXP].OR:
                temp = copyExpression(i)
                e.OR.append(temp)

            e.type = ORED
            e.EXP = None

    return cursor


def copyExpression(e):          # create a shallow copy, where expression IDs remain as they are
    temp = copy.deepcopy(e)
    return temp


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
        temp = ""
        for esub in e.AND:
            temp += generate(esub) + ' '
        return temp
    elif e.type == ORED:
        #print(e.selection,"/",e.size)
        return generate(e.OR[e.selection])
    
def isSelected(e):  # return whether e and all of its parents are currently selected
    while e.parent != None:
        if e.parent.type == ORED and e.parent.OR[e.parent.selection] != e:
            return False
        e = e.parent

    return True
        
    
