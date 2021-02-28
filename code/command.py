import re
import copy

CODE = 0
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
        if p_d.fullmatch(line):         # definition (ex: "e1: ")
            stage = expressionNum(line)
            eCurrent = Expression()
            expressions[expressionName(line)] = eCurrent

        elif stage == CODE:             # code sketch split
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

    def toString(self):
        if self.STR:
            return self.STR

        elif self.EXP:
            return self.EXP

        elif self.AND:
            total = ''
            for a in self.AND:
                total += (a.toString() + ' ')
            print(total)

        elif self.OR:
            for o in self.OR:
                print(o.toString())


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

        elif not code:
            code += token

        else:
            code += (' ' + token)

    if len(code) != 0:  # 그렇지 않은 경우 (ex: input[i])
        cursor.AND.append(Expression(expressionString=code))
        cursor.size += 1


def parseAdd(line, cursor):
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
    return int(line[1:-2])


def expressionName(line):
    return line[:-2]


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


def copyExpression(e):
    temp = copy.deepcopy(e)
    return temp

