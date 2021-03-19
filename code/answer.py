def read_file(filename):
    fp = open(filename, "r")
    answer = list()
    ios = IOSamples()
    index = 0
    line = fp.readline().rstrip('\n')       # 개행문자 제거

    while line:                             # 비어있는 줄에서 파싱 중단
        line = line.split(' ')              # 공백 기준으로 파싱

        if len(line) == 1 and type(line[0]) == str and line[0].lower() == '\\null':
            line = []                       # \null은 값이 없는 경우로 처리
        
        line = list(map(int, line))         # 문자열을 정수로 바꿈

        if index % 2 == 0:
            ios.setInput(line)

        else:
            ios.setOutput(line)

        index += 1
        line = fp.readline().rstrip('\n')   # 개행문자 제거

    return ios

class IOSamples:
    def __init__(self):
        self.input = list()
        self.output = list()
        self.pair = 0

    def setInput(self, num):
        self.input.append(num)
        self.pair += 1

    def setOutput(self, num):
        self.output.append(num)

    def toString(self):
        print('# of i/o pairs: %d' % self.pair)

        for i in range(self.pair):
            print('pair #%d, %d input: ' % (i + 1, len(self.input[i])), end='')
            for j in self.input[i]:
                print('%d ' % j, end='')
            print()

            print('pair #%d, %d output: ' % (i + 1, len(self.output[i])), end='')
            for j in self.output[i]:
                print('%d ' % j, end='')
            print()
        print()
