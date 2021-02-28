import argparse
import answer
import command

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='get filename')

    parser.add_argument('-e', help='file with element included')
    parser.add_argument('-c', help='file with command included')

    args = parser.parse_args()

    ios = answer.read_file(args.e)
    ios.toString()

    expressions = command.read_file(args.c)     # parse expressions

    for i in expressions:
        print(str(i)+':')
        expressions[i].toString()
        print()

    tree = command.makeTree()                   # make tree structure

    #var_list = command.parse(file_list)
    #result = command.classification(var_list)
    #command.print_file(result)
