import argparse
import answer
import command
import sys

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='get filename')

    parser.add_argument('-e', help='file with element included')
    parser.add_argument('-c', help='file with command included')

    args = parser.parse_args()    
    print(args)

    ios = answer.read_file(args.e)	# parse io samples
    ios.toString()   

    expressions = command.read_file(args.c)     # parse expressions

    for i in expressions:
        print(str(i)+':')
        print(expressions[i].toString())
        print()
        
    tree = command.deepcopyExpression(expressions['e0'], 0, 20) # make tree
    #print(tree.toString())

    code = command.generateNextDistinctCode(tree)
    while code != None:        
        result = answer.validateCode(ios, code)
        if result[1] == 0 and result[0] > 0:
            print("---- code #", command.distinctCodeCount, "----") 
            print(code)
            print()
        code = command.generateNextDistinctCode(tree)        
        
    print("A total of ", command.distinctCodeCount, " distinct codes explored")
      
    sys.exit(0)
    
    #var_list = command.parse(file_list)
    #result = command.classification(var_list)
    #command.print_file(result)
