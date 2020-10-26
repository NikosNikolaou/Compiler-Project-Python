from collections import defaultdict
from enum import Enum, auto
from sys import exit as printError
import string
import os, sys, getopt


class InputError(Exception): pass;
class NumberFollowedByLetter(Exception): pass;
class CommentsBracketNeverClosed(Exception): pass;
class BackslashUseError(Exception): pass;
class NumberOutOfRange(Exception): pass;

class TokenType(Enum):
	Unknown = auto();
	Symbol = auto();
	
	ID = auto();
	Constant = auto();
	Comments = auto();
	
	Program = auto();
	Declare = auto();
	Enddeclare = auto();
	Procedure = auto();
	Function = auto();
	In = auto();
	Inout = auto();
	If = auto();
	Else = auto();
	While = auto();
	Select = auto();
	Default = auto();
	Do = auto();
	Exit = auto();
	Return = auto();
	Print = auto();
	Call = auto();
	Or = auto();
	And = auto();
	Not = auto();
	
	Assignment = auto();
	Colon = auto();
	Semicolon = auto();
	Comma = auto();
	BoardbracketOpen = auto();
	BoardbracketClose = auto();
	BracketOpen = auto();
	BracketClose = auto();
	ParenthesisOpen = auto();
	ParenthesisClose = auto();
	Equal = auto();
	Different = auto();
	Smaller = auto();
	SmallerEqual = auto();
	Larger = auto();
	LargerEqual = auto();
	Plus = auto();
	Minus = auto();
	Multiply = auto();
	Divide = auto();
	
	EOF = auto();

class Token:
	def __init__(self, type, data, line):
		self.type = type;
		self.data = data;
		self.line = line;

class Quad:
	def __init__(self, num, op, x, y, z):
		self.num = num;
		self.op = op;
		self.x = x;
		self.y = y;
		self.z = z;

class Scope:
	def __init__(self, level, framelength, entitiesList):
		self.level = level;
		self.framelength = framelength;
		self.entitiesList = entitiesList;

class Entity:
	def __init__(self, name, entitytype):
		self.name = name;
		self.entitytype = entitytype;

class EntityVar:
	def __init__(self, offset):
		self.offset = offset;

class EntityPar:
	def __init__(self, parmode, offset):
		self.parmode = parmode;
		self.offset = offset;

class Argument:
	def __init__(self, parmode):
		self.parmode = parmode;

class EntityFunc:
	def __init__(self, functype, startquad, arguments, framelength):
		self.functype = functype;
		self.arguments = arguments;
		self.startquad = startquad;
		self.framelength = framelength;



#   PUBLIC DECLARATIONS - START   #
reserved =	[
					"program",
					"declare",
					"enddeclare",
					"procedure",
					"function",
					"call",
					"in",
					"inout",
					"exit",
					"return",
					"if",
					"else",
					"do",
					"while",
					"select",
					"default",
					"not",
					"and",
					"or",
					"print"
				];
line = 1;
chars = -1;
tokens = list();
usedTokens = list();
quads = list();
nextquadNumber = 1;
quadsNextTempNum = 1;
exitJumpPendingList = [];
symbolBoard = [];
finalCode = [];
#   PUBLIC DECLARATIONS - END   #



def nextToken():
	return tokens[-1];
	
def popToken():
	usedTokens.append(tokens[-1]);
	return tokens.pop();
	
def latestUsedToken(pos=-2):
	return usedTokens[pos];
	
def checkStrForValidInteger(str):
    try: 
        int(str);
        return True;
    except ValueError:
        return False;
		
def listToStr(list, joinSymbol=""):
	return joinSymbol.join(list);

def nextquad():
	return nextquadNumber;

def genquad(op, x, y ,z):
	global nextquadNumber;
	quad = Quad(nextquadNumber, str(op), str(x), str(y), str(z));
	nextquadNumber += 1;
	quads.append(quad);

def newtemp():
	global quadsNextTempNum;
	tempVar = "T_" + str(quadsNextTempNum);
	quadsNextTempNum += 1;
	symbolBoard[-1].entitiesList.append(Entity(tempVar, EntityVar(symbolBoard[-1].framelength)));
	symbolBoard[-1].framelength += 4;
	return tempVar;

def emptylist():
	return [];

def makelist(x):
	return [x];

def makequadlist(quadnum, op="_", x="_", y="_", z="_"):
	quad = Quad(quadnum, str(op), str(x), str(y), str(z));
	return makelist(quad);

def mergelist(list1, list2):
	return list1 + list2;

def backpatch(list, z):
	for i in list:
		quads[i-1].z = str(z);

def quadToStr(quad):
	return str(quad.num)+': ' + quad.op + ', ' + quad.x + ', ' + quad.y + ', ' + quad.z;

def newScope():
	level = len(symbolBoard)+1;
	framelength = 12;
	entitiesList = [];
	return Scope(level, framelength, entitiesList);

def addNewVarOnSymbolTable(varToken):
	varName = varToken.data;
	if (varExistsOnCurrentSymbolTableLevel(varName)==False):
		symbolBoard[-1].entitiesList.append(Entity(varName, EntityVar(symbolBoard[-1].framelength)));
		symbolBoard[-1].framelength += 4;
	else:
		printError("Grammh: %d --> H metavlhth '%s' pou dhlwnetai, exei hdh dhlw8ei prohgoumenws sto idio scope level!" %(varToken.line, varName));

def addNewParOnSymbolTable(varToken, varTypeToken):
	varName = varToken.data;
	varParmode = varTypeToken.type.name;
	if (varExistsOnCurrentSymbolTableLevel(varName)==False):
		symbolBoard[-1].entitiesList.append(Entity(varName, EntityPar(varParmode, symbolBoard[-1].framelength)));
		symbolBoard[-1].framelength += 4;
	else:
		printError("Grammh: %d --> H metavlhth '%s' pou dhlwnetai, exei hdh dhlw8ei prohgoumenws sto idio scope level!" %(varToken.line, varName));

def varExistsOnCurrentSymbolTableLevel(varName):
	for entity in symbolBoard[-1].entitiesList:
		if (isinstance(entity.entitytype, EntityVar) or isinstance(entity.entitytype, EntityPar)):
			if (entity.name == varName):
				return True;
	return False;

def addNewFuncOnSymbolTable(funcTypeToken, funcIdToken):
	funcName = funcIdToken.data;
	funcType = funcTypeToken.type.name;
	if (funcExistsOnCurrentSymbolTableLevel(funcName, funcType)==False):
		SQ = 'None';
		ARGS = [];
		FL = 0;
		symbolBoard[-1].entitiesList.append(Entity(funcName, EntityFunc(funcType, SQ, ARGS, FL)));
		symbolBoard.append(newScope());
	else:
		printError("Grammh: %d --> H <%s> %s pou dhlwnetai, exei hdh dhlw8ei prohgoumenws sto idio scope level!" %(funcIdToken.line, funcType, funcName));

def funcExistsOnCurrentSymbolTableLevel(funcName, funcType):
	for entity in symbolBoard[-1].entitiesList:
		if (isinstance(entity.entitytype, EntityFunc)):
			if (entity.name == funcName and entity.entitytype.functype == funcType):
				return True;
	return False;

def addCalleeArgumentOnPreviousSymbolTableLevel(argumentParmode):
	symbolBoard[-2].entitiesList[-1].entitytype.arguments.append(Argument(argumentParmode));

def varExistsOnSymbolTable(varName):
	for scope in symbolBoard[::-1]:
		for entity in scope.entitiesList:
			if (isinstance(entity.entitytype, EntityVar) or isinstance(entity.entitytype, EntityPar)):
				if (entity.name == varName):
					return True;
	return False;

def funcExistsOnSymbolTable(funcName, funcType):
	for scope in symbolBoard[::-1]:
		for entity in scope.entitiesList:
			if (isinstance(entity.entitytype, EntityFunc)):
				if (entity.name == funcName and entity.entitytype.functype == funcType):
					if (entity.entitytype.startquad != None and  entity.entitytype.framelength != 0):
						return True;
					else:
						if (scope.level+1 == currentScope()):
							return True;
						else:
							return False;
	return False;

def getFuncArgumentsParmodeMatchOnSymbolTable(funcName, funcType):
	argumentsList = None;
	for scope in symbolBoard[::-1]:
		for entity in scope.entitiesList:
			if (isinstance(entity.entitytype, EntityFunc)):
				if (entity.name == funcName and entity.entitytype.functype == funcType):
					argumentsList = [arg.parmode for arg in entity.entitytype.arguments];
					break;
		if (argumentsList != None):
			break;
	return argumentsList;

def findVariableInSymbolTable(varName):
	for scope in symbolBoard[::-1]:
		for entity in scope.entitiesList:
			if (isinstance(entity.entitytype, EntityVar)):
				if (entity.name == varName):
					return ("Var", scope.level, str(entity.entitytype.offset));
			if (isinstance(entity.entitytype, EntityPar)):
				if (entity.name == varName):
					return (entity.entitytype.parmode, scope.level, str(entity.entitytype.offset));

def findFuncInSymbolTable(funcName, funcType):
	for scope in symbolBoard[::-1]:
		for entity in scope.entitiesList:
			if (isinstance(entity.entitytype, EntityFunc)):
				if (entity.name == funcName and entity.entitytype.functype == funcType):
					return (scope.level+1, str(entity.entitytype.startquad), str(entity.entitytype.framelength));


def currentScope():
	return symbolBoard[-1].level;

def currentScopeFL():
	return str(symbolBoard[-1].framelength);

def printSymbolBoard():
	for scope in symbolBoard:
		print("ScopeLevel:%d --> " %(scope.level), end='');
		for entity in scope.entitiesList:
			if (isinstance(entity.entitytype, EntityVar)):
				entityVar = entity.entitytype;
				print("Var:%s, Offset:%d---" %(entity.name, entityVar.offset), end='');
			elif (isinstance(entity.entitytype, EntityFunc)):
				entityFunc = entity.entitytype;
				args = "";
				for arg in entity.entitytype.arguments:
					args += arg.parmode + ", ";
				print("Name:%s, Type:%s, SQ:%s, Args:%sFL:%d---" %(entity.name, entityFunc.functype, entityFunc.startquad, args, entityFunc.framelength), end='');
			elif (isinstance(entity.entitytype, EntityPar)):
				entityPar = entity.entitytype;
				print("Par:%s, Type:%s, Offset:%d---" %(entity.name, entityPar.parmode, entityPar.offset), end='');
		print();

def generateLatestCompiledFuncFinalCodeAndRemoveFromSymbolTable(startquad):
	finalCode.append("sw $ra, ($sp)");
	generateFinalCodeFromStartquad(startquad);
	finalCode.append("lw $ra, ($sp)");
	finalCode.append("jr $ra");
	
	symbolBoard.pop();

def generateMainFinalCode(startquad):
	finalCode.append("main:");
	finalCode.append("add $sp, $sp, "+currentScopeFL());
	finalCode.append("move $s0, $sp");
	
	generateFinalCodeFromStartquad(startquad);

	finalCode.insert(0, "");
	finalCode.insert(0, "j main");

def generateFinalCodeFromStartquad(startquad):
	pars = 0;
	finalCodeMissingAddFpSpFlCommandIndex = None;
	parRetFound = False;

	for i in range(startquad-1, len(quads)):
		quad = quads[i];
		if (finalCode[-1]=="sw $ra, ($sp)"):
			finalCode.insert(len(finalCode)-1, "L"+str(quad.num)+":");
		else:
			finalCode.append("L"+str(quad.num)+":");
		
		if (quad.op=="begin_block"):
			finalCode.append("#"+quad.x+"-->BEGIN");
		elif (quad.op=="end_block"):
			finalCode.append("#"+quad.x+"-->END");
		elif (quad.op=="jump"):
			finalCode.append("j L"+quad.z);
		elif (quad.op=="out"):
			loadvr(quad.x, 1)
			finalCode.append("move $a0, $t1");
			finalCode.append("li $v0, 1");
			finalCode.append("syscall");
			### Print an extra new line! ###
			finalCode.append("li $a0, 0xA  # 0xA ascii code for LF | 0xD ascii code for CR");
			finalCode.append("li $v0, 11   # syscall 11 prints the lower 8 bits of $a0 as an ascii character");
			finalCode.append("syscall");
			###############################
		elif (quad.op=="halt"):
			finalCode.append("li $v0, 10");
			finalCode.append("syscall");
		elif (quad.op=="retv"):
			loadvr(quad.x, 1);
			finalCode.append("lw $t0, -8($sp)");
			finalCode.append("sw $t1, ($t0)");
		elif (quad.op=="par"):
			if (pars==0):
				finalCodeMissingAddFpSpFlCommandIndex = len(finalCode);

			if (quad.y=="cv"):
				loadvr(quad.x, 1);
				finalCode.append("sw $t1, -"+str(12+4*pars)+"($fp)");
			elif (quad.y=="ref"):
				(variableType, variableScope, variableOffset) = findVariableInSymbolTable(quad.x);
				if (variableScope==currentScope()):
					if (variableType=="Var" or variableType=="In"):
						finalCode.append("add $t0, $sp, -"+variableOffset);
						finalCode.append("sw $t0, -"+str(12+4*pars)+"($fp)");
					else:	#Inout
						finalCode.append("lw $t0, -"+variableOffset+"($sp)");
						finalCode.append("sw $t0, -"+str(12+4*pars)+"($fp)");
				else:
					if (variableType=="Var" or variableType=="In"):
						gnlvcode(quad.x);
						finalCode.append("sw $t0, -"+str(12+4*pars)+"($fp)");
					else:	#Inout
						gnlvcode(quad.x);
						finalCode.append("lw $t0, ($t0)");
						finalCode.append("sw $t0, -"+str(12+4*pars)+"($fp)");
			else:	#ret
				(variableType, variableScope, variableOffset) = findVariableInSymbolTable(quad.x);
				finalCode.append("add $t0, $sp, -"+variableOffset);
				finalCode.append("sw $t0, -8($fp)");
				parRetFound = True;

			pars+=1;
		elif (quad.op=="call"):
			if (parRetFound):
				funcType = "Function";
			else:
				funcType = "Procedure";
			(funcScope, funcStartquad, funcFL) = findFuncInSymbolTable(quad.x, funcType);
			if (finalCodeMissingAddFpSpFlCommandIndex!=None):
				finalCode.insert(finalCodeMissingAddFpSpFlCommandIndex, "add $fp, $sp, "+funcFL);
			else:
				finalCode.append("add $fp, $sp, "+funcFL);
			
			if (funcScope==currentScope()):
				finalCode.append("lw $t0, -4($sp)");
				finalCode.append("sw $t0, -4($fp)");
			else:
				finalCode.append("sw $sp, -4($fp)");

			finalCode.append("add $sp, $sp, "+funcFL);
			finalCode.append("jal L"+funcStartquad);
			finalCode.append("add $sp, $sp, -"+funcFL);

			pars = 0;
			parRetFound = False;
		elif (quad.op==":="):
			loadvr(quad.x, 1);
			storerv(1, quad.z);
		elif (quad.op=="+" or quad.op=="-" or quad.op=="*" or quad.op=="/"):
			loadvr(quad.x, 1);
			loadvr(quad.y, 2);
			if (quad.op=="+"):
				finalCode.append("add $t1, $t1, $t2");
			elif (quad.op=="-"):
				finalCode.append("sub $t1, $t1, $t2");
			elif (quad.op=="*"):
				finalCode.append("mul $t1, $t1, $t2");
			elif (quad.op=="/"):
				finalCode.append("div $t1, $t1, $t2");
			storerv(1, quad.z);
		elif (quad.op in ["=", "<>", "<", ">", "<=", ">="]):
			loadvr(quad.x, 1);
			loadvr(quad.y, 2);
			if (quad.op=="="):
				finalCode.append("beq $t1, $t2, L"+quad.z);
			elif (quad.op=="<>"):
				finalCode.append("bne $t1, $t2, L"+quad.z);
			elif (quad.op=="<"):
				finalCode.append("blt $t1, $t2, L"+quad.z);
			elif (quad.op==">"):
				finalCode.append("bgt $t1, $t2, L"+quad.z);
			elif (quad.op=="<="):
				finalCode.append("ble $t1, $t2, L"+quad.z);
			elif (quad.op==">="):
				finalCode.append("bge $t1, $t2, L"+quad.z);

def loadvr(variable, register):
	register = str(register);
	if (checkStrForValidInteger(variable)):
		finalCode.append("li $t"+register+", "+variable);
	else:
		(variableType, variableScope, variableOffset) = findVariableInSymbolTable(variable);
		if (variableScope==1):
			finalCode.append("lw $t"+register+", -"+variableOffset+"($s0)");
		elif (variableScope==currentScope()):
			if (variableType=="Var" or variableType=="In"):
				finalCode.append("lw $t"+register+", -"+variableOffset+"($sp)");
			else:	#Inout
				finalCode.append("lw $t0, -"+variableOffset+"($sp)");
				finalCode.append("lw $t"+register+", ($t0)");
		else:
			if (variableType=="Var" or variableType=="In"):
				gnlvcode(variable);
				finalCode.append("lw $t"+register+", ($t0)");
			else:	#Inout
				gnlvcode(variable);
				finalCode.append("lw $t0, ($t0)");
				finalCode.append("lw $t"+register+", ($t0)");

def storerv(register, variable):
	register = str(register);
	(variableType, variableScope, variableOffset) = findVariableInSymbolTable(variable);
	if (variableScope==1):
		finalCode.append("sw $t"+register+", -"+variableOffset+"($s0)");
	elif (variableScope==currentScope()):
		if (variableType=="Var" or variableType=="In"):
			finalCode.append("sw $t"+register+", -"+variableOffset+"($sp)");
		else:	#Inout
			finalCode.append("lw $t0, -"+variableOffset+"($sp)");
			finalCode.append("sw $t"+register+", ($t0)");
	else:
		if (variableType=="Var" or variableType=="In"):
			gnlvcode(variable);
			finalCode.append("sw $t"+register+", ($t0)");
		else:	#Inout
			gnlvcode(variable);
			finalCode.append("lw $t0, ($t0)");
			finalCode.append("sw $t"+register+", ($t0)");

def gnlvcode(variable):
	(variableType, variableScope, variableOffset) = findVariableInSymbolTable(variable);
	finalCode.append("lw $t0, -4($sp)");
	for i in range(currentScope()-1-variableScope):
		finalCode.append("lw $t0, -4($t0)");
	finalCode.append("sub $t0, $t0, "+variableOffset);

def lektikosAnaluths(text):
	global line;
	global chars;
	
	typesDict = {':=':"Assignment", ':':"Colon", ';':"Semicolon", ',':"Comma", '[':"BoardbracketOpen", ']':"BoardbracketClose", '{':"BracketOpen", '}':"BracketClose", '(':"ParenthesisOpen", ')':"ParenthesisClose", '=':"Equal", '<>':"Different", '<':"Smaller", '<=':"SmallerEqual", '>':"Larger", '>=':"LargerEqual", '+':"Plus", '-':"Minus", '*':"Multiply", '/':"Divide"};
	typesDict = defaultdict(lambda: 'Unknown', typesDict);
	
	symbols = '>' + '<' + ':' + ';' + '=' + '+' + '-' + '*' + '/' + '\\' + ',' + '(' + ')' + '{' + '}' + '[' + ']';
	acceptables = string.ascii_letters + string.digits + string.whitespace + symbols;
	

	word = list();
	type = None;
	numSign = None;

	if (chars+1 >= len(text)):
		word = "EOF";
		type = TokenType.EOF;
		return Token(type, word, line);
	
	try:
		for i in range(chars+1, len(text)):
			if(type == None):
				if (text[i] not in acceptables):
					word.append(text[i]);
					type = TokenType.Unknown;
					raise InputError;
				else:
					if (text[i] in string.whitespace):
						if (text[i] == '\n'):
							line += 1;

						if ((i == len(text)-1) and (len(word) == 0)):
							chars = i;
							word = "EOF";
							type = TokenType.EOF;
							return Token(type, word, line);
						else:
							continue;
					elif (text[i] in string.ascii_letters):
						type = TokenType.ID;
					elif (text[i] in string.digits):
						type = TokenType.Constant;
						
						for k in range (i, -1, -1):
							if (text[k] in string.whitespace or text[k] in string.digits):
								if (k != 0):
									continue;
								else:
									numSign = '+';
									break;
								
							if (text[k] != '-'):
								numSign = '+';
								break;
							else:
								numSign = '-';
								break;
					else:
						type = TokenType.Symbol;
						
					word.append(text[i]);
			else:
				if (type == TokenType.ID or type == TokenType.Constant):
					if ((text[i] in string.ascii_letters) or (text[i] in string.digits)):
						word.append(text[i]);
					else:
						if (type == TokenType.Constant):
							if (checkStrForValidInteger(listToStr(word)) == False):
								raise NumberFollowedByLetter;
						i-=1;
						break;
				elif (type == TokenType.Symbol):
					if (word[-1] == '>'):
						if (text[i] == '='):
							word.append(text[i]);
							break;
					elif (word[-1] == '<'):
						if ((text[i] == '=') or (text[i] == '>')):
							word.append(text[i]);
							break;
					elif (word[-1] == ':'):
						if (text[i] == '='):
							word.append(text[i]);
							break;
					elif (word[-1] == '\\'):
						if (text[i] == '*'):
							word.append(text[i]);
							type = TokenType.Comments;
							continue;
						else:
							i-=1;
							raise BackslashUseError;
					i-=1;
					break;
				elif (type == TokenType.Comments):
					word.append(text[i]);
					if (word[-2:]==['*', '\\'] and len(word)>=4):		# \**\ --> sxolia dioti arxizei me \* kai teleiwnei me *\ kai to mhkos einai >=4 gia na mhn skaei sthn periptwsh: \*\
						break;
					elif (i == len(text)-1):
						raise CommentsBracketNeverClosed;
					else:
						continue;
		
		
		word_joined = listToStr(word);
		
		if (type == TokenType.ID):
			if (len(word) > 30):
				print("Warning: H metavlhth '%s' pou vre8hke stin grammh %d exei poly megalo mhkos (length = %d)\n\tH glwssa Ciscal dexetai metavlhtes me mhkos to poly 30 xarakthres!\n\t8a xrhsimopoih8oun mono ta 30 prwta grammata kai h metavlhth 8a metatrapei se: '%s'" %(word_joined, line, len(word), word_joined[:30]));
				word_joined = word_joined[:30];
			if (word_joined in reserved):
				type = TokenType[word_joined.title()];
		elif (type == TokenType.Constant):
			num = int(numSign + word_joined);
			if (num > 0):
				if (num > 32767):
					raise NumberOutOfRange;
			else:
				if (num < -32768):
					raise NumberOutOfRange;
		elif (type == TokenType.Symbol):
			type = TokenType[typesDict[word_joined]];
		
		chars = i;
		return Token(type, word_joined, line);
	
	except InputError:
		printError("Sfalma: H glwssa Ciscal den perilambanei to sumbolo '%s' pou vre8hke stin grammh: %d" %(listToStr(word), line));
	except NumberFollowedByLetter:
		printError("Sfalma: Yparxei suntaktiko la8os ston ari8mo '%s' pou vre8hke stin grammh: %d\n\tO ari8mos paremvaletai apo 1 (h perissotera) grammata!" %(listToStr(word), line));
	except CommentsBracketNeverClosed:
		printError("Sfalma: Ta sxolia pou 3ekinane (me to sumvolo '\\*') sthn grammh %d den kleinoun pote!\n\tHint: Xrhsimopoihste thn akolou8h sunta3h gia na sumperilavetai sxolia ston kwdika: \\* ... *\\" %(line));
	except BackslashUseError:
		printError("Sfalma: To backslash('\\') prepei na akolou8eitai panta apo '*' kai xrhsimopoieitai gia na anoi3ei ena block me sxolia! (Grammh: %d)\n\tHint: Xrhsimopoihste thn akolou8h sunta3h gia na sumperilavetai sxolia ston kwdika: \\* ... *\\" %(line))
	except NumberOutOfRange:
		printError("Sfalma: O ari8mos '%d' pou vre8hke sthn grammh %d einai ektos twn apodektwn oriwn ths glwssas Ciscal!\n\tH glwssa dexetai apokleistika ari8mous sto diasthma: [-32768, 32767]" %(int(numSign + listToStr(word)), line));



def suntaktikosAnaluths():
	symbolBoard.append(newScope());
	program();
	# printSymbolBoard();
	# print();

def program():
	programToken = popToken();
	if (programToken.type == TokenType.Program):
	
		programNameToken = popToken();
		if (programNameToken.type == TokenType.ID):
			block(programToken, programNameToken);
		else:
			printError("Grammh: %d --> Meta th le3h 'program' vre8hke to '%s' enw anamenetai to ID tou programmatos!" %(programNameToken.line, programNameToken.data));
	else:
		printError("Grammh: %d --> Vre8hke to '%s' enw anamenotan na uparxei h le3h 'program' h opoia shmatodotei thn enar3h tou programmatos!" %(programToken.line, programToken.data));

def block(blockTypeToken, blockNameToken):
	bracketopenToken = popToken();
	if (bracketopenToken.type == TokenType.BracketOpen):
		declarations(blockTypeToken);
		subprograms();

		startquad = nextquad();
		genquad("begin_block", blockNameToken.data, "_", "_");
		
		sequense(blockTypeToken);

		token = popToken();
		if (token.type == TokenType.BracketClose):
			genquad("end_block", blockNameToken.data, "_", "_");

			if (blockTypeToken.type == TokenType.Function or blockTypeToken.type == TokenType.Procedure):
				if (blockTypeToken.type == TokenType.Function):
					returnFound = checkIfFunctionReturnsValue();
					if (returnFound==False):
						printError("Sfalma: To block ths sunarthshs '%s' pou 3ekina sthn grammh %d den perilamvanei kanena 'return'.\n\tHint: Ka8e sunarthsh prepei na epistrefei toulaxistwn 1 timh xrhsimopoiwntas thn entolh: 'return(value)'" %(blockNameToken.data, blockTypeToken.line));

				symbolBoard[-2].entitiesList[-1].entitytype.startquad = startquad;
				symbolBoard[-2].entitiesList[-1].entitytype.framelength = symbolBoard[-1].framelength;
				generateLatestCompiledFuncFinalCodeAndRemoveFromSymbolTable(startquad);
			else:
				genquad("halt", "_", "_", "_");
				generateMainFinalCode(startquad);
		elif (token.type == TokenType.Program):
			printError("Grammh: %d --> H desmeumenh le3h 'program' den mporei na xrhsimopoih8ei se auto to shmeio tou programmatos.\n\tHint: H lektikh monada 'program' xrhsimopoieitai ws prwth le3h sthn arxh tou programatos kai shmatodotei thn dhmiourgia enos programatos-block." %(token.line));
		elif (token.type == TokenType.Declare or token.type == TokenType.Enddeclare):
			printError("Grammh: %d --> H desmeumenh le3h '%s' den mporei na xrhsimopoih8ei se auto to shmeio tou programmatos.\n\tHint: Oi lektikes monades 'declare' & 'enddeclare' xrhsimopoiountai diadoxika (declare ...metavlhtes... enddeclare) sthn arxh tou block tou kuriou programmatos 'H entos mias sunarthshs(h diadikasias) gia na dhlw8oun oi metavlhtes pou 8a xrhsimopoih8oun entos tou ekastote block." %(token.line, token.data));
		elif (token.type == TokenType.Procedure or token.type == TokenType.Function):
			printError("Grammh: %d --> H desmeumenh le3h '%s' den mporei na xrhsimopoih8ei se auto to shmeio tou programmatos.\n\tHint: Oi lektikes monades 'procedure' & 'function' xrhsimopoiountai sthn arxh tou block (meta ta declaration -an auta uparxoun) tou kuriou programmatos (h emfwleumena mesa se allh function h procedure) gia na orisoun thn dhlwsh sunarthshs h diadikasias antistoixa." %(token.line, token.data));
		elif (token.type == TokenType.In or token.type == TokenType.Inout):
			printError("Grammh: %d --> H desmeumenh le3h '%s' den mporei na xrhsimopoih8ei se auto to shmeio tou programmatos.\n\tHint: Oi lektikes monades 'in' & 'inout' xrhsimopoiountai meta th dhlwsh h thn klhsh sunarthsewn kai diadikasiwn gia na oristoun oi tupoi twn metavlhtwn-parametrwn pou autes dexontai h pou tous stelnontai kata thn klhsh tous antistoixa." %(token.line, token.data));
		elif (token.type == TokenType.Else):
			printError("Grammh: %d --> H desmeumenh le3h 'else' den mporei na xrhsimopoih8ei se auto to shmeio tou programmatos.\n\tHint: To 'else' mporei na xrhsimopoih8ei mono amesws meta to telos enos <If-Statement>." %(token.line));
		elif (token.type == TokenType.Default):
			printError("Grammh: %d --> H desmeumenh le3h 'default' den mporei na xrhsimopoih8ei se auto to shmeio tou programmatos.\n\tHint: To 'default' xrhsimopoieitai apokleistika kai aparaithta (ws 'default:') meta apo to telos tou block ka8e <Select-Statement>." %(token.line));
		elif (token.type == TokenType.Not):
			printError("Grammh: %d --> H desmeumenh le3h 'not' den mporei na xrhsimopoih8ei se auto to shmeio tou programmatos.\n\tHint: To 'not' mporei na xrhsimopoih8ei mono prin apo ena condition ws e3hs: not[ <CONDITION> ]" %(token.line));
		elif (token.type == TokenType.Or or token.type == TokenType.And):
			printError("Grammh: %d --> H desmeumenh le3h '%s' den mporei na xrhsimopoih8ei se auto to shmeio tou programmatos.\n\tHint: Oi logikoi telestes 'or' & 'and' mporoun na xrhsimopoih8oun mono anamesa se logikes ekfraseis." %(token.line, token.data));
		else:
			printError("Grammh: %d --> Vre8hke to '%s' enw anamenetai to kleisimo tou block pou 3ekina sthn grammh %d me th xrhsh agkulhs '}'" %(token.line, token.data, bracketopenToken.line));
	else:
		printError("Grammh: %d --> Vre8hke to '%s' enw anamenetai h dhmiourgia enos block me to anoigma agkulhs '{'" %(bracketopenToken.line, bracketopenToken.data));

def checkIfFunctionReturnsValue():
	functionQuads = [];
	for i in range (len(quads)-1, -1, -1):
		if (quads[i].op != "begin_block"):
			functionQuads.append(quads[i]);
		else:
			break;
	functionQuads.reverse();

	for funcQuad in functionQuads:
		if (funcQuad.op == "retv"):
			return True;
	return False;

def declarations(blockTypeToken):
	declareToken = nextToken();
	if (declareToken.type == TokenType.Declare):
		popToken();
		varlist(blockTypeToken);
		
		token = popToken();
		if (token.type == TokenType.Enddeclare):
			pass;
		else:
			printError("Grammh: %d --> Vre8hke to '%s' enw anamenetai h le3h 'enddeclare' h opoia 8a shmatodothsei to kleisimo tou declaration block pou 3ekina sthn grammh %d me th le3h 'declare'" %(token.line, token.data, declareToken.line));
	else:
		pass;

def subprograms():
	while (True):
		token = nextToken();
		if (token.type == TokenType.Procedure or token.type == TokenType.Function):
			func();
		else:
			break;

def sequense(blockTypeToken):
	latestUsedStatement = statement(blockTypeToken);
	
	while (True):
		token = nextToken();
		if (token.type == TokenType.Semicolon):
			popToken();
			
			latestUsedStatement = statement(blockTypeToken);
		else:
			if (
					token.type == TokenType.ID or
					token.type == TokenType.If or
					token.type == TokenType.Do or
					token.type == TokenType.While or
					token.type == TokenType.Select or
					token.type == TokenType.Exit or
					token.type == TokenType.Return or
					token.type == TokenType.Print or
					token.type == TokenType.Call
				):
				printError("Grammh: %d --> Prin to '%s' kai katopin ths oloklhrwshs tou <%s-Statement> pou 3ekina sthn grammh %d, leipei ';'" %(token.line, token.data, latestUsedStatement.type.name, latestUsedStatement.line));
			else:
				break;

def varlist(blockTypeToken):
	token = nextToken();
	if (token.type == TokenType.ID):
		popToken();

		addNewVarOnSymbolTable(token);
		
		while (True):
			token = nextToken();
			if (token.type == TokenType.Comma):
				popToken();
				
				token = popToken();
				if (token.type == TokenType.ID):
					addNewVarOnSymbolTable(token);
				else:
					printError("Grammh: %d --> Meta to ',' vre8hke '%s' enw anamenetai h dhlwsh tou onomatos (to ID) mias metavlhths!" %(token.line, token.data));
			else:
				break;
	else:
		pass;
	
def func():
	funcTypeToken = popToken();
	
	funcIdToken = popToken();
	if (funcIdToken.type == TokenType.ID):
		addNewFuncOnSymbolTable(funcTypeToken, funcIdToken);

		funcbody(funcTypeToken, funcIdToken);
	else:
		printError("Grammh: %d --> Vre8hke to '%s', enw h <%s> pou dhlwnete stin grammh %d prepei na akolou8eitai apo to ID ths!" %(funcIdToken.line, funcIdToken.data, funcTypeToken.type.name, funcTypeToken.line));
	
def funcbody(funcTypeToken, funcIdToken):
	formalpars(funcTypeToken, funcIdToken);
	block(funcTypeToken, funcIdToken);
	
def formalpars(funcTypeToken, funcIdToken):
	token = popToken();
	if (token.type ==  TokenType.ParenthesisOpen):
	
		token = nextToken();
		if (token.type == TokenType.ParenthesisClose):
			popToken();
		else:
			formalparlist(funcTypeToken, funcIdToken);
			
			token = popToken();
			if (token.type == TokenType.ParenthesisClose):
				pass;
			else:
				printError("Grammh: %d --> Vre8hke to '%s' enw anamenetai to kleisimo tou block dhlwsewn parametrwn ths <%s %s> me th xrhsh parentheshs ')'" %(token.line, token.data, funcTypeToken.type.name, funcIdToken.data));
	else:
		printError("Grammh: %d --> Vre8hke to '%s' enw anamenetai to anoigma paren8eshs '(' gia na dhlw8oun oi parametroi ths <%s %s>" %(token.line, token.data, funcTypeToken.type.name, funcIdToken.data));
	
def formalparlist(funcTypeToken, funcIdToken):
	formalparitem(funcTypeToken, funcIdToken);
	
	while (True):
		token = nextToken();
		if (token.type == TokenType.Comma):
			popToken();
			
			formalparitem(funcTypeToken, funcIdToken);
		else:
			break;
	
def formalparitem(funcTypeToken, funcIdToken):
	funcIdTypeToken = popToken();
	if (funcIdTypeToken.type == TokenType.In or funcIdTypeToken.type == TokenType.Inout):
		addCalleeArgumentOnPreviousSymbolTableLevel(funcIdTypeToken.type.name);
	
		token = popToken();
		if (token.type == TokenType.ID):
			addNewParOnSymbolTable(token, funcIdTypeToken);
		else:
			printError("Grammh: %d --> Vre8hke to '%s' enw anamenetai to onoma mias parametrou ths <%s %s> h opoia einai tupou: <%s>" %(token.line, token.data, funcTypeToken.type.name, funcIdToken.data, funcIdTypeToken.type.name));
	else:
		printError("Grammh: %d --> Meta to '%s' vre8hke '%s' enw anamenetai h dhlwsh tou tupou mias metavlhths ( in | inout ) ths sunarthshs <%s %s>" %(funcIdTypeToken.line, latestUsedToken().data, funcIdTypeToken.data, funcTypeToken.type.name, funcIdToken.data));
	
def statement(blockTypeToken):
	token = nextToken();
	
	if (token.type == TokenType.ID):
		assignmentstat();
		token.type = TokenType.Assignment;
	elif (token.type == TokenType.If):
		ifstat(blockTypeToken);
	elif (token.type == TokenType.Do):
		dowhilestat(blockTypeToken);
	elif (token.type == TokenType.While):
		whilestat(blockTypeToken);
	elif (token.type == TokenType.Select):
		selectstat(blockTypeToken);
	elif (token.type == TokenType.Exit):
		exitstat(blockTypeToken);
	elif (token.type == TokenType.Return):
		returnstat(blockTypeToken);
	elif (token.type == TokenType.Print):
		printstat();
	elif (token.type == TokenType.Call):
		callstat();
	else:
		token = None;

	latestUsedStatementToken = token;
	return latestUsedStatementToken;
	
def assignmentstat():
	idToken = popToken();
	
	token = popToken();
	if (token.type == TokenType.Assignment):
		if (varExistsOnSymbolTable(idToken.data)==False):
			printError("Grammh: %d --> H metavlhth '%s' sthn opoia ginetai ekxwrhsh, den exei dhlw8ei oute sto trexwn alla oute se goniko block!"%(idToken.line, idToken.data));

		ePlace = expression();

		genquad(":=", ePlace, "_", idToken.data);
	else:
		printError("Grammh: %d --> Meta to onoma metavlhths '%s' vre8hke to '%s' enw anamenetai to sumvolo ekxwrhshs ':='" %(token.line, idToken.data, token.data));
	
def expression():
	optionalsignToken = optionalsign();
	t1Place = term();

	if (optionalsignToken != None):
		if (optionalsignToken.type == TokenType.Minus):
			w = newtemp();
			genquad("-", "0", t1Place, w);
			t1Place = w;
	
	while (True):
		token = nextToken();
		if (token.type == TokenType.Plus or token.type == TokenType.Minus):
			addoperToken = addoper();
			t2Place = term();

			w = newtemp();
			genquad(addoperToken.data, t1Place, t2Place, w);
			t1Place = w;
		else:
			return t1Place;

def optionalsign():
	token = nextToken();
	if (token.type == TokenType.Plus or token.type == TokenType.Minus):
		addoperToken = addoper();
		return addoperToken;
	else:
		return None;
	
def term():
	f1Place = factor();
	
	while (True):
		token = nextToken();
		if (token.type == TokenType.Multiply or token.type == TokenType.Divide):
			muloperToken = muloper();
			f2Place = factor();

			w = newtemp();
			genquad(muloperToken.data, f1Place, f2Place, w);
			f1Place = w;
		else:
			return f1Place;
	
def factor():
	token = popToken();
	if (token.type == TokenType.Constant):
		fPlace = token.data;
		return fPlace;
	elif (token.type == TokenType.ParenthesisOpen):
		fPlace = expression();
		
		parcloseToken = popToken();
		if (parcloseToken.type == TokenType.ParenthesisClose):
			return fPlace;
		else:
			printError("Grammh: %d --> Meta thn ekfrash pou 3ekinaei me '(' sth grammh %d vre8hke to '%s' enw anamenetai to kleisimo parentheshs ')'" %(parcloseToken.line, token.line, parcloseToken.data));
	elif (token.type == TokenType.ID):
		fPlace = idtail(token);
		return fPlace;
	else:
		printError("Grammh: %d --> Meta to '%s' vre8hke to '%s' enw anamenetai ena apo ta akolou8a: ari8mos | metavlhth | (<Expression>) | klhsh sunarthshs" %(token.line, latestUsedToken().data, token.data));
	
def idtail(idtailToken):
	token = nextToken();
	if (token.type == TokenType.ParenthesisOpen):
		if (funcExistsOnSymbolTable(idtailToken.data, "Function")==False):
			printError("Grammh: %d --> H sunarthsh '%s' pou kaleitai, den exei dhlw8ei oute sto trexwn alla oute se emfwleumeno scope level!"%(idtailToken.line, idtailToken.data));
		actualpars(idtailToken, "Function");

		w = newtemp();
		genquad("par", w, "ret", "_");
		genquad("call", idtailToken.data, "_", "_");
		return w;
	else:
		if (varExistsOnSymbolTable(idtailToken.data)==False):
			printError("Grammh: %d --> H metavlhth '%s', den exei dhlw8ei oute sto trexwn alla oute se goniko block!"%(idtailToken.line, idtailToken.data));
		return idtailToken.data;
	
def actualpars(funcIdToken, funcType):
	parToken = popToken();
	if (parToken.type ==  TokenType.ParenthesisOpen):
	
		token = nextToken();
		if (token.type == TokenType.ParenthesisClose):
			popToken();

			parmodeList = ["-"];
			argumentsList = getFuncArgumentsParmodeMatchOnSymbolTable(funcIdToken.data, funcType);
			if (argumentsList != []):
				printError("Grammh: %d --> O typos twn parametrwn pou stelnontai sthn <%s> '%s' diaferei apo ta anamenomena!\n\tVre8hke: %s | Anamenotan: %s" %(funcIdToken.line, funcType, funcIdToken.data, listToStr(parmodeList, ", "), listToStr(argumentsList, ", ")));
		else:
			actualparlist(funcIdToken, funcType);
			
			token = popToken();
			if (token.type == TokenType.ParenthesisClose):
				pass;
			else:
				printError("Grammh: %d --> Meta thn klhsh ths '%s' kai katopin anoigmatos '(' kai perasma parametrwn sth grammh %d, vre8hke to '%s' enw anamenetai to kleisimo parentheshs ')' H xrhsh tou ',' gia pros8hkh kai allwn parametrwn." %(token.line, funcIdToken.data, parToken.line, token.data));
	else:
		printError("Grammh: %d --> Meta thn klhsh ths diadikasias '%s' vre8hke to '%s' enw anamenetai to anoigma parentheshs '(' gia to perasma parametrwn se auth!" %(parToken.line, funcIdToken.data, parToken.data));
	
def actualparlist(funcIdToken, funcType):
	parList = actualparitem(funcIdToken);
	
	while (True):
		token = nextToken();
		if (token.type == TokenType.Comma):
			popToken();
			
			newparList = actualparitem(funcIdToken);
			parList = mergelist(parList, newparList);
		else:
			parmodeList = [];
			for q in parList:
				genquad(q.op, q.x, q.y, q.z);
				if (q.y == "cv"):
					parmodeList.append("In");
				elif (q.y == "ref"):
					parmodeList.append("Inout");
			argumentsList = getFuncArgumentsParmodeMatchOnSymbolTable(funcIdToken.data, funcType);
			if (argumentsList == []):
				argumentsList = ["-"];
			if (parmodeList == []):
				parmodeList = ["-"];
			if (argumentsList != parmodeList):
				printError("Grammh: %d --> O typos twn parametrwn pou stelnontai sthn <%s> '%s' diaferei apo ta anamenomena!\n\tVre8hke: %s | Anamenotan: %s" %(funcIdToken.line, funcType, funcIdToken.data, listToStr(parmodeList, ", "), listToStr(argumentsList, ", ")));
			break;
	
def actualparitem(funcIdToken):
	idtypetoken = popToken();
	if (idtypetoken.type == TokenType.In):
		ePlace = expression();
		return makequadlist(nextquad(), "par", ePlace, "cv", "_");
	elif (idtypetoken.type == TokenType.Inout):
	
		token = popToken();
		if (token.type == TokenType.ID):
			if (varExistsOnSymbolTable(token.data)==False):
				printError("Grammh: %d --> H parametros <%s> '%s' h opoia stelnetai ws orisma, den exei dhlw8ei oute sto trexwn alla oute se goniko block!"%(token.line, idtypetoken.type.name, token.data));
		
			return makequadlist(nextquad(), "par", token.data, "ref", "_");
		else:
			printError("Grammh: %d --> Vre8hke to '%s' enw anamenetai to onoma mias parametrou h opoia einai tupou: <%s>" %(token.line, token.data, idtypetoken.type.name));
	else:
		printError("Grammh: %d --> Meta to '%s' vre8hke '%s' enw anamenetai o typos ths metavlhths pou 8a stalei sth sunarthsh '%s' kai einai enas apo tous akolou8ous: in | inout" %(idtypetoken.line, latestUsedToken().data, idtypetoken.data, funcIdToken.data));
	
def muloper():
	token = popToken();
	if (token.type == TokenType.Multiply or token.type == TokenType.Divide):
		return token;
	else:
		printError("Grammh: %d --> Vre8hke to '%s' enw anamenetai ena apo ta sumvola [ * | / ]" %(token.line, token.data));
	
def addoper():
	token = popToken();
	if (token.type == TokenType.Plus or token.type == TokenType.Minus):
		return token;
	else:
		printError("Grammh: %d --> Vre8hke to '%s' enw anamenetai ena apo ta sumvola [ + | - ]" %(token.line, token.data));
	
def ifstat(blockTypeToken):
	ifToken = popToken();
	
	token = popToken();
	if (token.type == TokenType.ParenthesisOpen):
		bBoolDict = condition();
		backpatch(bBoolDict["True"], nextquad());
		
		token = popToken();
		if (token.type == TokenType.ParenthesisClose):
			brackorstat(blockTypeToken, ifToken);
			ifList = makelist(nextquad());
			genquad("jump", "_", "_", "_");
			backpatch(bBoolDict["False"], nextquad());

			elsepart(blockTypeToken);
			backpatch(ifList, nextquad());
		else:
			printError("Grammh: %d --> Meta to 'if' sth grammh %d kai katopin anoigmatos paren8eshs kai dhlwsh sun8hkhs vre8hke to '%s' enw anamenetai to kleisimo parentheshs ')'" %(token.line, ifToken.line, token.data));
	else:
		printError("Grammh: %d --> Meta to 'if' sth grammh %d vre8hke to '%s' enw anamenetai to anoigma parentheshs '(' kai h ekfrash mias sun8hkhs!" %(token.line, ifToken.line, token.data));
	
def condition():
	bBoolDict = {};
	q1BoolDict = boolterm();

	bBoolDict = q1BoolDict;
	
	while (True):
		token = nextToken();
		if (token.type == TokenType.Or):
			popToken();
			
			backpatch(bBoolDict["False"], nextquad());
			q2BoolDict = boolterm();
			bBoolDict["True"] = mergelist(bBoolDict["True"], q2BoolDict["True"]);
			bBoolDict["False"] = q2BoolDict["False"];
		else:
			return bBoolDict;
	
def boolterm():
	qBoolDict = {};
	r1BoolDict = boolfactor();

	qBoolDict = r1BoolDict;
	
	while (True):
		token = nextToken();
		if (token.type == TokenType.And):
			popToken();
			
			backpatch(qBoolDict["True"], nextquad());
			r2BoolDict = boolfactor();
			qBoolDict["False"] = mergelist(qBoolDict["False"], r2BoolDict["False"]);
			qBoolDict["True"] = r2BoolDict["True"];
		else:
			return qBoolDict;
	
def boolfactor():
	token = nextToken();
	if (token.type == TokenType.Not):
		popToken();
		
		brackopnToken = popToken();
		if (brackopnToken.type == TokenType.BoardbracketOpen):
			bBoolDict = condition();
			
			brackclsToken = popToken();
			if (brackclsToken.type == TokenType.BoardbracketClose):
				notbBoolDict = {"True": bBoolDict["False"], "False": bBoolDict["True"]};
				return notbBoolDict;
			else:
				printError("Grammh: %d --> Meta th le3h 'not' sthn grammh %d kai katopin '[' akolou8oumeno apo sun8hkh vre8hke to '%s' enw anamenetai to kleisimo bracket ']'" %(brackclsToken.line, token.line, brackclsToken.data));
		else:
			printError("Grammh: %d --> Meta th le3h 'not' sthn grammh %d vre8hke to '%s' enw anamenetai to anoigma bracket '['" %(brackopnToken.line, token.line, brackopnToken.data));
	elif (token.type == TokenType.BoardbracketOpen):
		popToken();
		bBoolDict = condition();
			
		brackclsToken = popToken();
		if (brackclsToken.type == TokenType.BoardbracketClose):
			return bBoolDict;
		else:
			printError("Grammh: %d --> Epeita apo to anoigma bracket '[' sth grammh %d akolou8oumeno apo sun8hkh vre8hke to '%s' enw anamenetai to kleisimo bracket ']'" %(brackclsToken.line, token.line, brackclsToken.data));
	else:
		e1Place = expression();
		relopToken = relationaloper();
		e2Place = expression();

		rTrue = makelist(nextquad());
		genquad(relopToken.data, e1Place, e2Place, "_");
		rFalse = makelist(nextquad());
		genquad("jump", "_", "_", "_");

		return {"True": rTrue, "False": rFalse};
	
def relationaloper():
	token = popToken();
	if (
			token.type == TokenType.Equal or
			token.type == TokenType.Smaller or
			token.type == TokenType.SmallerEqual or
			token.type == TokenType.Different or
			token.type == TokenType.LargerEqual or
			token.type == TokenType.Larger
		):
		return token;
	else:
		printError("Grammh: %d --> Epeita apo sun8hkh ekfrashs vre8hke to '%s' anamenetai enas apo tous akolou8ous telestes: = | < | <= | <> | >= | > " %(token.line, token.data));
	
def brackorstat(blockTypeToken, reservedCallerToken):
	token = nextToken();
	if (token.type == TokenType.BracketOpen):
		bracketsseq(blockTypeToken, reservedCallerToken);
	else:
		statement(blockTypeToken);
		
		token = popToken();
		if (token.type == TokenType.Semicolon):
			pass;
		else:
			printError("Grammh: %d --> Meta to '%s' sth grammh %d kai katopin ths ekteleshs 0 h perissoterwn statements vre8hke to '%s' enw anamenetai ';'" %(token.line, reservedCallerToken.data, reservedCallerToken.line, token.data));
	
def bracketsseq(blockTypeToken, reservedCallerToken):
	token = popToken();
	if (token.type == TokenType.BracketOpen):
		sequense(blockTypeToken);
		
		token = popToken();
		if (token.type == TokenType.BracketClose):
			pass;
		else:
			printError("Grammh: %d --> Meta to '%s' sth grammh %d kai katopin anoigmatos agkulhs kai xrhsh 0 h perissoterwn statements vre8hke to '%s' enw anamenetai to kleisimo agkulhs '}'" %(token.line, reservedCallerToken.data, reservedCallerToken.line, token.data));
	else:
		printError("Grammh: %d --> Meta to '%s' sth grammh %d vre8hke to '%s' enw anamenetai h dhmiourgia block me to anoigma agkulhs '{'" %(token.line, reservedCallerToken.data, reservedCallerToken.line, token.data));
	
def elsepart(blockTypeToken):
	elseToken = nextToken();
	if (elseToken.type == TokenType.Else):
		popToken();
		
		brackorstat(blockTypeToken, elseToken);
	else:
		pass;
	
def dowhilestat(blockTypeToken):
	global exitJumpPendingList;

	doToken = popToken();
	blockTypeToken = doToken;

	squad = nextquad();
	brackorstat(blockTypeToken, doToken);
	
	whileToken = popToken();
	if (whileToken.type == TokenType.While):
	
		token = popToken();
		if (token.type == TokenType.ParenthesisOpen):
			bBoolDict = condition();
			
			token = popToken();
			if (token.type == TokenType.ParenthesisClose):
				backpatch(bBoolDict["True"], squad);
				backpatch(bBoolDict["False"], nextquad());
				backpatch(exitJumpPendingList, nextquad());
				exitJumpPendingList[:] = [];
			else:
				printError("Grammh: %d --> To do-while statement pou 3ekinhse sth grammh %d me to 'do' oloklhrwnetai me to 'while' sth grammh %d. Meta to while kai katopin to anoigma paren8eshs '(' kai thn dhlwsh ths sun8hkhs vre8hke to '%s' enw anamenetai to kleisimo parentheshs ')'" %(token.line, doToken.line, whileToken.line, token.data));
		else:
			printError("Grammh: %d --> To do-while statement pou 3ekinhse sth grammh %d me to 'do' oloklhrwnetai me to 'while' sth grammh %d. Meta to while vre8hke to '%s' enw anamenetai to anoigma parentheshs '('" %(token.line, doToken.line, whileToken.line, token.data));
	else:
		printError("Grammh: %d --> Sto telos tou 'do' statement pou 3ekinhse sth grammh %d vre8hke to '%s' enw anamenetai h desmeumenh le3h 'while'" %(whileToken.line, doToken.line, whileToken.data));
	
def whilestat(blockTypeToken):
	whileToken = popToken();
	
	token = popToken();
	bquad = nextquad();
	if (token.type == TokenType.ParenthesisOpen):
		bBoolDict = condition();
		
		token = popToken();
		if (token.type == TokenType.ParenthesisClose):
			backpatch(bBoolDict["True"], nextquad());

			brackorstat(blockTypeToken, whileToken);

			genquad("jump", "_", "_", bquad);
			backpatch(bBoolDict["False"], nextquad());
		else:
			printError("Grammh: %d --> Meta to 'while' sth grammh %d kai efoson exei prohgh8ei '(' kai sun8hkh vre8hke to '%s' enw anamenetai to kleisimo parentheshs ')'" %(token.line, whileToken.line, token.data));
	else:
		printError("Grammh: %d --> Meta to 'while' sth grammh %d vre8hke to '%s' enw anamenetai to anoigma parentheshs '('" %(token.line, whileToken.line, token.data));
		
def selectstat(blockTypeToken):
	selToken = popToken();
	
	token = popToken();
	if (token.type == TokenType.ParenthesisOpen):
	
		selID = None;
		token = popToken();
		if (token.type == TokenType.ID):
			selID = token.data;
		else:
			printError("Grammh: %d --> H paren8esh pou anoigei gia to select statement pou 3ekina sth grammh %d prepei na perikleiei mia metavlhth, enw vre8hke to '%s'" %(token.line, selToken.line, token.data));
		
		token = popToken();
		if (token.type == TokenType.ParenthesisClose):
			if (varExistsOnSymbolTable(selID)==False):
				printError("Grammh: %d --> H metavlhth '%s' h opoia xrhsimopoieitai sto select-statement, den exei dhlw8ei oute sto trexwn alla oute se goniko block!"%(token.line, selID));
		else:
			printError("Grammh: %d --> H paren8esh pou anoigei gia to select-statement pou 3ekina sth grammh %d den kleinei pote. Vre8hke to '%s' enw anamenetai ')'" %(token.line, selToken.line, token.data));
		

		selFalse = emptylist();
		selDone = emptylist();
		counter = 1;
		while (True):
			constToken = nextToken();
			if (constToken.type == TokenType.Constant):
				popToken();
				colToken = popToken();
				if (colToken.type == TokenType.Colon):
					if (int(constToken.data) == counter):
						counter += 1;
						
						selTrue = makelist(nextquad());
						backpatch(selFalse, nextquad());
						genquad("=", selID, constToken.data, "_");

						selFalse = makelist(nextquad());
						genquad("jump", "_", "_", "_");

						backpatch(selTrue, nextquad());
						selConstToken = Token(constToken.type, constToken.data + ":", constToken.line)
						brackorstat(blockTypeToken, selConstToken);

						selDone = mergelist(selDone, makelist(nextquad()));
						genquad("jump", "_", "_", "_");
					else:
						printError("Grammh: %d --> H sta8era '%s:' entos tou select statement pou 3ekina sth grammh %d 8a eprepe na exei thn timh '%s:'" %(constToken.line, constToken.data, selToken.line, counter));
				else:
					printError("Grammh: %d --> H sta8era '%s' pou vrisketai mesa sto select statement (pou 3ekina sthn grammh %d) prepei na akolou8eitai apo ':'" %(constToken.line, constToken.data, selToken.line));
			else:
				backpatch(selFalse, nextquad());
				break;
		
		defToken = popToken();
		colToken = popToken();
		if (defToken.type == TokenType.Default and colToken.type == TokenType.Colon):
			selDefToken = Token(defToken.type, defToken.data + ":", defToken.line);
			brackorstat(blockTypeToken, selDefToken);

			backpatch(selDone, nextquad());
		else:
			printError("Grammh: %d --> To select statement pou 3ekinaei sth grammh %d prepei aparaithta na oloklhrwnetai me th le3h 'default:', enw vre8hke '%s'" %(defToken.line, selToken.line, defToken.data));
	else:
		pprintError("Grammh: %d --> Meta to select statement sth grammh %d vre8hke to '%s' enw anamenetai to anoigma parentheshs '('" %(token.line, selToken.line, token.data));
	
def exitstat(blockTypeToken):
	global exitJumpPendingList;
	
	exitToken = popToken();
	if (blockTypeToken.type != TokenType.Do):
		printError("Grammh: %d --> Vre8hke 'exit' mesa se <%s>-Block to opoio 3ekinhse sth grammh %d.\n\tHint: To 'exit' mporei na xrhsimopoih8ei mono mesa se do-while statement block." %(exitToken.line, blockTypeToken.type.name, blockTypeToken.line));
	
	exitJumpPendingList = mergelist(exitJumpPendingList, makelist(nextquad()));
	genquad("jump", "_", "_", "_");
	
def returnstat(blockTypeToken):
	retToken = popToken();
	if (blockTypeToken.type != TokenType.Function):
		printError("Grammh: %d --> Vre8hke 'return' mesa se <%s>-Block to opoio 3ekinhse sth grammh %d.\n\tHint: To 'return' mporei na xrhsimopoih8ei mono mesa se block sunarthsewn (Functions)" %(retToken.line, blockTypeToken.type.name, blockTypeToken.line));
	
	token = popToken();
	if (token.type == TokenType.ParenthesisOpen):
		ePlace = expression();
		
		token = popToken();
		if (token.type == TokenType.ParenthesisClose):
			genquad("retv", ePlace, "_", "_");
		else:
			printError("Grammh: %d --> Meta to 'return' sth grammh %d kai efoson exei prohgh8ei '(' kai sun8hkh vre8hke to '%s' enw anamenetai to kleisimo parentheshs ')'" %(token.line, retToken.line, token.data));
	else:
		printError("Grammh: %d --> Meta to 'return' sth grammh %d vre8hke to '%s' enw anamenetai to anoigma parentheshs '('" %(token.line, retToken.line, token.data));
	
def printstat():
	printToken = popToken();
	
	token = popToken();
	if (token.type == TokenType.ParenthesisOpen):
		ePlace = expression();
		
		token = popToken();
		if (token.type == TokenType.ParenthesisClose):
			genquad("out", ePlace, "_", "_");
		else:
			printError("Grammh: %d --> Meta to 'return' sth grammh %d kai efoson exei prohgh8ei '(' kai sun8hkh vre8hke to '%s' enw anamenetai to kleisimo parentheshs ')'" %(token.line, printToken.line, token.data));
	else:
		printError("Grammh: %d --> Meta to 'print' sth grammh %d vre8hke to '%s' enw anamenetai to anoigma parentheshs '('" %(token.line, printToken.line, token.data));
	
def callstat():
	callToken = popToken();
	
	token = popToken();
	if (token.type == TokenType.ID):
		if (funcExistsOnSymbolTable(token.data, "Procedure")==False):
			printError("Grammh: %d --> H diadikasia '%s' pou kaleitai, den exei dhlw8ei oute sto trexwn alla oute se emfwleumeno scope level!" %(token.line, token.data));
		actualpars(token, "Procedure");

		genquad("call", token.data, "_", "_");
	else:
		printError("Grammh: %d --> Meta th le3h 'call' (grammh %d) vre8hke to '%s' enw anamenotan to onoma mias diadikasias!" %(token.line, callToken.line, token.data));
	

def writeIntermediateCodeToFile(inputfile_noExtension):
	intermediateOutputFile = inputfile_noExtension + ".int";
	try:
		with open(intermediateOutputFile, 'w') as f:
			for x in quads:
				f.write(quadToStr(x) + "\n");
	except IOError as e:
		printError("%s: %s!" %(intermediateOutputFile, e.strerror));
	except:
		printError("Error creating output file: %s" %(sys.exc_info()[1]));

def writeAssemblyFinalCodeToFile(inputfile_noExtension):
	assemblyOutputFile = inputfile_noExtension + ".asm";
	try:
		with open(assemblyOutputFile, 'w') as f:
			for x in finalCode:
				if (x.find(":")==-1):
					f.write("\t"+ x + "\n");
				else:
					f.write(x + "\n");
	except IOError as e:
		printError("%s: %s!" %(assemblyOutputFile, e.strerror));
	except:
		printError("Error creating output file: %s" %(sys.exc_info()[1]));

def writeExecutableFileForC(inputfile_noExtension):
	funcOrProcFound = checkForFunctionsOrProceduresInQuads();
	if (not funcOrProcFound):
		lines = translateQuadsIntoExecutableLinesOfC();

		outputfile_c = inputfile_noExtension + ".c";
		try:
			with open(outputfile_c, 'w') as f:
				for line in lines:
					f.write(line + "\n");
		except IOError as e:
			printError("%s: %s!" %(outputfile_c, e.strerror));
		except:
			printError("Error creating output file: %s" %(sys.exc_info()[1]));

		return True;
	else:
		return False;

def checkForFunctionsOrProceduresInQuads():
	totalBlocks = 0;
	for quad in quads:
		if (quad.op == "begin_block"):
			totalBlocks += 1;
	
	return (totalBlocks>1);

def translateQuadsIntoExecutableLinesOfC():
	programDeclarations = [];
	for quad in quads:
		if ((quad.z!='_') and (checkStrForValidInteger(quad.z)==False) and (quad.z not in programDeclarations)):
			programDeclarations.append(quad.z);
	
	lines = [];
	lines.append("#include <stdio.h>\n");
	for quad in quads:
		if (quad.op == "begin_block"):
			lines.append("int main() {" +"\t//"+quadToStr(quad));
			vars = "int";
			for var in programDeclarations:
				vars += " " + var + ",";
			lines.append(vars[:len(vars)-1] + ";");
			lines.append("L"+str(quad.num)+": ;");
		elif (quad.op == "halt"):
			lines.append("L"+str(quad.num)+": return 0;" +"\t//"+quadToStr(quad));
		elif (quad.op == "end_block"):
			lines.append("L"+str(quad.num)+": ;\n}" +"\t//"+quadToStr(quad));
		elif (quad.op == ':='):
			lines.append("L"+str(quad.num)+": " + quad.z + " = " + quad.x + ";" +"\t//"+quadToStr(quad));
		elif (quad.op in ('+', '-', '/', '*')):
			lines.append("L"+str(quad.num)+": " + quad.z + " = " + quad.x + quad.op + quad.y + ";" +"\t//"+quadToStr(quad));
		elif (quad.op in ('<', '>', '=', '<>', '>=', '<=')):
			lines.append("L"+str(quad.num)+": if (" + quad.x + quad.op + quad.y + ") goto L" + quad.z + ";" +"\t//"+quadToStr(quad));
		elif (quad.op == "jump"):
			lines.append("L"+str(quad.num)+": goto L" +  quad.z + ";" +"\t//"+quadToStr(quad));
		elif (quad.op == "out"):
			lines.append("L"+str(quad.num)+": printf(\"%d\\n\", " + quad.x + ");" +"\t//"+quadToStr(quad));

	return lines;

def printUsageAndExit():
	printError("Use: ciscal_compiler.py -i <inputfile>");

def main(argv):
	global tokens;
	
	inputfile = None;
	text = None;
	
	try:
		opts, args = getopt.getopt(argv, "i:", ["input="]);
		if (not opts):
			printUsageAndExit();
	except getopt.GetoptError:
		printUsageAndExit();
	
	for opt, arg in opts:
		if opt in ("-i", "--input"):
			inputfile = arg;
		else:
			printUsageAndExit();
	
	try:
		with open(inputfile, 'r') as f:
			text = f.read();
	except IOError as e:
		printError("%s: %s!" %(inputfile, e.strerror));
	except:
		printError("Error opening input file: %s" %(sys.exc_info()[1]));

	while (True):
		token = lektikosAnaluths(text);
		#print("%s --> %s" %(token.type.name, token.data));		# token.[.type {.type.name | .type.value} | .data | .line]
		
		if (token.type != TokenType.Unknown and token.type != TokenType.Comments):
			tokens.append(token);
		if (token.type == TokenType.EOF):
			break;
	
	tokens.reverse();
	suntaktikosAnaluths();

	inputfile_noExtension = None;
	if (inputfile.rfind(".") == -1):
		inputfile_noExtension = inputfile;
	else:
		inputfile_noExtension = inputfile[:inputfile.rfind(".")];
	writeIntermediateCodeToFile(inputfile_noExtension);
	executableCreated = writeExecutableFileForC(inputfile_noExtension);
	writeAssemblyFinalCodeToFile(inputfile_noExtension);

	print("Ciscal: File '%s' successfully compiled!" %(inputfile));
	print(">>> O endiamesos kwdikas vrisketai sto arxeio: '"+inputfile_noExtension+".int'");
	print(">>> O telikos ektelesimos kwdikas se assembly (arxitektonikhs MIPS) vrisketai sto arxeio: '"+inputfile_noExtension+".asm'");
	if (executableCreated):
		print("\t>>> Exei epishs dhmiourgh8ei ektelesimo arxeio ("+inputfile_noExtension+".c) me epituxia!");
	else:
		print("\t>>> Den dhmiourgh8hke extelesimo arxeio '"+inputfile_noExtension+".c' ka8ws vre8hkan sunarthseis sto arxeio eisodou!");



if __name__ == "__main__":
	main(sys.argv[1:]);