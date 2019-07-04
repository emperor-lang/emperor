#include "AST.h"

// #define MAKE_NODE(A,  S,  N) \
// 	((person*)memcpy(calloc(sizeof(person) + (N), 1), \
// 					&(person const){ .age = (A), .sex = (S) }, \
// 					sizeof(person)))

static AstNode_t **varToAstNodeArr(int total, va_list varArgs)
{
	AstNode_t **args = (AstNode_t **)malloc(total * sizeof(AstNode_t *));
	for (int i = 0; i < total; i++)
	{
		args[i] = (AstNode_t *)(varArgs + i * sizeof(AstNode_t *));
	}
	return args;
}

static AstNode_t *makeAstNode(nodeType_t nodeType, NodeValue_t value, int numChildren, AstNode_t **children)
{
	AstNode_t *newNode = (AstNode_t *)malloc(sizeof(AstNode_t));
	newNode->nodeType = nodeType;
	newNode->value = value;
	if (children != NULL && numChildren != 0)
	{
		memcpy(&(newNode->children), children, numChildren * sizeof(AstNode_t *));
		newNode->numChildren = numChildren;
	}
	else
	{
		newNode->numChildren = 0;
		// newNode->children = NULL;
	}

	return newNode;
}

AstNode_t *makeLeaf(nodeType_t nodeType, NodeValue_t value)
{
	return makeAstNode(nodeType, value, 0, NULL);
}

AstNode_t *makeJoiningNode(nodeType_t nodeType, int numChildren, ...)
{
	// REsolve arguments in to **
	va_list argPtr;
	va_start(argPtr, numChildren);
	AstNode_t **children = varToAstNodeArr(numChildren, argPtr);
	va_end(argPtr);

	return makeAstNode(nodeType, (NodeValue_t){.misc = NULL}, numChildren, children);
}

AstNode_t *makeNode(nodeType_t nodeType, NodeValue_t value, int numChildren, ...)
{
	va_list argPtr;
	va_start(argPtr, numChildren);
	AstNode_t **children = varToAstNodeArr(numChildren, argPtr);
	va_end(argPtr);

	return makeAstNode(nodeType, value, numChildren, children);
}

void printNode(AstNode_t *node)
{
	char *buffer = NULL;
	int len;

	// Get the output in the right format
	len = sprintf(buffer, "<AstNode %p: %d>(%d)", node, node->nodeType, node->numChildren);

	if (len != -1)
	{
		printf("%s\n", buffer);
		free(buffer);
	}
}

void destroyNode(AstNode_t *node)
{
	printf("%s%d\n", "Destroying node of type ", node->nodeType);
	for (int i = 0; i < node->numChildren; i++)
	{
		destroyNode(node->children[i]);
	}
	free(node);
}
