# include <stdlib.h>
# include <stdio.h>

typedef struct action
{
	// action_t parent;
	void** args;
	
} action_t;

void action(void)
{
	printf("Action taken!");
	// void (*foo) (void) asdf = &action;
}

action_t* makeAction(action_t parent, void** args)
{
	action_t* action = (action_t*)calloc(1, sizeof(action_t));
	return action;	
}
