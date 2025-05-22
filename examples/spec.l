%{
    /* Need this for the call to atof() below. */
#include <math.h>
/* Need this for printf(), fopen(), and stdin below. */
#include <stdio.h>
%}
%{ //Warning this shoudln't be visible
    // This is a test1
    %}

%{
    // This is a test2
%}

%{
    // This is a test3
%}
DIGIT    [0-9]
ID       [a-z][a-z0-9]*

A   hello
B   {A}{C}
C   {B}

%{
    // This is a test4
%}
%%

\ [ [] {
    printf("Hello world")
}
{DIGIT}+ {
    printf("An integer: %s (%d)\n", yytext,
        atoi(yytext));
    }


{DIGIT}+"."{DIGIT}*        {
    printf("A float: %s (%g)\n", yytext,
        atof(yytext));
    }


if|then|begin|end|procedure|function        {
    printf("A keyword: %s\n", yytext);
    }

{ID}    printf("An identifier: %s\n", yytext);


"+"|"-"|"*"|"/"        printf("An operator: %s\n", yytext);


"{"[^}\n]*"}"    /* Eat up one-line comments. */


[ \t\n]+        /* Eat up white space. */

A printf("This is A");

.  printf("Unrecognized character: %s\n", yytext);


%%


int main(int argc, char *argv[])
{
    ++argv, --argc;  /* Skip over program name. */
    if (argc > 0)
        yyin = fopen(argv[0], "r");
    else
        yyin = stdin;


    yylex();
}
