#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

typedef struct {
    long length;
    unsigned char chars[1];
} string;
const string consts[256] = {
	{1, (char)0}, {1, (char)1}, {1, (char)2},
	{1, (char)3}, {1, (char)4}, {1, (char)5},
	{1, (char)6}, {1, (char)7}, {1, (char)8},
	{1, (char)9}, {1, (char)10}, {1, (char)11},
	{1, (char)12}, {1, (char)13}, {1, (char)14},
	{1, (char)15}, {1, (char)16}, {1, (char)17},
	{1, (char)18}, {1, (char)19}, {1, (char)20},
	{1, (char)21}, {1, (char)22}, {1, (char)23},
	{1, (char)24}, {1, (char)25}, {1, (char)26},
	{1, (char)27}, {1, (char)28}, {1, (char)29},
	{1, (char)30}, {1, (char)31}, {1, (char)32},
	{1, (char)33}, {1, (char)34}, {1, (char)35},
	{1, (char)36}, {1, (char)37}, {1, (char)38},
	{1, (char)39}, {1, (char)40}, {1, (char)41},
	{1, (char)42}, {1, (char)43}, {1, (char)44},
	{1, (char)45}, {1, (char)46}, {1, (char)47},
	{1, (char)48}, {1, (char)49}, {1, (char)50},
	{1, (char)51}, {1, (char)52}, {1, (char)53},
	{1, (char)54}, {1, (char)55}, {1, (char)56},
	{1, (char)57}, {1, (char)58}, {1, (char)59},
	{1, (char)60}, {1, (char)61}, {1, (char)62},
	{1, (char)63}, {1, (char)64}, {1, (char)65},
	{1, (char)66}, {1, (char)67}, {1, (char)68},
	{1, (char)69}, {1, (char)70}, {1, (char)71},
	{1, (char)72}, {1, (char)73}, {1, (char)74},
	{1, (char)75}, {1, (char)76}, {1, (char)77},
	{1, (char)78}, {1, (char)79}, {1, (char)80},
	{1, (char)81}, {1, (char)82}, {1, (char)83},
	{1, (char)84}, {1, (char)85}, {1, (char)86},
	{1, (char)87}, {1, (char)88}, {1, (char)89},
	{1, (char)90}, {1, (char)91}, {1, (char)92},
	{1, (char)93}, {1, (char)94}, {1, (char)95},
	{1, (char)96}, {1, (char)97}, {1, (char)98},
	{1, (char)99}, {1, (char)100}, {1, (char)101},
	{1, (char)102}, {1, (char)103}, {1, (char)104},
	{1, (char)105}, {1, (char)106}, {1, (char)107},
	{1, (char)108}, {1, (char)109}, {1, (char)110},
	{1, (char)111}, {1, (char)112}, {1, (char)113},
	{1, (char)114}, {1, (char)115}, {1, (char)116},
	{1, (char)117}, {1, (char)118}, {1, (char)119},
	{1, (char)120}, {1, (char)121}, {1, (char)122},
	{1, (char)123}, {1, (char)124}, {1, (char)125},
	{1, (char)126}, {1, (char)127}, {1, (char)128},
	{1, (char)129}, {1, (char)130}, {1, (char)131},
	{1, (char)132}, {1, (char)133}, {1, (char)134},
	{1, (char)135}, {1, (char)136}, {1, (char)137},
	{1, (char)138}, {1, (char)139}, {1, (char)140},
	{1, (char)141}, {1, (char)142}, {1, (char)143},
	{1, (char)144}, {1, (char)145}, {1, (char)146},
	{1, (char)147}, {1, (char)148}, {1, (char)149},
	{1, (char)150}, {1, (char)151}, {1, (char)152},
	{1, (char)153}, {1, (char)154}, {1, (char)155},
	{1, (char)156}, {1, (char)157}, {1, (char)158},
	{1, (char)159}, {1, (char)160}, {1, (char)161},
	{1, (char)162}, {1, (char)163}, {1, (char)164},
	{1, (char)165}, {1, (char)166}, {1, (char)167},
	{1, (char)168}, {1, (char)169}, {1, (char)170},
	{1, (char)171}, {1, (char)172}, {1, (char)173},
	{1, (char)174}, {1, (char)175}, {1, (char)176},
	{1, (char)177}, {1, (char)178}, {1, (char)179},
	{1, (char)180}, {1, (char)181}, {1, (char)182},
	{1, (char)183}, {1, (char)184}, {1, (char)185},
	{1, (char)186}, {1, (char)187}, {1, (char)188},
	{1, (char)189}, {1, (char)190}, {1, (char)191},
	{1, (char)192}, {1, (char)193}, {1, (char)194},
	{1, (char)195}, {1, (char)196}, {1, (char)197},
	{1, (char)198}, {1, (char)199}, {1, (char)200},
	{1, (char)201}, {1, (char)202}, {1, (char)203},
	{1, (char)204}, {1, (char)205}, {1, (char)206},
	{1, (char)207}, {1, (char)208}, {1, (char)209},
	{1, (char)210}, {1, (char)211}, {1, (char)212},
	{1, (char)213}, {1, (char)214}, {1, (char)215},
	{1, (char)216}, {1, (char)217}, {1, (char)218},
	{1, (char)219}, {1, (char)220}, {1, (char)221},
	{1, (char)222}, {1, (char)223}, {1, (char)224},
	{1, (char)225}, {1, (char)226}, {1, (char)227},
	{1, (char)228}, {1, (char)229}, {1, (char)230},
	{1, (char)231}, {1, (char)232}, {1, (char)233},
	{1, (char)234}, {1, (char)235}, {1, (char)236},
	{1, (char)237}, {1, (char)238}, {1, (char)239},
	{1, (char)240}, {1, (char)241}, {1, (char)242},
	{1, (char)243}, {1, (char)244}, {1, (char)245},
	{1, (char)246}, {1, (char)247}, {1, (char)248},
	{1, (char)249}, {1, (char)250}, {1, (char)251},
	{1, (char)252}, {1, (char)253}, {1, (char)254},
	{1, (char)255}
};
string empty = {0, ""};

long *_initArray(long size, long init)
{
    int i;
    long *a = malloc(size * sizeof(long) + 1);
	a[0] = size;
    for (i = 1; i <= size; i++)
		a[i] = init;
    return a+1;
}
void _checkIndexArray(long *a, long i)
{
	if(i<0 || i>a[-1]) {
		fprintf(stderr, "indice %ld excedido!\n", i);
		exit(-1);
	}
}
long *_allocRecord(long ctos, ...)
{
    int i;
    long *p, *a;
	va_list va;
    p = a = malloc(ctos*sizeof(long));
	va_start(va, ctos);
    for (i = 0; i < ctos; i ++)
		*p++ = va_arg(va, long);
    return a;
}
void _checkNil(long* r)
{
	if(r==0) {
		fprintf(stderr, "Nil!\n");
		exit(-1);
	}
}
long _stringCompare(string *s, string *t)
{
    int i;
    if(s == t)
		return 0;
    for(i = 0; i<s->length && i<t->length; i++)
		if(s->chars[i]!=t->chars[i])
		    return s->chars[i]-t->chars[i];
	return s->length-t->length;
}
void print(string *s)
{
    int i;
    unsigned char *p = s->chars;
    for (i = 0; i < s->length; i++, p++)
		putchar(*p);
}
void flush()
{
    fflush(stdout);
}
long ord(string *s)
{
    if (s->length == 0)
		return -1;
    else
		return s->chars[0];
}
string *chr(long i)
{
    if (i < 0 || i >= 256) {
		printf("chr(%ld) out of range\n", i);
		exit(1);
    }
    return (string*)(consts + i);
}
long size(string *s)
{
    return s->length;
}
string *substring(string *s, long first, long n)
{
    if (first < 0 || first + n > s->length) {
		printf("substring([%ld],%ld,%ld) out of range\n", s->length, first, n);
		exit(1);
    }
    if (n == 1)
		return (string*)(consts + s->chars[first]);
    {
		string *t = malloc(sizeof(long) + n);
		int i;
		t->length = n;
		for (i = 0; i < n; i++)
		    t->chars[i] = s->chars[first + i];
		return t;
    }
}
string *concat(string *a, string *b)
{
    if (a->length == 0)
		return b;
    else if (b->length == 0)
		return a;
    else {
		int i, n = a->length + b->length;
		string *t = malloc(sizeof(long) + n);
		t->length = n;
		for (i = 0; i < a->length; i++)
		    t->chars[i] = a->chars[i];
		for (i = 0; i < b->length; i++)
		    t->chars[i + a->length] = b->chars[i];
		return t;
    }
}
long not(long i)
{
    return !i;
}
string *getstr()
{
    int i = getc(stdin);
    if (i == EOF)
		return &empty;
    else
		return (string*)(consts + i);
}
int main()
{
    int i;
	/*
    for (i = 0; i < 256; i++) {
		consts[i].length = 1;
		consts[i].chars[0] = i;
    }
	*/
    return _tigermain(0 /* static link!? */ );
}
