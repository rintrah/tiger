#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

struct string {long length; unsigned char chars[1];};

struct string constantes[256];
struct string empty={0, ""};

long* _newArray(long size, long init)
{
	long *p, i;
	if (size < 0)
		 exit(1);
	if ((p=(long *)malloc((size+1)*sizeof(long))) == NULL)
		 exit(1);
	p[0] = size;
	for(i=1; i < (size+1);i++)
		p[i]=init;
//	printf("En la dirección de memoria: %x ,se creó un nuevo array\n",p+1);
	return (p+1);
}

void _checkindex(long *array, long indice)
{
	if(indice < 0 || indice > array[-1]){
		printf("¡El índice del arreglo está fuera de rango!\n");
		exit(1);
	}
}

long* _newRecord(int ctos, ...)
{
	va_list va;
	long *p;
	int i;
	if ((p=(long *)malloc(ctos*sizeof(long))) == NULL) 
		exit(1);
	va_start(va, ctos);
	for(i=0; i<ctos; i++)
		p[i] = va_arg(va, long);
	va_end(va);
	return p;
}

void _checkNil(long *pointer)
{
 if(pointer ==  0){
	printf("¡El record es un puntero a NULL!\n");
 	exit(1);
	}
}

struct string * concat(struct string* a, struct string *b)
{
	if (a->length == 0) return b;
	else if (b->length == 0) return a;
			else { int i, n = a->length + b->length;
						struct string *t = (struct string *)malloc(sizeof(long)+n);
						if (t==NULL) {
								printf("¡Error en la asignación de memoria para la concatenación.\n");
								exit(1);
						}
						for(i=0; i < a->length; i++)
							t->chars[i] = a->chars[i];
						for(i=0; i < b->length; i++)
							t->chars[i+a->length] = b->chars[i];
						return t;
			}
}
void print(struct string *t)
{
	int i;
	unsigned char *p=t->chars;
	for(i=0; i < t->length; i++, p++)
		putchar(*p);
}

int not(int i)
{
	return !i;
}

int main()
{
	int i;
	for(i=0; i < 256; i++){ 
		constantes[i].length=1;
		constantes[i].chars[0]=i;
	}
	return _tigermain(0);
}

struct string *chr(int indice)
{
	if (indice < 0 || indice > 256) {
		printf("El índice %d está fuera de rango\n", indice);
		exit(1); 
	}
	return constantes+indice;
}

void flush()
{	
	fflush(stdout);
}

int ord(struct string *s)
{
	if (s->length == 0) return -1;
	else return s->chars[0];
}

int _stringEqual(struct string *s, struct string *t) 
{
	int i;
	if (s == t) return 1;
	if (s->length != t->length) return 0;
	for (i = 0; i < s->length; i++) if (s->chars[i] != t->chars[i]) return 0;
	return 1;
}

struct string *getstr()
{
	int i = getc(stdin);
	if (i==EOF) return &empty;
	else return constantes+i;
}	
