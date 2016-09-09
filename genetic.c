/*
author: jivfu
date: 09/08/2016
description: Genetic algorithm to create logical circuits using a truth table with 16 inputs.
The validation section is an adaptation of Dr Jimenez Code, that you can find at: http://faculty.cse.tamu.edu/djimenez/614/hw1/validate.c
*/

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<time.h>


#define MAXGATES 16
#define CHROMOSIZE 2*MAXGATES
#define POPSIZE 2*CHROMOSIZE
#define MAXCROSSPOINTS 4
#define MUTATION_PROB 0.05



#define GATE_AND	1
#define GATE_OR		2
#define GATE_XOR	3
#define GATE_NOT	4
#define GATE_NAND	5
#define GATE_NOR	6

typedef struct c {
	int gen[CHROMOSIZE]; 
	int fitness; //number of gates true;
} chromo;

struct gate {
	int type;
	int op1, op2;
	int val;
};

struct gate gates[MAXGATES];

chromo population[POPSIZE];
chromo newPopulation[POPSIZE];

int tt[16];
int population_fitness = 0;
int numberPoints;
int crossPoints[MAXCROSSPOINTS];

/*FUNCTION DECLARATIONS*/
void print_individual();

/*VALIDATE*/

void get_tt (char *fname) {
	char	s[1000];
	int	i, nrows = 0;

	FILE *f = fopen (fname, "r");
	if (!f) {
		perror (fname);
		exit (1);
	}

	/* skip past the first two lines */

	fgets (s, 1000, f);
	fgets (s, 1000, f);

	/* loop, getting rows of the truth table; actually we're just
	 * getting the last 0-or-1 digit in the strings
	 */
	for (;;) {
		char ch, *p;
		fgets (s, 1000, f);
		if (feof (f)) break;
		p = s + strlen (s);
		while (p != s && !(*p == '0' || *p == '1')) p--;
		if (*p == '0' || *p == '1') {
			tt[nrows++] = *p - '0';
		}
	}
	if (nrows != 16) {
		fprintf (stderr, "error: expecting 16 rows in the truth table!\n");
		exit (1);
	}
	fclose (f);
}

int eval (int n, int a, int b, int c, int d) {
	if (n < 0) {
		switch (n) {
		case -4: return a;
		case -3: return b;
		case -2: return c;
		case -1: return d;
		default:
			fprintf (stderr, "error: unknown variable %d!\n", n);
			exit (1);
		}
	}
	switch (gates[n].type) {
	case GATE_XOR: return eval (gates[n].op1, a, b, c, d) ^ eval (gates[n].op2, a, b, c, d);
	case GATE_AND: return eval (gates[n].op1, a, b, c, d) && eval (gates[n].op2, a, b, c, d);
	case GATE_OR: return eval (gates[n].op1, a, b, c, d) || eval (gates[n].op2, a, b, c, d);
	case GATE_NOT: return ! eval (gates[n].op1, a, b, c, d);
	case GATE_NAND: return !(eval (gates[n].op1, a, b, c, d) && eval (gates[n].op2, a, b, c, d));
	case GATE_NOR: return !(eval (gates[n].op1, a, b, c, d) || eval (gates[n].op2, a, b, c, d));
	default: 
		fprintf (stderr, "error(2): unknown gate type %d on gate %d!\n", gates[n].type, n);
		exit (1);
	}
	
}


void print_circuit (int ngates) {
	int i;
	for (i=0; i<ngates; i++) {
		char ops[3];
		switch (gates[i].type) {
		case GATE_AND: strcpy (ops, "&&"); break;
		case GATE_OR: strcpy (ops, "||"); break;
		case GATE_NOT: strcpy (ops, "!!"); break;
		case GATE_XOR: strcpy (ops, "^^"); break;
		case GATE_NAND: strcpy (ops, "!&"); break;
		case GATE_NOR: strcpy (ops, "!|"); break;
		default: 
			fprintf (stderr, "error(1): unknown gate type %d!\n", gates[i].type);
			exit (1);
		}
		printf ("%d: %s %d %d\n", i, ops, gates[i].op1, gates[i].op2);
	}
}


/*GENETIC*/

void inicialize_population(){
	int g;
	for(int i=0;i<POPSIZE;i++){
		for(int j=0;j<2*MAXGATES;j+=2){	//10		
			for(int h=0;h<2;h++){
				int d = (j/2)+4;
				g = -4 + rand()%(d);
				population[i].gen[j+h]=g;
			}
		}		
	}
}

void evaluate_individual(int individual){
	char s[1000];
	int ok;
	int errors = 0;
	for(int i=0;i<MAXGATES;i++){
		gates[i].type = GATE_NAND;
		for(int j=0;j<2*i+1;j++){			
			gates[i].op1 = population[individual].gen[j];
			j++;
			gates[i].op2 = population[individual].gen[j];
		}
	}
	//print_circuit(MAXGATES);

	//printf ("checking...\n-4 -3 -2 -1   | orig  yours\n");
	for (int i=0; i<16; i++) {
		int a = !!(i & 8);
		int b = !!(i & 4);
		int c = !!(i & 2);
		int d = !!(i & 1);
		int x = eval (MAXGATES-1, a, b, c, d);
		//printf (" %d  %d  %d  %d   |  %d      %d", a, b, c, d, tt[i], x);
		if (x != tt[i]) {
			//printf ("*");
			errors++;
		}
		//printf ("\n");
	}
	population[individual].fitness = 16-errors;
	if(errors == 0){
		print_individual(individual);
	}
	//print_individual(0);
}

void evaluate_population(){
	population_fitness = 0;
	for(int i=0;i<POPSIZE;i++){
		evaluate_individual(i);
		population_fitness += population[i].fitness;
	}
}



/*SELECTION*/
int select_individual(){ //roulette
	float aux = (float) rand()/RAND_MAX;
	int i=-1;
	float sum=0;
	do{
		i++;
		sum+=(float)population[i].fitness/population_fitness;
	}while(sum<aux);
	return i;
}

/*CROSSOVER*/

void bubble_sort(){
	for (int c = 0 ; c < ( MAXCROSSPOINTS - 1 ); c++){
		for (int d = 0 ; d < MAXCROSSPOINTS - c - 1; d++){
			if (crossPoints[d] > crossPoints[d+1]){
				int swap       = crossPoints[d];
				crossPoints[d]   = crossPoints[d+1];
				crossPoints[d+1] = swap;
			}
		}
	}
}


void create_crosspoints(){
	int nInside=0;
	int i=0;
	int band = 1;
	int n ;
	int cZ = CHROMOSIZE;
	do{
		band = 1;
		n = 1+ rand()%(cZ-1);		
		for(int i=0;i<nInside;i++){
			if(crossPoints[i]==n){
				band = 0;
				i=100;
			}
		}
		if(band==1){
			crossPoints[nInside] = n;
			nInside++;
		}
	}while(nInside<MAXCROSSPOINTS);
	bubble_sort();
	/*printf("Crosspoints: ");
	for(int i=0;i<MAXCROSSPOINTS;i++){
		printf("%d ",crossPoints[i]);
	}
	printf("\n");*/
}

void crossover(int a, int b, int nSize){
    chromo newA, newB;
    int aux;
    int nCreated=0;	
    create_crosspoints();

	if(a!=b){
		newA = population[a];
		newB = population[b];
		for(int i=0;i<MAXCROSSPOINTS-1;i+=2){
			if(crossPoints[i]!=0){
				for(int j=crossPoints[i];j<crossPoints[i+1];j++){
					aux=newA.gen[j];
					newA.gen[j]=newB.gen[j];
					newB.gen[j] = aux;
				}
			}
		}
		newPopulation[nSize] = newA;
		newPopulation[nSize+1]=newB;
	}else{
		fprintf(stderr,"The same indivual");
		exit(1);
	}
}



void mutation(){
	for(int i=0;i<POPSIZE;i++){
		for(int j=0;j<CHROMOSIZE;j++){
			float m = (float) rand()/RAND_MAX;
			if(MUTATION_PROB>m){
				newPopulation[i].gen[j] = (-4 + rand()%(4 + j/2));
			}
		}
	}
}

void print_individual(int individual){
	for(int j=0;j<CHROMOSIZE;j++){
			printf("%d|",population[individual].gen[j]);
	}
	printf(" Fit: %d\n",population[individual].fitness);
}

void print_population(){
	for(int i=0;i<POPSIZE;i++){
		printf("%d: ",i);
		for(int j=0;j<CHROMOSIZE;j++){
			printf("%d|",population[i].gen[j]);
		}
		printf(" FIT: %d \n",population[i].fitness);
	}
	float pz = POPSIZE;
	printf("Population Performance: %f\n",population_fitness/pz);

}

void copy_population(){
	for(int i=0;i<POPSIZE;i++){
		population[i] = newPopulation[i];
	}

}

void save_population(int epoch, long int seconds){
	char fileName[80];
	
	
	sprintf(fileName,"Pop%ld/poblacion%d.txt",seconds,epoch);
	FILE *fp = fopen(fileName,"w");
	if(fp!=NULL){
		for(int i=0;i<POPSIZE;i++){
			fprintf(fp,"%d: ",i);
			for(int j=0;j<CHROMOSIZE;j++){
				fprintf(fp,"%d ",population[i].gen[j]);
			}
			fprintf(fp, "\n");
			fprintf(fp,"Fit: %d\n",population[i].fitness);
		}
		fclose(fp);
	}

}

int find_best(){
	int max = -1;
	int index=-1;
	for(int i=0;i<POPSIZE;i++){
		if(population[i].fitness>max){
			max = population[i].fitness;
			index = i;
		}
	}
	return index;

}


int main(int argc, char **argv){
	int a,b;
	char path[80];
	if (argc != 3) {
		fprintf (stderr, "Usage: %s <truth-table> <number of epochs>\n", argv[0]);
		return 0;
	}
	time_t t;  
   /* Intializes random number generator */
   srand((unsigned) time(&t));
	get_tt (argv[1]);
	int maxEpochs = atoi(argv[2]);
	inicialize_population();	
	time_t seconds;
   	seconds = time(NULL);
   	sprintf(path,"mkdir Pop%ld",seconds);
	system(path);
	printf("Los resultados se guardan en: Pop%ld\n",seconds);
	for(int epoch=0;epoch<maxEpochs;epoch++){
		evaluate_population();
		printf("%d: ",epoch);			
		print_individual(find_best());
		//print_population();	
		for(int i=0;i<POPSIZE+2;i+=2){
			a= select_individual(); 			
			do{
				b= select_individual();
			}while(a==b);
			crossover(a,b,i);
			mutation();
		}
		save_population(epoch,seconds);
		copy_population();
	}
	return 1;

}