#include<stdio.h>
#include<stdlib.h>

double** multiply(double** , double** , double** , int, int);
double** divide(double** , int, int, int, double);
double** subtract(double** , int, int, int, int);
double** multsub(double** , int, int, int, int, double);
double** inverse(double**, double**, int);
double** multfin(double** , double** , double** , int, int, int);
double** makematrix(int , int );
void freed(double**, int);
void freed(double** matrix, int rows){
for(int i=0; i< rows; i++){
	free(matrix[i]);
}
free(matrix);
return;
}
double** makematrix( int row, int col){
	double** matrix = (double**) malloc(row * sizeof(double*));
	for (int i=0; i< row; i++){
		matrix[i] = (double*) malloc(col * sizeof(double));
	}
return matrix;	
}

double** multfin(double** xarr, double** weights, double** result, int m1row, int m2col, int share){
	double sum=0;
	for (int i=0; i< m1row; i++){
		for(int j=0; j<m2col; j++){
			for(int n=0; n<share; n++){
				sum += xarr[i][n]* weights[n][j];
			}
		result[i][j] = sum;
		sum = 0;
		}
	}

return result;	
}


double** multiply(double** matrix1, double** matrix2, double** result, int xcol, int share){

	double sum=0;
	for (int i=0; i< xcol; i++){
		for(int j=0; j<xcol; j++){
			for(int n=0; n<share; n++){
				sum += matrix1[i][n]*matrix2[n][j];	
			}
		result[i][j] = sum;
		sum = 0;
		}
	}

return result; 

}

double** divide(double** matrix, int rows, int i , int j , double constant){
	for(int n =j; n <rows; n++){

	matrix[i][n] = matrix[i][n]/constant;

	}
return matrix;
}

double** subtract(double** matrix, int rows, int i , int k , int j){
	for(int n = j; n<rows; n++){
	matrix[k][n] = matrix[k][n] - matrix[i][n];
	}
return matrix;
}

double** multsub(double** matrix, int rows, int i , int k , int j, double constant){
	for(int n =j; n< rows; n++){
	matrix[k][n] = matrix[k][n] - (constant*matrix[i][n]);
	}
return matrix;
}

double** inverse(double** toinverse, double**identity, int row){
double constant;

	for(int i = 0; i< row;i++){

		if(toinverse[i][i] != 1){
		constant = toinverse[i][i];

		toinverse = divide(toinverse, row, i, i, constant);
		identity = divide(identity, row, i, 0, constant);

		}
		if(toinverse[i][i]==1){
			for(int k = i+1; k< row; k++){
				if(toinverse[k][i] <0 || toinverse[k][i] >1){
				constant = toinverse[k][i];
				toinverse = multsub(toinverse, row, i, k, i, constant);
				identity = multsub(identity, row, i, k, 0, constant);
	

				}
				else if(toinverse[k][i]==1){
				toinverse = subtract(toinverse, row, i, k, i);
				identity = subtract(identity, row, i, k, 0);
        }

				else if(toinverse[k][i] >0 && toinverse[k][i] < 1){
				constant = toinverse[k][i];
				toinverse = multsub(toinverse, row, i, k, i, constant);
				identity = multsub(identity, row, i, k, i, constant);
        }
        
				else if(toinverse[k][i] == 0 && k +1 == row){
				break;
				}
			}// first k loop
		}// if pivot = 1
	}//first i loop

	for(int i = row-1; i >=0 ;i--){
		if(toinverse[i][i] >1){
		constant = toinverse[i][i];
		toinverse = divide(toinverse, row, i, i, constant);
		identity = divide(identity, row, i, 0, constant);
				
		}
		if(toinverse[i][i]==1){
			for(int k = i-1; k>=0 ; k--){

				if(toinverse[k][i] <0){
				constant = toinverse[k][i];
				toinverse = multsub(toinverse, row, i, k, i, constant);
				identity = multsub(identity, row, i, k, 0, constant);

				}
				else if(toinverse[k][i]==1){
				toinverse = subtract(toinverse, row, i, k, i);
				identity = subtract(identity, row, i, k, 0);
				}
				else if(toinverse[k][i] >1){
				constant = toinverse[k][i];
				toinverse = multsub(toinverse, row, i, k, i, constant);
				identity = multsub(identity, row, i, k, 0, constant);

				}
				else if(toinverse[k][i] >0 && toinverse[k][i] < 1){
				constant = toinverse[k][i];
				toinverse = multsub(toinverse, row, i, k, i, constant);
				identity = multsub(identity, row, i, k, 0, constant);

}
				else if(toinverse[k][i] == 0 && k +1 == row){
				break;
				}
			}// first k loop
		}// if pivot = 1
	}
return identity;
}
int main (int argc, char**argv){
FILE* tr;
FILE*td;
int xcol;
int trcol;
int row;
int houses;
double** training;
double** y;
double** x; 
double** xtrans;
double** toinverse; 
double** xinverse; 
double** augment; 
double** product; 
double** w; 
double**prices;
double** attributes;
if (argc!= 3){
		return 0;
	}
	tr = fopen(argv[1], "r");
	td = fopen(argv[2],"r");
	if(tr == NULL){
		return 0;
	}
	if(td == NULL){
		return 0;
	}
	fscanf(tr,"%d %d", &xcol, &row);
	trcol = xcol+1;

training = makematrix(row, trcol);
	for (int i=0; i< row; i++){
		for (int j=0;j< trcol; j++){
			fscanf(tr, "%lf%*c", &training[i][j]);
		}
	}

	y = makematrix(row, 1);
	for(int i = 0; i< row; i++){
		y[i][0] = training[i][xcol];
	}

x = makematrix(row, trcol);
	for(int i = 0; i< row; i++){
	x[i][0] = 1;
	}
	for (int i=0; i< row; i++){
		for (int j=1;j< trcol; j++){
			x[i][j] =training[i][j-1];
		}
		
	}

	xtrans = makematrix(trcol, row);
	for(int i =0; i< row; i++){
		for(int j =0; j< trcol; j++){
			xtrans[j][i] = x[i][j];
		}

	}

toinverse = makematrix(trcol, trcol);
	for(int i=0; i<trcol; i++){
		for(int j=0; j< trcol; j++){
			toinverse[i][j] = 0; 	
		}
	}

toinverse = multfin(xtrans, x, toinverse, trcol, trcol, row);

xinverse = makematrix(trcol, trcol);
	for(int i=0; i< trcol; i++){
		for(int j=0; j< trcol; j++){
			xinverse[i][j] = 0; 
		}
	}

augment = makematrix(trcol, trcol);
	for(int i=0; i< trcol; i++){
		for(int j=0; j< trcol; j++){
			augment[i][j] = 0; 
		}
	}
	for(int i=0; i< trcol; i++){
			augment[i][i]=1;
	}

xinverse = inverse(toinverse, augment, trcol);

product = makematrix(trcol, row);
	for(int i=0; i< trcol; i++){
		for(int j=0; j< row; j++){
			product[i][j] = 0; 
		}
	}

product = multfin(xinverse, xtrans, product, trcol, row, trcol);
w = makematrix(trcol, 1);
for(int i =0; i< trcol; i++){
w[i][0] = 0.0;
}
w = multfin(product, y, w, trcol, 1, row);
//  open test data file fscanf 

fscanf(td,"%d ", &houses);
attributes = makematrix(houses, trcol);
for(int i = 0; i< houses; i++){
	attributes[i][0] = 1;
}
for (int i=0; i< houses; i++){
		for (int j=1;j< trcol; j++){
			fscanf(td, "%lf%*c", &attributes[i][j]);
		}
	}
prices = makematrix(houses, 1);
prices = multfin(attributes, w, prices, houses, 1, trcol);

//printf("prices is: \n");
for(int i =0; i < houses; i++){
printf("%0.0lf\n", prices[i][0]);
}
freed(training, row);
freed(y, row);
freed(x, row);
freed(xtrans, trcol);
freed(toinverse, trcol);
freed(augment, trcol);
freed(product, trcol );
freed(w, trcol);
freed(prices, houses);
freed(attributes, houses);
	
fclose(tr);
fclose(td);


return 0;
}
