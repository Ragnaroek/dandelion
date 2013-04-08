#include <math.h>
/* Table of constant values */

static int N = 100000;
static int NC = 3;
static int P;
static int PS;

static double X[] = {10.0, 5.0, -5.0};
static double **data;

/*  ***  SIMPLE EXAMPLE PROGRAM FOR DGLFB  *** */


double model(double *x, int i) {

	return((data[i][0] <= x[0]) ? x[1] * sin(data[i][0]) : x[2] * cos(data[i][0]));
}


void init_data(int nrows, int ncols) {

	int i, j;

	for (i = 0; i < nrows; ++i) {
		for (j = 0; j < ncols; ++j) {
			data[i][j] = 0.01 * (double) (i - nrows/2);
		}
	}
}


int main() {

    extern void dglfb_(), dglgb_();
    extern void madrj_(), rhols_();

    static double *b;
    static double *v, *x;
    static int ui[1], *iv, lv;
    static double ur[1];
    static int liv;

	int i;

	data = (double **) malloc(N * sizeof(double *));
	for (i = 0; i < N; ++i) {
		data[i] = (double *) malloc(1 * sizeof(double));
	}
	init_data(N, 1);

	x = (double *) malloc(NC * sizeof(double));
	b = (double *) malloc(2 * NC * sizeof(double));

	P = PS = NC;
	lv = N * (P + 5 + (P - PS + 2) * (P - PS + 1) / 2) + 4 * PS + P * (2 * P + 20) + 105;
	liv = 5 * P + 82;

	v = (double *) malloc(lv * sizeof(double));
	iv = (int *) malloc(lv * sizeof(int));

	/* initial values and bounds */
	for (i = 0; i < NC; ++i) {
		x[i] = 0.0;
		b[2 * i] = -10.0;
		b[2 * i + 1] = 10.0;
	}
	x[0] = 9.0;
	b[0] = 0.0;
	b[1] = 20.0;

	/* default parameters for dglfb */
    iv[0] = 0;

    dglfb_(&N, &NC, &NC, x, b, rhols_, ui, ur, iv, &liv, &lv, v, 
	    madrj_, ui, ur, madrj_);
}


void madrj_(n, p, x, nf, need, r__, rp, ui, ur, uf)
int *n, *p;
double *x;
int *nf, *need;
double *r__, *rp;
int *ui;
double *ur;
int (*uf) ();
{
	int i;

    /* Parameter adjustments */
    --r__;

	for (i = 1; i <= *n; ++i) {
		r__[i] = model(x, i - 1);
	}
}

void rhols_(need, f, n, nf, xn, r__, rp, ui, ur, w)
int *need;
double *f;
int *n, *nf;
double *xn, *r__, *rp;
int *ui;
double *ur, *w;
{
	int i;
	double sum, tmp;

    /* Parameter adjustments */
    --rp;
    --r__;

	if (need[0] == 1) {
		for (sum = 0.0, i = 1; i <= *n; ++i) {
			tmp = r__[i] - model(X, i - 1);
			sum += fabs(tmp);
		}
		*f = 0.5 * sum;
	} else {
		for (i = 1; i <= *n; ++i) {
			r__[i] = (r__[i] - model(X, i - 1) < 0.0) ? -0.5 : 0.5;
			rp[i] = 0.0;
		}
	}
}
