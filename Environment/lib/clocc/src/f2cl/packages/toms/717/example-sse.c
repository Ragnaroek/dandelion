/* Table of constant values */

static long int N = 100000;
static long int NC = 3;
static long int P;
static long int PS;

static double X[] = {10.0, 5.0, -5.0};
static double **data;

/*  ***  SIMPLE EXAMPLE PROGRAM FOR DGLFB  *** */


double model(double *x, int i) {

	return((data[i][0] <= x[0]) ? x[1] * sin(data[i][0]) : x[2] * cos(data[i][0]));
}


void init_data(long int nrows, long int ncols) {

	int i, j;

	for (i = 0; i < nrows; ++i) {
		for (j = 0; j < ncols; ++j) {
			data[i][j] = 0.01 * (double) (i - nrows/2);
		}
	}
}


int main() {

    extern int dglfb_(), dglgb_();
    extern int madrj_(), rhols_();

    static double *b;
    static double *v, *x;
    static long int ui[1], *iv, lv;
    static double ur[1];
    static long int liv;

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
	iv = (long int *) malloc(lv * sizeof(long int));

	/* initial values and bounds */
	for (i = 0; i < NC; ++i) {
		x[i] = 0.0;
		b[2 * i] = -10.0;
		b[2 * i + 1] = 10.0;
	}
	x[0] = 10.0;
	b[0] = 0.0;
	b[1] = 20.0;

	/* default parameters for dglfb */
    iv[0] = 0;

    dglfb_(&N, &NC, &NC, x, b, rhols_, ui, ur, iv, &liv, &lv, v, 
	    madrj_, ui, ur, madrj_);
}


int madrj_(n, p, x, nf, need, r__, rp, ui, ur, uf)
long int *n, *p;
double *x;
long int *nf, *need;
double *r__, *rp;
long int *ui;
double *ur;
int (*uf) ();
{
	int i;
    extern double sin(), cos();

    /* Parameter adjustments */
    --r__;

	for (i = 1; i <= *n; ++i) {
		r__[i] = model(x, i - 1);
	}
    return 0;
}

int rhols_(need, f, n, nf, xn, r__, rp, ui, ur, w)
long int *need;
double *f;
long int *n, *nf;
double *xn, *r__, *rp;
long int *ui;
double *ur, *w;
{
	int i;
	double sum, tmp;

    extern double sin(), cos();

    /* Parameter adjustments */
    --rp;
    --r__;

	if (need[0] == 1) {
		for (sum = 0.0, i = 1; i <= *n; ++i) {
			tmp = r__[i] - model(X, i - 1);
			sum += tmp * tmp;
		}
		*f = 0.5 * sum;
	} else {
		for (i = 1; i <= *n; ++i) {
			r__[i] = r__[i] - model(X, i - 1);
			rp[i] = 1.0;
		}
	}

    return 0;
}
