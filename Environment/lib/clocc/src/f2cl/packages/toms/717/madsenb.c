/* madsenb.f -- translated by f2c (version 20000817).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__3 = 3;
static integer c__2 = 2;
static integer c__5 = 5;

/*  ***  SIMPLE TEST PROGRAM FOR DGLGB AND DGLFB  *** */

/* Main program */ int MAIN__()
{
    /* Format strings */
    static char fmt_10[] = "(\002 DGLGB ON PROBLEM MADSEN...\002)";
    static char fmt_20[] = "(\002 DGLGB NEEDED LIV .GE. ,I3,12H AND LV .GE\
.\002,i4)";
    static char fmt_30[] = "(/\002 DGLFB ON PROBLEM MADSEN...\002)";
    static char fmt_40[] = "(/\002 DGLFB ON PROBLEM MADSEN AGAIN...\002)";

    /* Builtin functions */
    integer s_wsfe(), e_wsfe(), do_fio();
    /* Subroutine */ int s_stop();

    /* Local variables */
    static integer nout;
    static doublereal b[4]	/* was [2][2] */;
    extern /* Subroutine */ int dglfb_(), dglgb_();
    static doublereal v[200], x[2];
    extern /* Subroutine */ int madrj_(), rhols_();
    extern integer i7mdcn_();
    static integer ui[1], iv[92], lv;
    static doublereal ur[1];
    extern /* Subroutine */ int divset_();
    static integer liv;

    /* Fortran I/O blocks */
    static cilist io___7 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___11 = { 0, 0, 0, fmt_20, 0 };
    static cilist io___12 = { 0, 0, 0, fmt_30, 0 };
    static cilist io___13 = { 0, 0, 0, fmt_40, 0 };



/* I7MDCN... RETURNS OUTPUT UNIT NUMBER. */


/* +++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++ */

    nout = i7mdcn_(&c__1);
    lv = 200;
    liv = 92;

/*  ***  SPECIFY INITIAL X AND BOUNDS ON X  *** */

    x[0] = 3.;
    x[1] = 1.;
/*     *** BOUNDS ON X(1)... */
    b[0] = -.1;
    b[1] = 10.;
/*     *** BOUNDS ON X(2)... */
    b[2] = 0.;
    b[3] = 2.;

/*  ***  SET IV(1) TO 0 TO FORCE ALL DEFAULT INPUT COMPONENTS TO BE USED. */

    iv[0] = 0;

    io___7.ciunit = nout;
    s_wsfe(&io___7);
    e_wsfe();

/*  ***  CALL DGLG, PASSING UI FOR RHOI, UR FOR RHOR, AND MADRJ FOR */
/*  ***  UFPARM (ALL UNUSED IN THIS EXAMPLE). */

    dglgb_(&c__3, &c__2, &c__2, x, b, rhols_, ui, ur, iv, &liv, &lv, v, 
	    madrj_, ui, ur, madrj_);

/*  ***  SEE HOW MUCH STORAGE DGLGB USED... */

    io___11.ciunit = nout;
    s_wsfe(&io___11);
    do_fio(&c__1, (char *)&iv[43], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&iv[44], (ftnlen)sizeof(integer));
    e_wsfe();

/*  ***  SOLVE THE SAME PROBLEM USING DGLFB... */

    io___12.ciunit = nout;
    s_wsfe(&io___12);
    e_wsfe();
    x[0] = 3.;
    x[1] = 1.;
    iv[0] = 0;
    dglfb_(&c__3, &c__2, &c__2, x, b, rhols_, ui, ur, iv, &liv, &lv, v, 
	    madrj_, ui, ur, madrj_);

/*  ***  REPEAT THE LAST RUN, BUT WITH A DIFFERENT INITIAL STEP BOUND */

/*  ***  FIRST CALL DIVSET TO GET DEFAULT IV AND V INPUT VALUES... */

    divset_(&c__1, iv, &liv, &lv, v);

/*  ***  NOW ASSIGN THE NONDEFAULT VALUES. */

    v[34] = .1;
    x[0] = 3.;
    x[1] = 1.;

    io___13.ciunit = nout;
    s_wsfe(&io___13);
    e_wsfe();

    dglfb_(&c__3, &c__2, &c__2, x, b, rhols_, ui, ur, iv, &liv, &lv, v, 
	    madrj_, ui, ur, madrj_);

    s_stop("", (ftnlen)0);
} /* MAIN__ */

/* *********************************************************************** */

/*     MADRJ */

/* *********************************************************************** */
/* Subroutine */ int madrj_(n, p, x, nf, need, r__, rp, ui, ur, uf)
integer *n, *p;
doublereal *x;
integer *nf, *need;
doublereal *r__, *rp;
integer *ui;
doublereal *ur;
/* Subroutine */ int (*uf) ();
{
    /* System generated locals */
    integer rp_dim1, rp_offset;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sin(), cos();


/* *** BODY *** */

    /* Parameter adjustments */
    --r__;
    rp_dim1 = *p;
    rp_offset = 1 + rp_dim1 * 1;
    rp -= rp_offset;
    --x;
    --ui;
    --ur;

    /* Function Body */
    if (*need == 2) {
	goto L10;
    }
/* Computing 2nd power */
    d__1 = x[1];
/* Computing 2nd power */
    d__2 = x[2];
    r__[1] = d__1 * d__1 + d__2 * d__2 + x[1] * x[2];
    r__[2] = sin(x[1]);
    r__[3] = cos(x[2]);
    goto L999;

L10:
    rp[rp_dim1 + 1] = x[1] * 2. + x[2];
    rp[rp_dim1 + 2] = x[2] * 2. + x[1];
    rp[(rp_dim1 << 1) + 1] = cos(x[1]);
    rp[(rp_dim1 << 1) + 2] = 0.;
    rp[rp_dim1 * 3 + 1] = 0.;
    rp[rp_dim1 * 3 + 2] = -sin(x[2]);

L999:
    return 0;
} /* madrj_ */

/* Subroutine */ int rhols_(need, f, n, nf, xn, r__, rp, ui, ur, w)
integer *need;
doublereal *f;
integer *n, *nf;
doublereal *xn, *r__, *rp;
integer *ui;
doublereal *ur, *w;
{
    /* Initialized data */

    static doublereal half = .5;
    static doublereal one = 1.;
    static doublereal rlimit = 0.;
    static doublereal zero = 0.;

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    static integer i__;
    extern doublereal dr7mdc_(), dv2nrm_();


/* *** LEAST-SQUARES RHO *** */


/* *** EXTERNAL FUNCTIONS *** */


/* *** LOCAL VARIABLES *** */

    /* Parameter adjustments */
    --need;
    --w;
    --rp;
    --r__;
    --xn;
    --ui;
    --ur;

    /* Function Body */

/* *** BODY *** */

    if (need[1] == 2) {
	goto L20;
    }
    if (rlimit <= zero) {
	rlimit = dr7mdc_(&c__5);
    }
/*     ** SET F TO 2-NORM OF R ** */
    *f = dv2nrm_(n, &r__[1]);
    if (*f >= rlimit) {
	goto L10;
    }
/* Computing 2nd power */
    d__1 = *f;
    *f = half * (d__1 * d__1);
    goto L999;

/*     ** COME HERE IF F WOULD OVERFLOW... */
L10:
    *nf = 0;
    goto L999;

L20:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	rp[i__] = one;
	w[i__] = one;
/* L30: */
    }
L999:
    return 0;
/* *** LAST LINE OF RHOLS FOLLOWS *** */
} /* rhols_ */

