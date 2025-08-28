#include <math.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Applic.h>
#include <R_ext/Utils.h>

void vecintalloc (int **vec, int n)
/*--------------------------------------------------
 * Memory allocation for an integer vector of length  n
 --------------------------------------------------*/
{
    if ( (*vec = (int *) calloc(n+1, sizeof(int))) != NULL) {
	**vec = n;
	return;
    } else {
	return;
    }
}


void resolpol(double a, double b, double c, double *x1, double *x2, int *warn)
{
    double delta;
    delta = (b * b) - 4 * a * c;
    *warn = 0;
    if (delta > 0) {
	*x1 = (-b - sqrt(delta)) / (2 * a);
	*x2 = (-b + sqrt(delta)) / (2 * a);
    } else {
	*warn = 1;
    }
}

void freeintvec (int *vec)
/*--------------------------------------------------
* Free memory for an integer  vector
--------------------------------------------------*/
{
    
    free((char *) vec);
    
}


void vecalloc (double **vec, int n)
/*--------------------------------------------------
 * Memory Allocation for a vector of length n
 --------------------------------------------------*/
{
    if ( (*vec = (double *) calloc(n+1, sizeof(double))) != 0) {
	**vec = n;
	return;
    } else {
	return;
    }
}


void freevec (double *vec)
/*--------------------------------------------------
 * Free memory for a vector
 --------------------------------------------------*/
{
    free((char *) vec);	
}



void discretraj(double *x, double *y, double *dat, double *xn, 
		double *yn, int n, int nn, double *datn, 
		double u, int *neff)
{
    /* Declaration */
    double R, xt, yt, a, b, c, pente, ori, x1, x2, di1, di2;
    int k, m, p, fini, ok, warn, *dedans, lo, new, pp;
    
    /* memory allocation */
    fini = 0;
    k = 1;
    p = 2;
    m = 1;
    ok = 0;
    a = 0;
    b = 0;
    c = 0;
    pente = 0;
    ori = 0;
    x1 = 0;
    x2 = 0;
    lo = 0;
    di1 = 0;
    di2 = 0;
    *neff = 0;
    new = 0;
    pp = 1;
    
    
    vecintalloc(&dedans,2);
    
    /* Main algorithm */
    while (fini == 0) {
	
	dedans[1] = 0;
	dedans[2] = 0;
	ok = 0;
	xt = xn[k];
	yt = yn[k];
	k++;
	new = 0;
	
	/* Determines the "upper" point */
	while (ok == 0) {
	    if (new == 1)
		p++;
	    R = sqrt((((x[p] - xt) * (x[p] - xt)) + ((y[p] - yt) * (y[p] - yt))));
	    if (R > u) {
		ok = 1;
	    } else {
		if (p == n) {
		    fini = 1;
		    ok = 1;
		}
	    }
	    new = 1;
	}
	m = p-1;
    
    if (fini == 0) {
	/* Does the difference between x[p] and x[m] = 0? */
	if ((fabs(x[p] - x[m]) > 0.000000000001)) {
	    /* Computes the slope between m and p */
	    pente = (y[p] - y[m]) / (x[p] - x[m]); /* when diff(x) == 0 ? */
	    /* The intercept */
	    ori = y[p] - (pente * x[p]);
	    /* The parameters of the polynomial equation */
	    a = 1 + (pente * pente);
	    b = (-2 * xt) + (2 * pente * ori) - (2 * pente * yt);
	    c = (xt * xt) + (yt * yt) + (ori * ori) - (2 * ori * yt) - (u * u);
	    resolpol(a, b, c, &x1, &x2, &warn);
	    /* 
	       A line cuts a circle with radius u at two points. One has 
	       (i) to identify the point the closest from m,n and 
	       (ii) to keep the one on the segment m-p
	    */
	    
	    
	    /* Which one are in the interval ? */
	    if (x1 >= x[m]) {
		if (x1 < x[p]) {
		    dedans[1] = 1;
		    lo = 1;
		}
	    }
	    if (x1 >= x[p]) {
		if (x1 < x[m]) {
		    dedans[1] = 1;
		    lo = 1;
		}
	    }
	    if (x2 >= x[m]) {
		if (x2 < x[p]) {
		    dedans[2] = 1;
		    lo = 2;
		}
	    }
	    if (x2 >= x[p]) {
		if (x2 < x[m]) {
		    dedans[2] = 1;
		    lo = 2;
		}
	    }
	    
	    /* What is the minimum distance to m ? */
	    if ((dedans[1] + dedans[2]) > 1) {
		di1 = fabs((double) (x[p] - x1));
		di2 = fabs((double) (x[p] - x2));
		
		/* verify that xk-1 is not in the same interval. Otherwise one increase of 1 */
		if (di1 < di2) {
		    lo = 2;
		} else {
		    lo = 1;
		}
		if (pp == p) {
		    if (di1 < di2) {
			lo = 1;
		    } 
		    if (di2 < di1) {
			lo = 2;
		    } 
		}
	    }
	    
	    /* storage of the coordinates */
	    if (lo == 1) {
		xn[k] = x1;
		yn[k] = (pente * x1) + ori;
	    }
	    if (lo == 2) {
		xn[k] = x2;
		yn[k] = (pente * x2) + ori;
	    }

	} else { /* We change x and y coordinates */
	    
	    /* Computes the slope between m and p */
	    pente =  (x[p] - x[m]) / (y[p] - y[m]);
	    /* The intercept */
	    ori = x[p] - (pente * y[p]);
	    /* The parameters of the polynomial equation */
	    a = 1 + (pente * pente);
	    b = (-2 * yt) + (2 * pente * ori) - (2 * pente * xt);
	    c = (xt * xt) + (yt * yt) + (ori * ori) - (2 * ori * xt) - (u * u);
	    resolpol(a, b, c, &x1, &x2, &warn);
	    /* 
	       A line cuts a circle with radius u at two points. One has 
	       (i) to identify the point the closest from m,n and 
	       (ii) to keep the one on the segment m-p
	    */
	    
	    
	    /* Which one are in the interval ? */
	    if (x1 >= y[m]) {
		if (x1 < y[p]) {
		    dedans[1] = 1;
		    lo = 1;
		}
	    }
	    if (x1 >= y[p]) {
		if (x1 < y[m]) {
		    dedans[1] = 1;
		    lo = 1;
		}
	    }
	    if (x2 >= y[m]) {
		if (x2 < y[p]) {
		    dedans[2] = 1;
		    lo = 2;
		}
	    }
	    if (x2 >= y[p]) {
		if (x2 < y[m]) {
		    dedans[2] = 1;
		    lo = 2;
		}
	    }
	    
	    /* What is the minimum distance to m ? */
	    if ((dedans[1] + dedans[2]) > 1) {
		di1 = fabs((double) (y[p] - x1));
		di2 = fabs((double) (y[p] - x2));
		
		/* verify that yk-1 is not in the same interval. Otherwise one increase of 1 */
		if (di1 < di2) {
		    lo = 2;
		} else {
		    lo = 1;
		}
		if (pp == p) {
		    if (di1 < di2) {
			lo = 1;
		    } 
		    if (di2 < di1) {
			lo = 2;
		    } 
		}
	    }
	    
	    /* storage of the coordinates */
	    if (lo == 1) {
		yn[k] = x1;
		xn[k] = (pente * x1) + ori;
	    }
	    if (lo == 2) {
		yn[k] = x2;
		xn[k] = (pente * x2) + ori;
	    }
	}
	
	/* Computes the nnew date (linear approximation) */
	di1 = sqrt((((xn[k] - x[m]) * (xn[k] - x[m])) + ((yn[k] - y[m]) * (yn[k] - y[m]))));
	R = sqrt((((x[p] - x[m]) * (x[p] - x[m])) + ((y[p] - y[m]) * (y[p] - y[m]))));
	di2 = dat[p] - dat[m];
	datn[k] = dat[m] + (di1 * di2 / R);
    }
    if (k == nn) {
	fini = 1;
    }
    pp = p;
    }
    
    /* Free memory */
    *neff = k;
    freeintvec(dedans);
}



/* For external Call from within R */

void discretrajr(double *xr, double *yr, double *datr, double *xnr, 
		 double *ynr, int *nr, int *nnr, double *datnr, 
		 double *xdeb, double *ydeb, double *ur, double *dat0, int *neff)
{
    /* Declaration */
    int i, n, nn;
    double *x, *y, *xn, *yn, *dat, *datn, u;
    
    /* Memory allocation */
    n = *nr;
    nn = *nnr;
    u = *ur;
    
    vecalloc(&x, n);
    vecalloc(&y, n);
    vecalloc(&xn, nn);
    vecalloc(&yn, nn);
    vecalloc(&dat, n);
    vecalloc(&datn, nn);
    
    /* R to C */
    for (i = 1; i <= n; i++) {
	x[i] = xr[i-1];
	y[i] = yr[i-1];
	dat[i] = datr[i-1];
    }
    
    xn[1] = *xdeb;
    yn[1] = *ydeb;
    datn[1] = *dat0;
    
    /* Main function  */
    discretraj(x, y, dat, xn, yn, n, nn, datn, u, neff);
    
    /* C to R */
    for (i = 1; i <= nn; i++) {
	xnr[i-1] = xn[i];
	ynr[i-1] = yn[i];
	datnr[i-1] = datn[i];
    }
    
    /* Free memory */
    freevec(x);
    freevec(y);
    freevec(xn);
    freevec(yn);
    freevec(dat);
    freevec(datn);
}



SEXP simtraj(SEXP G10, SEXP S10, SEXP G11, SEXP S11, SEXP Hv, SEXP Lv, SEXP idv, SEXP nv)
{
  int i, n, id, j, k;
  SEXP H, H2, sno, lires, v, z;
  double *Hr, *H2r, *G11r, *S11r, *G10r, *S10r, *snor, *Lr, *vr, *zr;
  
  id = INTEGER(idv)[0];
  n = INTEGER(nv)[0];
  PROTECT(H = allocVector(REALSXP, 4));
  PROTECT(H2 = allocVector(REALSXP, 4));
  PROTECT(sno = allocVector(REALSXP, 4));
  PROTECT(v = allocVector(REALSXP, 2*n));
  PROTECT(z = allocVector(REALSXP, 2*n));
  PROTECT(lires = allocVector(VECSXP, 2));
  G11r=REAL(G11);
  S11r=REAL(S11);
  S10r=REAL(S10);
  G10r=REAL(G10);
  Hr = REAL(H);
  H2r = REAL(H2);
  Lr = REAL(Lv);
  snor = REAL(sno);
  vr = REAL(v);
  zr = REAL(z);
  zr[0]=0;
  zr[1]=0;
  vr[0]=0;
  vr[1]=0;
  
  for (i=0; i<4; i++) {
    Hr[i] = REAL(Hv)[i];
    H2r[i] = REAL(Hv)[i];
  }
  
  GetRNGstate();
  k=id*2;
  for (i = id; i < n; i++) {
    for (j =0; j<4; j++) {
      snor[j] = norm_rand();
    }
    if (i==0) {
      H2r[0] = (G10r[0]*Hr[0]+G10r[2]*Hr[1]) + (S10r[0]*snor[0]+S10r[2]*snor[1]);
      H2r[1] = (G10r[1]*Hr[0]+G10r[3]*Hr[1]) + (S10r[1]*snor[0]+S10r[3]*snor[1]);
      H2r[2] = (G10r[0]*Hr[2]+G10r[2]*Hr[3]) + (S10r[0]*snor[2]+S10r[2]*snor[3]);
      H2r[3] = (G10r[1]*Hr[2]+G10r[3]*Hr[3]) + (S10r[1]*snor[2]+S10r[3]*snor[3]);
    }
    if (i!=0) {
      H2r[0] = (G11r[0]*Hr[0]+G11r[2]*Hr[1]) + (S11r[0]*snor[0]+S11r[2]*snor[1]);
      H2r[1] = (G11r[1]*Hr[0]+G11r[3]*Hr[1]) + (S11r[1]*snor[0]+S11r[3]*snor[1]);
      H2r[2] = (G11r[0]*Hr[2]+G11r[2]*Hr[3]) + (S11r[0]*snor[2]+S11r[2]*snor[3]);
      H2r[3] = (G11r[1]*Hr[2]+G11r[3]*Hr[3]) + (S11r[1]*snor[2]+S11r[3]*snor[3]);
    }
    for (j=0;j<4;j++) {
      Hr[j] = H2r[j];
    }
    zr[k] = Hr[0]*Lr[0]+Hr[2]*Lr[1];
    zr[k+1] = Hr[0]*Lr[2]+Hr[2]*Lr[3];
    vr[k] = Hr[1];
    vr[k+1] = Hr[3];
    k = k+2;
  }
  
  PutRNGstate();

  SET_VECTOR_ELT(lires, 0, z);
  SET_VECTOR_ELT(lires, 1, v);

  UNPROTECT(6);

  return(lires);
  
}


