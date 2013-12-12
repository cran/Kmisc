#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

// [[export]]
SEXP any_na( SEXP x ) {
	SEXP out;
	PROTECT(out = allocVector(LGLSXP, 1));
	int len = length(x);
	switch( TYPEOF(x) ) {
	case REALSXP: {
		double* ptr = REAL(x);
		for( int i=0; i < len; ++i ) {
			if( ISNA( ptr[i] ) || ISNAN( ptr[i] ) ) {
				LOGICAL(out)[0] = TRUE;
				UNPROTECT(1);
				return out;
			}
		}
		LOGICAL(out)[0] = FALSE;
		UNPROTECT(1);
		return out;
	}
	case INTSXP: {
		int* ptr = INTEGER(x);
		for( int i=0; i < len; ++i ) {
			if( ptr[i] == NA_INTEGER ) {
				LOGICAL(out)[0] = TRUE;
				UNPROTECT(1);
				return out;
			}
		}
		LOGICAL(out)[0] = FALSE;
		UNPROTECT(1);
		return out;
	}
	case LGLSXP: {
		int* ptr = LOGICAL(x);
		for( int i=0; i < len; ++i ) {
			if( ptr[i] == NA_LOGICAL ) {
				LOGICAL(out)[0] = TRUE;
				UNPROTECT(1);
				return out;
			}
		}
		LOGICAL(out)[0] = FALSE;
		UNPROTECT(1);
		return out;
	}
	case STRSXP: {
		for( int i=0; i < len; ++i ) {
			if( STRING_ELT(x, i) == NA_STRING ) {
				LOGICAL(out)[0] = TRUE;
				UNPROTECT(1);
				return out;
			}
		}
		LOGICAL(out)[0] = FALSE;
		UNPROTECT(1);
		return out;
	}
	}
	error("argument is of incompatible type '%s'", type2char( TYPEOF(x) ) );
	return x;
}

#undef USE_RINTERNALS

#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

// [[export]]
SEXP charlist_transpose_to_df( SEXP x, SEXP names ) {
  
  if( TYPEOF(x) != VECSXP ) {
    error("argument must be a list; type is '%s'", type2char( TYPEOF(x)));
  }
  
  int out_nRow = length(x);
	int out_nCol = length( VECTOR_ELT(x, 0) );
  for (int i=0; i < out_nRow; ++i) {
    if (length( VECTOR_ELT(x, i)) != out_nCol) {
      error("each column of 'x' must be of equal length");
    }
  }

	SEXP out = PROTECT( allocVector( VECSXP, out_nCol ) );

	for( int j=0; j < out_nCol; ++j ) {
		SEXP tmp = PROTECT( allocVector( STRSXP, out_nRow ) );
		for( int i=0; i < out_nRow; ++i ) {
			SET_STRING_ELT( tmp, i, STRING_ELT( VECTOR_ELT( x, i ), j ) );
		}
		SET_VECTOR_ELT( out, j, tmp );
		UNPROTECT(1);
	}

	SEXP row_names = PROTECT( allocVector( INTSXP, out_nRow ) );
	int* row_names_ptr = INTEGER(row_names);
	for( int i=0; i < out_nRow; ++i ) {
		row_names_ptr[i] = i+1;
	}

	setAttrib(out, R_ClassSymbol, mkString("data.frame"));
	setAttrib(out, R_RowNamesSymbol, row_names);
  
  // make the names
#define m out_nCol
  if (isNull(names)) {
    SEXP nm = PROTECT( allocVector(STRSXP, out_nCol) );
    char str[ (int) log10(m) + 3];
    for (int i = 0; i < m; ++i) {
			sprintf(str, "%s%i", "V", i + 1);
			SET_STRING_ELT(nm, i, mkChar(str));
		}
		setAttrib(out, R_NamesSymbol, nm);
    UNPROTECT(1);
	} else {
    setAttrib(out, R_NamesSymbol, names);
	}
#undef m
  

	UNPROTECT(2);
	return out;
}

#undef USE_RINTERNALS

#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

// [[export]]
SEXP copy(SEXP x_) {
  SEXP x = PROTECT( duplicate(x_) );
  UNPROTECT(1);
  return x;
}

#undef USE_RINTERNALS

#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

SEXP recurse_factor_to_char( SEXP X, SEXP parent, int i ) {

  if( TYPEOF(X) == VECSXP ) {
    for( int j=0; j < length(X); ++j ) {
      recurse_factor_to_char( VECTOR_ELT(X, j), X, j );
    }
  } else {
    if( isFactor(X) ) {
      SET_VECTOR_ELT( parent, i, asCharacterFactor(X) );
    }
  }
  return X;

}

// [[export]]
SEXP factor_to_char( SEXP X_, SEXP inplace_ ) {
  
  int inplace = asInteger(inplace_);
  int numprotect = 0;
  SEXP X;
  if (inplace) {
    X = X_;
  } else {
    PROTECT( X = duplicate(X_) );
    ++numprotect;
  }
  if( TYPEOF(X) == VECSXP ) {
    SEXP out = recurse_factor_to_char( X, X, 0);
    UNPROTECT(numprotect);
    return out;
  } else {
    if( isFactor(X) ) {
      SEXP out = asCharacterFactor(X);
      UNPROTECT(numprotect);
      return out;
    } else {
      warning("X is neither a list nor a factor; no change done");
      UNPROTECT(numprotect);
      return X;
    }
  }
}

#undef USE_RINTERNALS

#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

// [[export]]
SEXP in_interval( SEXP x, SEXP lo, SEXP hi, 
        SEXP include_lower, SEXP include_upper ) {
  
  int len = Rf_length(x);
  double lower = REAL(lo)[0], upper = REAL(hi)[0], *xp = REAL(x);
  
  int inc_lower = asLogical(include_lower);
  int inc_upper = asLogical(include_upper);
  
  SEXP out = PROTECT( allocVector( LGLSXP, len ) );
  int *outp = LOGICAL(out);
  
  if( inc_lower == 1 && inc_upper == 1 ) {
      for( int i=0; i < len; ++i ) {
        outp[i] = xp[i] >= lower && xp[i] <= upper;
        }
  }
  
  if( inc_lower == 1 && inc_upper == 0 ) {
      for( int i=0; i < len; ++i ) {
        outp[i] = xp[i] >= lower && xp[i] < upper;
        }
  }
  
  if( inc_lower == 0 && inc_upper == 1 ) {
      for( int i=0; i < len; ++i ) {
        outp[i] = xp[i] > lower && xp[i] <= upper;
        }
  }
  
  if( inc_lower == 0 && inc_upper == 0 ) {
      for( int i=0; i < len; ++i ) {
        outp[i] = xp[i] > lower && xp[i] < upper;
        }
  }
  
  UNPROTECT(1);
  return out;
  
}

#undef USE_RINTERNALS

#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>

// [[export]]
SEXP list2df(SEXP x_, SEXP inplace) {

	SEXP x;
	int unprotect_num = 1;

	if (TYPEOF(inplace) != LGLSXP || length(inplace) > 1) {
		error("'inplace' must be a logical vector of length 1; type is '%s'",
		type2char(TYPEOF(inplace)));
	}

	if (LOGICAL(inplace)[0] < 0) {
		error("'inplace' must be non-NA");
	}

	if (TYPEOF(x_) != VECSXP)
		error("argument must be a list; type is '%s'", type2char(TYPEOF(x_)));

	if (LOGICAL(inplace)[0]) {
		x = x_;
	} else {
		x = PROTECT( duplicate(x_) );
		++unprotect_num;
	}

	int m = length(x);
	int n = length( VECTOR_ELT(x, 0) );
	for (int i = 1; i < m; ++i) {
		if (length( VECTOR_ELT(x, i) ) != n) {
			error("not all columns are of equal length");
		}
	}

	SEXP row_names = PROTECT( allocVector( INTSXP, n ) );
	for (int i = 0; i < n; ++i)
		INTEGER(row_names)[i] = i + 1;

	setAttrib(x, R_ClassSymbol, mkString("data.frame"));
	setAttrib(x, R_RowNamesSymbol, row_names);

	// set the names, if NULL
	if (isNull( getAttrib(x, R_NamesSymbol) )) {
		SEXP colnames;
		PROTECT(colnames = allocVector(STRSXP, m));
		++unprotect_num;
		char str[ (int) log10(m) + 3];
    for (int i = 0; i < m; ++i) {
			sprintf(str, "%s%i", "V", i + 1);
			SET_STRING_ELT(colnames, i, mkChar(str));
		}
		setAttrib(x, R_NamesSymbol, colnames);

	}

	UNPROTECT(unprotect_num);
	return x;

}

#undef USE_RINTERNALS

#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

void set_rownames(SEXP x);
void set_names(SEXP x);

// [[export]]
SEXP mat2df(SEXP x) {
  char type = TYPEOF(x);
  if (!isMatrix(x))
    error("'x' must be a matrix");
  int nRow = INTEGER(getAttrib(x, R_DimSymbol))[0];
  int nCol = INTEGER(getAttrib(x, R_DimSymbol))[1];
  SEXP output = PROTECT( allocVector(VECSXP, nCol) );
  
  #define HANDLE_CASE(ACCESSOR, CTYPE) { \
  for (int i=0; i < nCol; ++i) { \
    SET_VECTOR_ELT(output, i, allocVector(type, nRow)); \
    CTYPE* output_ptr = ACCESSOR(VECTOR_ELT(output, i)); \
    CTYPE* x_ptr = ACCESSOR(x); \
    for (int j=0; j < nRow; ++j) { \
      output_ptr[j] = x_ptr[nRow*i + j]; \
    } \
  } \
  break; \
  } \
  
  #define HANDLE_CASE_MEMCPY(ACCESSOR, CTYPE) { \
  for (int i=0; i < nCol; ++i) { \
    char sz = sizeof(CTYPE); \
    SET_VECTOR_ELT(output, i, allocVector(type, nRow)); \
    SEXP elt = VECTOR_ELT(output, i); \
    memcpy( \
      (char*) DATAPTR(elt), \
      (char*) DATAPTR(x) + (i*nRow*sz), \
      nRow*sz \
    ); \
  } \
  break; \
  } \
  
  switch (type) {
    case INTSXP: HANDLE_CASE_MEMCPY(INTEGER, int);
    case REALSXP: HANDLE_CASE_MEMCPY(REAL, double);
    case LGLSXP: HANDLE_CASE_MEMCPY(INTEGER, int);
    case STRSXP: HANDLE_CASE(STRING_PTR, SEXP);
    default: error("Unhandled SEXP type '%s'", type2char(type));
  }
  
  #undef HANDLE_CASE
  #undef HANDLE_CASE_MEMCPY
  
  SEXP dimnames = PROTECT(getAttrib(x, R_DimNamesSymbol));
  if (isNull(dimnames)) {
    
    set_rownames(output);
    set_names(output);
      
  } else {
    
    if (!isNull(VECTOR_ELT(dimnames, 0)))
      setAttrib(output, R_RowNamesSymbol, VECTOR_ELT(dimnames, 1));
    else
      set_rownames(output);
    
    if (!isNull(VECTOR_ELT(dimnames, 1)))
      setAttrib(output, R_NamesSymbol, VECTOR_ELT(dimnames, 1));
    else
      set_names(output);
    
  }
  
  setAttrib(output, R_ClassSymbol, mkString("data.frame"));
  
  UNPROTECT(2);
  return output;
  
}

#undef USE_RINTERNALS

#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>

// utils.c
char max_type(SEXP, SEXP);

// a function that operates like R's 'rep(..., each=each)',
// but only works for characters
SEXP rep_each_char( SEXP x, SEXP id_ind_, int each ) {

	SEXP out;
  int* id_ind = INTEGER(id_ind_);
	int len = length(id_ind_);
	PROTECT( out = allocVector( STRSXP, len*each ) );
	int counter=0;
	SEXP* ptr = STRING_PTR(x);
	SEXP* out_ptr = STRING_PTR(out);
	for( int i=0; i < len; ++i ) {
		for( int j=0; j < each; ++j ) {
			out_ptr[counter] = ptr[ id_ind[i] ];
			++counter;
		}
	}
	UNPROTECT(1);
	return out;
}

// some macro definitions used for repeating a vector 
// n times

// simply using memcpy with strings is dangerous
#define HANDLE_CASE_STRING \
  	case STRSXP: { \
    int counter = 0; \
		PROTECT( out = allocVector( STRSXP, len*times ) ); \
		SEXP* ptr = STRING_PTR(x); \
		SEXP* out_ptr = STRING_PTR(out); \
		for( int i=0; i < times; ++i ) { \
			for( int j=0; j < len; ++j ) { \
				out_ptr[counter] = ptr[j]; \
				++counter; \
			} \
		} \
		UNPROTECT(1); \
		return out; \
		} \

#define HANDLE_CASE(RTYPE, CTYPE) \
  case RTYPE: { \
  char sz = sizeof(CTYPE); \
  PROTECT(out = allocVector(RTYPE, len*times)); \
  for (int i=0; i < times; ++i) { \
    memcpy( \
      (char*) DATAPTR(out) + (i*len*sz), \
      (char*) DATAPTR(x), \
      len*sz \
    ); \
  } \
  UNPROTECT(1); \
  return out; \
  } \

SEXP stack_vector( SEXP x, int times ) {
	SEXP out;
	int len = length(x);
	switch( TYPEOF(x) ) {
	HANDLE_CASE( INTSXP, int );
	HANDLE_CASE( REALSXP, double );
	HANDLE_CASE( LGLSXP, int );
	HANDLE_CASE_STRING;
	}
  
  // if we've reached here, we have an unhandled / incompatible SEXP type
	error("argument is of incompatible type '%s'", type2char(TYPEOF(x)));
	return R_NilValue;
}

#undef HANDLE_CASE

// checks if all values in a VECSXP x are of the same type
bool diff_types(SEXP x, SEXP val_ind_) {
  if (TYPEOF(x) != VECSXP) {
    error("Expected a VECSXP but got a '%s'", type2char(TYPEOF(x)));
  }
  int n = length(val_ind_);
  int* val_ind = INTEGER(val_ind_);
  char type = TYPEOF( VECTOR_ELT(x, val_ind[0]) );
  for (int i=1; i < n; ++i) {
    if (TYPEOF( VECTOR_ELT(x, val_ind[i]) ) != type) {
      return true;
    }
  }
  return false;
}

// [[export]]
SEXP melt_dataframe( SEXP x, SEXP id_ind_, SEXP val_ind_, SEXP variable_name, SEXP value_name ) {
  
  if (length(x) == 0) {
    error("Can't melt a data.frame with 0 columns");
  }
  
  if (length(VECTOR_ELT(x, 0)) == 0) {
    error("Can't melt a data.frame with 0 rows");
  }
  
  int* id_ind = INTEGER(id_ind_);
  int* val_ind = INTEGER(val_ind_);
  
  int nColStack = length(id_ind_);
	int nColRep = length(val_ind_);
  
  int nRow = length( VECTOR_ELT(x, 0) );
	int out_nRow = nRow * nColRep;
	int out_nCol = nColStack + 2;
  
  char mt = max_type(x, val_ind_);
  if (mt > STRSXP) {
    error("Error: cannot melt data.frames w/ elements of type '%s'", CHAR(type2str(mt)));
  }
  
  if (diff_types(x, val_ind_)) {
    warning("Coercing type of 'value' variables to '%s'", CHAR(type2str(mt)));
  }
  
  SEXP out;
	PROTECT(out = allocVector( VECSXP, out_nCol ));

	// populate the value array
	SEXP value_SEXP;

#define HANDLE_CASE( RTYPE, CTYPE ) \
		case RTYPE: { \
      PROTECT( value_SEXP = allocVector( RTYPE, value_len ) ); \
      SEXP tmp; \
			for( int i=0; i < nColRep; ++i ) { \
        if (TYPEOF( VECTOR_ELT(x, val_ind[i]) ) != mt) { \
          tmp = PROTECT( coerceVector( VECTOR_ELT(x, val_ind[i]), mt ) ); \
        } else { \
          tmp = VECTOR_ELT(x, val_ind[i]); \
        } \
        memcpy( \
          (char*) DATAPTR(value_SEXP) + (i*nRow*sizeof(CTYPE)), \
          (char*) DATAPTR(tmp), \
          nRow * sizeof(CTYPE) \
        ); \
        if (TYPEOF( VECTOR_ELT(x, val_ind[i]) ) != mt) { \
          UNPROTECT(1); \
        } \
			} \
			break; \
		} \


	int value_len = nColRep * nRow;
	int value_type = mt;
  switch( value_type ) {
	HANDLE_CASE( INTSXP, int );
	HANDLE_CASE( REALSXP, double );
	HANDLE_CASE( LGLSXP, int );
	case STRSXP: {
    int counter = 0;
    SEXP* curr_str_vec_ptr;
    SEXP tmp;
		PROTECT( value_SEXP = allocVector( STRSXP, value_len ) );
		for( int i=0; i < nColRep; ++i ) {
#define curr_str_vec (VECTOR_ELT(x, val_ind[i]))
      if (TYPEOF(curr_str_vec) != STRSXP) {
        if (isFactor(curr_str_vec)) {
          PROTECT(tmp = asCharacterFactor(curr_str_vec));
        } else {
          PROTECT(tmp = coerceVector(curr_str_vec, STRSXP));
        }
        curr_str_vec_ptr = STRING_PTR(tmp);
      } else {
        curr_str_vec_ptr = STRING_PTR(curr_str_vec);
      }
#undef curr_str_vec
			SEXP* value_SEXP_ptr = STRING_PTR( value_SEXP );
			for( int j=0; j < nRow; ++j ) {
				value_SEXP_ptr[counter] = curr_str_vec_ptr[j];
				++counter;
			}
      if (TYPEOF( VECTOR_ELT(x, val_ind[i]) ) != mt) {
        UNPROTECT(1);
      }
		}
		break;
	}
	default:
		error("Unsupported RTYPE encountered");
	}
  
#undef HANDLE_CASE

	// generate the id variables, and assign them on generation
  // we need to convert factors if necessary
	for( int i=0; i < nColStack; ++i ) {
		SET_VECTOR_ELT( out, i, stack_vector( VECTOR_ELT( x, id_ind[i] ), nColRep ));
    if (isFactor( VECTOR_ELT(x, id_ind[i]) )) {
      setAttrib( VECTOR_ELT(out, i), R_ClassSymbol, mkString("factor") );
      setAttrib( VECTOR_ELT(out, i), R_LevelsSymbol, getAttrib( VECTOR_ELT(x, id_ind[i]), R_LevelsSymbol ) );
    }
	}

	// assign the names, values
	SET_VECTOR_ELT( out, nColStack, rep_each_char( getAttrib( x, R_NamesSymbol ), val_ind_, nRow ) );
  SET_VECTOR_ELT( out, nColStack+1, value_SEXP );
	UNPROTECT(1); // value_SEXP

	// set the row names
	SEXP row_names;
	PROTECT( row_names = allocVector(INTSXP, out_nRow) );
	int* row_names_ptr = INTEGER(row_names);
	for( int i=0; i < out_nRow; ++i ) {
		row_names_ptr[i] = i+1;
	}
	setAttrib( out, R_RowNamesSymbol, row_names );
	UNPROTECT(1); // row_names

	// set the class to data.frame
	setAttrib(out, R_ClassSymbol, mkString("data.frame"));

	// set the names
	SEXP names = getAttrib(x, R_NamesSymbol);
	SEXP names_out;
	PROTECT(names_out = allocVector( STRSXP, out_nCol ));
  
  SEXP* names_ptr = STRING_PTR(names);
  SEXP* names_out_ptr = STRING_PTR(names_out);
  for (int i=0; i < nColStack; ++i) {
    names_out_ptr[i] = names_ptr[ id_ind[i] ];
  }
	
  SET_STRING_ELT( names_out, nColStack, STRING_ELT(variable_name, 0) );
	SET_STRING_ELT( names_out, nColStack+1, STRING_ELT(value_name, 0) );
	setAttrib( out, R_NamesSymbol, names_out );
	UNPROTECT(1); // names_out

	UNPROTECT(1); // out
  return out;

}

#undef USE_RINTERNALS

#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

SEXP rep_row_names( SEXP x, int times ) {
	SEXP out;
	int len = length(x);
	int counter = 0;
	PROTECT( out = allocVector( STRSXP, len*times ) );
	SEXP* x_ptr = STRING_PTR(x);
	SEXP* out_ptr = STRING_PTR(out);
	for( int i=0; i < times; ++i ) {
		for( int j=0; j < len; ++j ) {
			out_ptr[counter] = x_ptr[j];
			//SET_STRING_ELT( out, counter, STRING_ELT(x, j) );
			++counter;
		}
	}
	UNPROTECT(1);
	return out;
}
SEXP rep_col_names( SEXP x, int each ) {
	SEXP out;
	int len = length(x);
	PROTECT( out = allocVector( STRSXP, len*each ) );
	int counter=0;
	SEXP* ptr = STRING_PTR(x);
	SEXP* out_ptr = STRING_PTR(out);
	for( int i=0; i < len; ++i ) {
		for( int j=0; j < each; j++ ) {
			out_ptr[counter] = ptr[i];
			//SET_STRING_ELT( out, counter, ptr[i] );
			++counter;
		}
	}
	UNPROTECT(1);
	return out;
}

SEXP matrix_to_vector( SEXP x, int size ) {

	SEXP out;
	switch( TYPEOF(x) ) {
	case INTSXP: {
		PROTECT( out = allocVector(INTSXP, size) );
		int* mat_ptr = INTEGER(x);
		int* out_ptr = INTEGER(out);
		for( int i=0; i < size; ++i ) {
			out_ptr[i] = mat_ptr[i];
		}
		UNPROTECT(1);
		return out;
	}
	case REALSXP: {
		PROTECT( out = allocVector(REALSXP, size) );
		double* mat_ptr = REAL(x);
		double* out_ptr = REAL(out);
		for( int i=0; i < size; ++i ) {
			out_ptr[i] = mat_ptr[i];
		}
		UNPROTECT(1);
		return out;
	}
	case LGLSXP: {
		PROTECT( out = allocVector(LGLSXP, size) );
		int* mat_ptr = LOGICAL(x);
		int* out_ptr = LOGICAL(out);
		for( int i=0; i < size; ++i ) {
			out_ptr[i] = mat_ptr[i];
		}
		UNPROTECT(1);
		return out;
	}
	case STRSXP: {
		PROTECT( out = allocVector( STRSXP, size ) );
		SEXP* mat_ptr = STRING_PTR(x);
		SEXP* out_ptr = STRING_PTR(out);
		for( int i=0; i < size; ++i ) {
			out_ptr[i] = mat_ptr[i];
		}
		UNPROTECT(1);
		return out;
	}
	default: {
		return R_NilValue;
	}
	}

}

// [[export]]
SEXP melt_matrix( SEXP x ) {

	SEXP row, col, out, out_row_names;

	int nRow = nrows(x);
	int nCol = ncols(x);
	int out_nRow = nRow*nCol;
	int out_nCol = 3;

	// the output will be a 3 column data.frame
	// 1: repeated row names / indexes
	// 2: repeated col names / indexes
	// 3: values

	SEXP row_names, col_names;
	const char* row_names_char;
	const char* col_names_char;
	GetMatrixDimnames(x, &row_names, &col_names, &row_names_char, &col_names_char);

	PROTECT( out = allocVector( VECSXP, 3 ) );
	int counter;

	// row indices
	if( isNull(row_names) ) {
		PROTECT( row = allocVector( INTSXP, out_nRow ) );
		int* row_ptr = INTEGER(row);
		counter = 0;
		for( int i=0; i < nCol; ++i ) {
			for( int j=0; j < nRow; ++j ) {
				row_ptr[counter] = j+1;
				++counter;
			}
		}
	} else {
		PROTECT( row = rep_row_names(row_names, nCol) );
	}

	// col indices
	if( isNull(col_names) ) {
		PROTECT( col = allocVector( INTSXP, out_nRow ) );
		int* col_ptr = INTEGER(col);
		counter = 0;
		for( int i=0; i < nCol; ++i ) {
			for( int j=0; j < nRow; ++j ) {
				col_ptr[counter] = i+1;
				++counter;
			}
		}
	} else {
		PROTECT( col = rep_col_names(col_names, nRow) );
	}

	// dim_names is a list; 1st entry is row names, 2nd is col names
	SET_VECTOR_ELT( out, 0, row );
	SET_VECTOR_ELT( out, 1, col );
	SET_VECTOR_ELT( out, 2, matrix_to_vector(x, out_nRow) );

	// set row names
	PROTECT( out_row_names = allocVector( INTSXP, out_nRow) );
	int* row_names_ptr = INTEGER(out_row_names);
	for( int i=0; i < out_nRow; ++i ) {
		row_names_ptr[i] = i+1;
	}
	setAttrib( out, R_RowNamesSymbol, out_row_names );
	UNPROTECT(1);

	// set class
	setAttrib( out, R_ClassSymbol, mkString("data.frame") );

	// set names
	SEXP names;
	PROTECT( names = allocVector( STRSXP, out_nCol ) );
	SET_STRING_ELT( names, 0, mkChar("row") );
	SET_STRING_ELT( names, 1, mkChar("col") );
	SET_STRING_ELT( names, 2, mkChar("value") );
	setAttrib( out, R_NamesSymbol, names );
	UNPROTECT(1);

	// unprotect the rest of the stuff from earlier
	UNPROTECT(3);
	return out;

}

#undef USE_RINTERNALS

#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

// [[export]]
SEXP simp( SEXP x, SEXP y ) {

	SEXP out;
	PROTECT( out = allocVector(REALSXP, 1) );

	int nx = length(x);
	int ny = length(y);

	if( nx != ny ) {
		Rf_error("'x' must be the same length as 'y'");
	}

	double out_num = 0;
	double mult = 1;

	for( int i=0; i < nx; ++i ) {

		// get the correct multiplier
		if( i == 0 ) {
			mult = 1.0;
		} else if( i == nx-1 ) {
			mult = 1.0;
		} else if( i % 2 == 1 ) {
			mult = 4.0;
		} else if( i % 2 == 0 ) {
			mult = 2.0;
		}

		out_num = out_num + (mult * REAL(y)[i]);
		// Rprintf("i = %i; REAL(y)[i] = %f; mult = %f; out_num = %f\n", i, REAL(y)[i], mult, out_num);
	}

	double h = (REAL(x)[nx-1] - REAL(x)[0]) / (double) nx;
	// Rprintf("h = %f\n", h);

	out_num = (h / 3.0) * out_num;
	// Rprintf("the final value of out_num is %f\n", out_num);

	REAL(out)[0] = out_num;
	UNPROTECT(1);
	return out;

}

#undef USE_RINTERNALS

#define USE_RINTERNALS

#include <R.h>
#include <Rdefines.h>

// [[export]]
SEXP str_rev( SEXP x ) {
  
  int len = length(x);
  SEXP out;
  PROTECT( out = allocVector( STRSXP, len ) );
  
  // Loop through each string
  for( int i=0; i < len; ++i ) {
    
    // Get the current element of the string
    int len_elt = length( STRING_ELT(x, i) );
    const char* element = CHAR( STRING_ELT(x, i) );
    
    // Allocate space for the reversed string
    char* elt_rev = R_alloc( len_elt+1, sizeof(char) );
    
    // Reverse 'elt'
    for( int j=0; j < len_elt; ++j ) {
      elt_rev[j] = element[ len_elt - j - 1];
    }
    
    // Set the null terminator
    elt_rev[len_elt] = '\0';
    
    // Set the i'th element of out to the reversed char
    SET_STRING_ELT( out, i, mkChar( elt_rev ) );
    
  }
    
    UNPROTECT(1);
    return out;
}

#undef USE_RINTERNALS

#define USE_RINTERNALS

#include <R.h>
#include <Rdefines.h>

// [[export]]
SEXP str_slice(SEXP x, SEXP n) {
    
    // Treat x as a vector of characters
    int x_len = length(x);
    int len_substr = INTEGER(n)[0];
    
    // Allocate memory for a list
    SEXP out;
    PROTECT( out = allocVector(VECSXP, x_len) );
    
    for( int k=0; k < x_len; ++k ) {
        
        // The string as a pointer to an array of characters
        const char* xx = CHAR(STRING_ELT( x, k ) );

        // The length of the string supplied
        int len = length( STRING_ELT( x, k ) );

        // The number of substrings
        int num_substr = len / len_substr;
        
        // Allocate memory for the vector of substrings
        SEXP substring;
        PROTECT( substring = allocVector(STRSXP, num_substr) );

        int string_counter = 0;
        for( int i=0; i < num_substr; ++i ) {

            // allocate memory for a string
            char* elt = R_alloc( len_substr+1, sizeof(char)  );

            // Push items onto the element
            for( int j=0; j < len_substr; ++j ) {
                elt[j] = xx[string_counter];
                string_counter++;
            }

            // Set the terminator
            elt[len_substr] = '\0';

            SET_STRING_ELT( substring, i, mkChar(elt) );
        }
        
        // Set the list element to the substring
        SET_VECTOR_ELT(out, k, substring);
        UNPROTECT(1);
        
    }
    
    UNPROTECT(1);
    return( out );
    
}

#undef USE_RINTERNALS

#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>

// utils.c
char max_type1(SEXP);

// [[export]]
SEXP transpose_list(SEXP x_) {
  
  SEXP x;
  bool do_coerce = false;
  int n = length(x_);
  int N = length(VECTOR_ELT(x_, 0));
  char type = max_type1(x_);
  
  for (int i=0; i < n; ++i) {
    
    if (length(VECTOR_ELT(x_, i)) != N) {
      Rf_error("Each element in the list must be of the same length");
    }
    
  }
    
  for (int i=0; i < n; ++i) {    
    if (TYPEOF( VECTOR_ELT(x_, i) ) != type) {
      Rf_warning("Coercing vectors in the list to type '%s'", type2char(type));
      do_coerce = true;
      break;
    }
  }
  
  if (do_coerce) {
    x = PROTECT( duplicate(x_) );
    for (int i=0; i < n; ++i) {
      if (TYPEOF(VECTOR_ELT(x, i)) != type) {
        SET_VECTOR_ELT(x, i, coerceVector(VECTOR_ELT(x, i), type));
      }
    }
  } else {
    x = x_;
  }
  
  SEXP output = PROTECT( allocVector(VECSXP, N) );
  
  #define HANDLE_CASE(RTYPE, CTYPE, ACCESSOR) { \
    for (int j=0; j < N; ++j) { \
      SET_VECTOR_ELT(output, j, allocVector(RTYPE, n)); \
      CTYPE* ptr = ACCESSOR( VECTOR_ELT(output, j) ); \
      for (int i=0; i < n; ++i) { \
        ptr[i] = ACCESSOR( VECTOR_ELT(x, i) )[j]; \
      } \
    } \
    break; \
  } \
    
  switch (type) {
  case LGLSXP: HANDLE_CASE(LGLSXP, int, LOGICAL);
  case INTSXP: HANDLE_CASE(INTSXP, int, INTEGER);
  case REALSXP: HANDLE_CASE(REALSXP, double, REAL);
  case STRSXP: HANDLE_CASE(STRSXP, SEXP, STRING_PTR);
  }
  
  #undef HANDLE_CASE
  
  UNPROTECT(1);
  if (do_coerce) UNPROTECT(1);
  return output;
  
}

#undef USE_RINTERNALS

#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

#define GUARD(X) PROTECT(X); ++numprotect;
#define UNGUARD UNPROTECT(numprotect)

// [[export]]
SEXP unmelt(SEXP data, SEXP uniq_id, SEXP other_ind_, SEXP id_ind_, SEXP value_ind_) {

	// int id_ind = asInteger(id_ind_);
	int value_ind = asInteger(value_ind_);
	int* other_ind = INTEGER(other_ind_);
	int nRow = (int)(length(VECTOR_ELT(data, 0)) / length(uniq_id));
	int numprotect = 0;

	if (TYPEOF(uniq_id) != STRSXP) {
		GUARD(uniq_id = coerceVector(uniq_id, STRSXP));
	}

	int n_uniq = length(uniq_id);

	SEXP output;
	GUARD(output = allocVector(VECSXP, length(other_ind_) + length(uniq_id)));

	int n_other = length(other_ind_);

	// ensure that the unmelting process will go smoothly
#define HANDLE_CASE(RTYPE, CTYPE, ACCESSOR) \
		case RTYPE: { \
			CTYPE* tmp = ACCESSOR( VECTOR_ELT(data, other_ind[i]) ); \
			for (int j=0; j < nRow; ++j) { \
				for (int k=1; k < n_uniq; ++k) { \
					if (tmp[j] != tmp[j + nRow*k]) { \
						Rf_error("Mismatch in elements at indices %i and %i in vector %s", j+1, j + nRow*k+1, CHAR(STRING_ELT(getAttrib(data, R_NamesSymbol), other_ind[i]))); \
					} \
				} \
			} \
			break; \
		} \


	if (n_uniq > 1) {
		for (int i=0; i < n_other; ++i) {
			switch (TYPEOF(VECTOR_ELT(data, other_ind[i]))) {
			HANDLE_CASE(LGLSXP, int, LOGICAL);
			HANDLE_CASE(INTSXP, int, INTEGER);
			HANDLE_CASE(REALSXP, double, REAL);
			HANDLE_CASE(STRSXP, SEXP, STRING_PTR);
			default: Rf_error("Unhandled type %s", type2char(TYPEOF(VECTOR_ELT(data, other_ind[i]))));
			}
		}
	}

#undef HANDLE_CASE

	// copy in the 'other' variables first
#define COPY(RTYPE, CTYPE, ACCESSOR) { \
		PROTECT(tmp = allocVector(RTYPE, nRow)); \
		CTYPE* tmp_ptr = ACCESSOR(tmp); \
		CTYPE* data_ptr = ACCESSOR(VECTOR_ELT(data, other_ind[i])); \
		for (int i=0; i < nRow; ++i) { \
			tmp_ptr[i] = data_ptr[i]; \
		} \
		SET_VECTOR_ELT(output, i, tmp); \
		UNPROTECT(1); \
		break; \
		} \

	SEXP tmp;
	for (int i=0; i < n_other; ++i) {
		switch (TYPEOF(VECTOR_ELT(data, other_ind[i]))) {
		case LGLSXP: COPY(LGLSXP, int, LOGICAL);
		case INTSXP: COPY(INTSXP, int, INTEGER);
		case REALSXP: COPY(REALSXP, double, REAL);
		case STRSXP: COPY(STRSXP, SEXP, STRING_PTR);
		default: Rf_error("Unhandled SEXP type");
		}
	}

#undef COPY

#define COPY(RTYPE, CTYPE, ACCESSOR) { \
		PROTECT(tmp = allocVector(RTYPE, nRow)); \
		CTYPE* tmp_ptr = ACCESSOR(tmp); \
		CTYPE* data_ptr = ACCESSOR(VECTOR_ELT(data, value_ind)); \
		for (int j=0; j < nRow; ++j) { \
			tmp_ptr[j] = data_ptr[j + (i*nRow)]; \
		} \
		SET_VECTOR_ELT(output, i + n_other, tmp); \
		UNPROTECT(1); \
		break; \
		} \

	// copy the value
	int valuetype = TYPEOF(VECTOR_ELT(data, value_ind));
	for (int i=0; i < n_uniq; ++i) {
		switch (valuetype) {
		case LGLSXP: COPY(LGLSXP, int, LOGICAL);
		case INTSXP: COPY(INTSXP, int, INTEGER);
		case REALSXP: COPY(REALSXP, double, REAL);
		case STRSXP: COPY(STRSXP, SEXP, STRING_PTR);
		}
	}

	// set the names
	SEXP datanames = getAttrib(data, R_NamesSymbol);
	SEXP names;
	GUARD(names = allocVector(STRSXP, n_other + n_uniq));
	for (int i=0; i < n_other; ++i) {
		SET_STRING_ELT(names, i, STRING_ELT(datanames, i));
	}
	for (int i=0; i < n_uniq; ++i) {
		SET_STRING_ELT(names, n_other+i, STRING_ELT(uniq_id, i));
	}
	setAttrib(output, R_NamesSymbol, names);

	// set the class
	setAttrib(output, R_ClassSymbol, mkString("data.frame"));

	// set the rows
	SEXP rownames;
	GUARD( rownames=allocVector(INTSXP, nRow) );
	int* rownames_ptr = INTEGER(rownames);
	for (int i=0; i < nRow; ++i) {
		rownames_ptr[i] = i+1;
	}
	setAttrib(output, R_RowNamesSymbol, rownames);
	UNGUARD;
	return output;
}

#undef USE_RINTERNALS

#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

char max_type1(SEXP x) {
  if (TYPEOF(x) != VECSXP) {
    error("Expected a VECSXP but got a '%s'", type2char(TYPEOF(x)));
  }
  int n = length(x);
  char max_type = -1;
  char tmp = -1;
  for (int i=0; i < n; ++i) {
    // factors should mean we coerce to string
    if (isFactor(VECTOR_ELT(x, i))) {
      if (STRSXP > max_type) {
        max_type = STRSXP;
      }
    } else if ((tmp = TYPEOF( VECTOR_ELT(x, i) )) > max_type) {
      max_type = tmp;
    }
  }
  return max_type;
}

char max_type(SEXP x, SEXP ind_) {
  if (TYPEOF(x) != VECSXP) {
    error("Expected a VECSXP but got a '%s'", type2char(TYPEOF(x)));
  }
  int n = length(ind_);
  int* ind = INTEGER(ind_);
  char max_type = -1;
  char tmp = -1;
  for (int i=0; i < n; ++i) {
    // factors should mean we coerce to string
    if (isFactor(VECTOR_ELT(x, ind[i]))) {
      if (STRSXP > max_type) {
        max_type = STRSXP;
      }
    } else if ((tmp = TYPEOF( VECTOR_ELT(x, ind[i]) )) > max_type) {
      max_type = tmp;
    }
  }
  return max_type;
}

void set_names(SEXP x) {
  int m = length(x);
  SEXP nm = PROTECT( allocVector(STRSXP, m) );
  char str[ (int) log10(m) + 3];
  for (int i = 0; i < m; ++i) {
  	sprintf(str, "%s%i", "V", i + 1);
		SET_STRING_ELT(nm, i, mkChar(str));
	}
	setAttrib(x, R_NamesSymbol, nm);
  UNPROTECT(1);
}

void set_rownames(SEXP x) {
  int n = length(VECTOR_ELT(x, 0));
  SEXP rownames = PROTECT( allocVector(INTSXP, n) );
  int* ptr = INTEGER(rownames);
  for (int i=0; i < n; ++i) {
    ptr[i] = i+1;
  }
  setAttrib(x, R_RowNamesSymbol, rownames);
  UNPROTECT(1);
}

#undef USE_RINTERNALS

