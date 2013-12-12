#include <Rcpp.h>
using namespace Rcpp;

SEXP fast_factor(SEXP, SEXP);

// [[Rcpp::export(.char_to_factor)]]
RObject char_to_factor( RObject x_, bool inplace ) {

  RObject x;
  if (inplace) {
    x = x_;
  } else {
    x = clone(x_);
  }
  
  if (TYPEOF(x) == VECSXP) {
    int n = Rf_length(x);
    for (int i=0; i < n; ++i) {
      SET_VECTOR_ELT(x, i, char_to_factor( VECTOR_ELT(x, i), false ));
    }
  } else if (TYPEOF(x) == STRSXP) {
    x = fast_factor(x, sort_unique( as<CharacterVector>(x) ));
  }
  
  return x;
  
}

#include <Rcpp.h>
#include <Rdefines.h>
using namespace Rcpp;

template <int RTYPE>
IntegerVector do_counts( const Vector<RTYPE>& x ) {
  IntegerVector output = table(x);
  if (Rf_isFactor(x)) {
    Rf_setAttrib(output, R_NamesSymbol, Rf_getAttrib(x, R_LevelsSymbol));
  }
  // fix names
  CharacterVector names = output.attr("names");
  for (int i=0; i < output.size(); ++i) {
    if (names[i] == "-0") {
      names[i] = "0";
    }
  }
  output.attr("names") = names;
  return output;
}

// [[Rcpp::export]]
SEXP counts( SEXP x ) {
  switch( TYPEOF(x) ) {
  case INTSXP: return do_counts<INTSXP>(x);
  case REALSXP: return do_counts<REALSXP>(x);
  case STRSXP: return do_counts<STRSXP>(x);
  case LGLSXP: return do_counts<LGLSXP>(x);
  default: {
    Rf_error("'x' is of invalid type '%s'", Rf_type2char( TYPEOF(x) ));
    return R_NilValue;
  }
  }
}

#include <iostream>
#include <fstream>
#include <Rcpp.h>
using namespace Rcpp;

std::string get_item( std::string& line, std::string& delim, int column ) {

	char* line_cast = const_cast<char*>( line.c_str() );
	const char* pch = strtok(line_cast, delim.c_str());
	int counter = 0;
	while( pch != NULL ) {
		if( counter == column-1 ) {
			return( std::string(pch) );
		}
		pch = strtok(NULL, delim.c_str());
		++counter;
	}
	stop( "get_line is broken" );
	return( "get_line is broken" );
}

inline bool in( std::string& item, std::vector< std::string >& list ) {

	if( std::find( list.begin(), list.end(), item ) != list.end() ) {
		return true;
	} else {
		return false;
	}
}

// [[Rcpp::export]]
void extract_rows_from_file_to_file(
		std::string input_file_name,
		std::string output_file_name,
		std::string delim,
		std::vector< std::string > items_to_keep,
		int column_to_check ) {

	std::string line;
	std::string line_copy;
	std::string item_to_check;

	std::ifstream conn( input_file_name.c_str(), std::ios_base::binary );
	std::ofstream out_conn( output_file_name.c_str() );
	std::ostreambuf_iterator<char> out_itr( out_conn );

	if( !out_conn.is_open() ) {
		stop("Couldn't open the output file!");
	}

	if( conn.is_open() ) {
		// Rcout << "Successfully opened file." << std::endl;
		while( std::getline(conn, line) ) {

			// copy the string
			line_copy = line.c_str();
			item_to_check = get_item( line_copy, delim, column_to_check );
			// Rcout << "The item we're checking is: " << item_to_check << std::endl;
			if( in( item_to_check, items_to_keep ) ) {
				// Rcout << "Copying line" << std::endl;
				std::copy( line.begin(), line.end(), out_itr );
				out_itr = '\n';
			}
		}
	} else {
		stop("Couldn't open File!\nInput file path: " + input_file_name);
	}

	conn.close();
	out_conn.close();

}

// [[Rcpp::export]]
std::vector<std::string> extract_rows_from_file(
		std::string input_file_name,
		std::string delim,
		std::vector< std::string > items_to_keep,
		int column_to_check ) {

	std::string line;
	std::string line_copy;
	std::string item_to_check;
	std::vector<std::string> output;

	std::ifstream conn( input_file_name.c_str(), std::ios_base::binary );

	if( conn.is_open() ) {
		// Rcout << "Successfully opened file." << std::endl;
		while( std::getline(conn, line) ) {

			// copy the string
			line_copy = line.c_str();
			item_to_check = get_item( line_copy, delim, column_to_check );
			// Rcout << "The item we're checking is: " << item_to_check << std::endl;
			if( in( item_to_check, items_to_keep ) ) {
				// Rcout << "Copying line" << std::endl;
				output.push_back(line);
			}
		}
	} else {
		stop("Couldn't open File!\nInput file path: " + input_file_name);
	}

	conn.close();
	return output;

}

#include <Rcpp.h>
using namespace Rcpp;

template <int RTYPE>
IntegerVector fast_factor_template( const Vector<RTYPE>& x, SEXP levels, bool isNull ) {

  Vector<RTYPE> sorted;
  if (isNull) {
    sorted = sort_unique(x);
  } else {
    sorted = Vector<RTYPE>(levels);
  }
  
	IntegerVector out = match( x, sorted );

	// handle NAs
	if( Vector<RTYPE>::is_na( *sorted.begin() ) ) {

		out = out - 1;
		// we replace all 0's with NAs in the output
		for( IntegerVector::iterator it = out.begin(); it != out.end(); ++it ) {
			if( (*it) == 0 ) {
				(*it) = NA_INTEGER;
			}
		}

		// we remove the first element from sorted, since it acts as levels
		Vector<RTYPE> levels( sorted.begin()+1, sorted.end() );
		out.attr("levels") = as<CharacterVector>( levels );

	} else {
		out.attr("levels") = as<CharacterVector>( sorted );
	}

	out.attr("class") = "factor";
	return out;

}

// [[Rcpp::export]]
SEXP fast_factor( SEXP x, SEXP levels ) {
  int type = TYPEOF(x);
  if (!Rf_isNull(levels) && TYPEOF(levels) != type) {
    levels = Rf_coerceVector(levels, type);
  }
  bool isNull = Rf_isNull(levels);
	switch( type ) {
	case INTSXP: return fast_factor_template<INTSXP>(x, levels, isNull);
	case REALSXP: return fast_factor_template<REALSXP>(x, levels, isNull);
	case STRSXP: return fast_factor_template<STRSXP>(x, levels, isNull);
	case LGLSXP: return fast_factor_template<INTSXP>(x, levels, isNull);
	}
	Rf_error("argument is of incompatible type '%s'", Rf_type2char( TYPEOF(x) ));
	return R_NilValue;
}

#include <Rcpp.h>
using namespace Rcpp;

template <class T>
IntegerMatrix do_matches(List x) {
  int n = x.size();
  IntegerMatrix output(n, n);
  
  for (int i=0; i < n; ++i) {
    for (int j=0; j < n; ++j) {
      int tmp = sum( !is_na( match( as<T>(x[i]), as<T>(x[j]) ) ) );
      output(i, j) = (int) tmp;
    }
  }
  
  return output;
  
}

// [[Rcpp::export]]
IntegerMatrix matches(List x) {
  switch( TYPEOF(x[0]) ) {
    case INTSXP: return do_matches<IntegerVector>(x);
    case REALSXP: return do_matches<NumericVector>(x);
    case STRSXP: return do_matches<CharacterVector>(x);
    case LGLSXP: return do_matches<IntegerVector>(x);
    default: {
      stop("invalid SEXP type");
      return R_NilValue;
    }
    }
}

#include <Rcpp.h>

#ifdef WIN32         // means WIN64, too
#undef Realloc
#undef Free
#include <windows.h>
#include <stdio.h>
#include <tchar.h>
#else
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>   // for open()
#include <unistd.h>  // for close()
#endif

using namespace Rcpp;

inline int nrow(const char* x, int sz) {
  const char* ptr = &x[0];
  int nrow = 0;
  while (ptr < x + sz) {
    nrow += *ptr == '\n';
    ++ptr;
  }
  return nrow;
}

// [[Rcpp::export]]
SEXP read(std::string path, bool lines) {

	using namespace std;
  
  const char eol = '\n';
  char* map;
	
#ifndef WIN32
  struct stat file_info;
  
  int fd = open( path.c_str(), O_RDONLY );
  if (fstat(fd, &file_info) == -1) {
  	stop("Could not read file information.");
	}
	int sz = file_info.st_size;
  if (sz <= 0) {
    SEXP output = Rf_allocVector(STRSXP, 0);
    return output;
  }
#ifdef MAP_POPULATE
  map = (char*) mmap(0, sz, PROT_READ, MAP_SHARED | MAP_POPULATE, fd, 0);
#else
  map = (char*) mmap(0, sz, PROT_READ, MAP_SHARED, fd, 0);
#endif

  if (map == MAP_FAILED) {
  	close(fd);
		stop("Error mapping the file.");
	}

#else
  // tactlessly borrowed from data.table
  // Following: http://msdn.microsoft.com/en-gb/library/windows/desktop/aa366548(v=vs.85).aspx
  HANDLE hFile=0;
  HANDLE hMap=0;
  DWORD dwFileSize=0;
  const char* fnam = path.c_str();
  hFile = CreateFile(fnam, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
  if (hFile==INVALID_HANDLE_VALUE) Rf_error("File not found: %s",fnam);
  dwFileSize=GetFileSize(hFile,NULL);
  if (dwFileSize==0) { CloseHandle(hFile); Rf_error("File is empty: %s", fnam); }
  size_t filesize = (size_t)dwFileSize;
  int sz = (int) filesize;
  hMap=CreateFileMapping(hFile, NULL, PAGE_READONLY, 0, 0, NULL); // dwFileSize+1 not allowed here, unlike mmap where +1 is zero'd
  if (hMap==NULL) { CloseHandle(hFile); Rf_error("This is Windows, CreateFileMapping returned error %d for file %s", GetLastError(), fnam); }
  map = (char *)MapViewOfFile(hMap,FILE_MAP_READ,0,0,dwFileSize);
  if (map == NULL) {
      CloseHandle(hMap);
      CloseHandle(hFile);
  }
#endif

	SEXP output;
  
  // split by '\n'?
  if (lines) {
    
    // incomplete final line?
    bool read_last_line = false;
    char last_char = *(map + sz - 1);
    if (last_char != '\n' && last_char != '\r') {
      Rf_warning("incomplete final line found on '%s'", path.c_str());
      read_last_line = true;
    }
    
    if (strchr(map, '\n') == NULL && strchr(map, '\r') == NULL) {
      Rf_warning("incomplete final line found on '%s'", path.c_str());
      PROTECT( output = Rf_allocVector(STRSXP, 1) );
      SET_STRING_ELT(output, 0, Rf_mkCharLen(map, sz));
    } else {
      int n = nrow(map, sz);
      if (read_last_line) {
        ++n;
      }
      output = PROTECT( Rf_allocVector(STRSXP, n) );
      char* pch_old = &map[0];
      char* pch;
      pch = strchr(map, eol);
      // avoid reading \r
      int pch_incr = 1;
      if (*(pch-1) == '\r') {
        --pch;
        ++pch_incr;
      }
      int i = 0;
      while (pch != NULL) {
        if (*(pch-1) == '\r') {
          --pch;
        }
        SET_STRING_ELT(output, i, Rf_mkCharLen(pch_old, (int)((size_t) pch - (size_t) pch_old)));
        pch_old = pch + pch_incr;
        pch = strchr(pch_old, eol);
        ++i;
      }
      if (read_last_line) {
        SET_STRING_ELT(output, n-1, Rf_mkChar(pch_old));
      }
    }
  } else {
    output = PROTECT( Rf_allocVector(STRSXP, 1) );
    SET_STRING_ELT(output, 0, Rf_mkCharLen(map, sz));
  }
  
#ifndef WIN32
  munmap(map, sz);
	close(fd);
#else
  UnmapViewOfFile(map);
  CloseHandle(hMap);
  CloseHandle(hFile);
#endif
  
  UNPROTECT(1);
  return output;

}

#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <string.h>

using namespace Rcpp;

std::string get_item( std::string& line, const char* delim, const int column ) {

	char* line_cast = const_cast<char*>( line.c_str() );
	const char* pch = strtok(line_cast, delim);
	int counter = 0;
	while( pch != NULL ) {
		if( counter == column-1 ) {
			return( std::string(pch) );
		}
		pch = strtok(NULL, delim);
		++counter;
	}
	stop( "get_line is broken" );
	return( "get_line is broken" );
}

inline bool in( const std::string& elem, const std::map<std::string, std::ofstream*>& x ) {
	if( x.find(elem) == x.end() ) {
		return false;
	} else {
		return true;
	}
}

inline void print_counter( int& counter ) {
	if ( (counter % 100000) == 0 ) {
		Rcpp::Rcout << "i = " << counter << std::endl;
	}
  ++counter;
}

// [[Rcpp::export]]
void split_file(
		std::string path,
		std::string dir,
		std::string basename,
		std::string path_sep,
		std::string sep,
		std::string prepend,
		std::string file_ext,
		int column,
		int skip,
		bool verbose) {

	// space for a line, and a file map
	std::string line;
	std::map< std::string, std::ofstream* > files;
	std::map< std::string, std::ostreambuf_iterator<char>* > file_itrs;
	const char* delim = sep.c_str();

	// input file connections
	std::ifstream conn;
	conn.open( path.c_str(), std::ios_base::binary );

	int counter = 0;

	if( conn.is_open() ) {

		// skip lines
		if( skip > 0 ) {
			for( int i=0; i < skip; ++i ) {
				std::getline( conn, line );
			}
		}

		while( std::getline( conn, line ) ) {

			// check the value of the 'column'th item
			// we copy the string so that strtok doesn't mangle it
			std::string str_copy;
			str_copy = line.c_str();
			std::string col_item = get_item(str_copy, delim, column);

			// if a column entry has not yet been found, open a new file connection
			if( !in( col_item, files ) ) {
				if( verbose ) {
					Rcpp::Rcout << "Opening new file for column entry: " << col_item << std::endl;
				}
				std::string file_path =  dir + path_sep + basename + "_" + prepend + col_item + file_ext;
				files[col_item] = new std::ofstream( file_path.c_str() );
				file_itrs[col_item] = new std::ostreambuf_iterator<char>( *files[col_item] );
			}

			// write the line to the appropriate ofstream
			copy( line.begin(), line.end(), *file_itrs[col_item] );
			*file_itrs[col_item] = '\n';
			// *files[col_item] << line << std::endl;

			// write out the counter?
			if( verbose ) {
				print_counter(counter);
			}

		}

	}

	// close the other file connections
	typedef std::map<std::string, std::ofstream*>::iterator MItr;
	for( MItr it = files.begin(); it != files.end(); ++it ) {
		it->second->close();
		delete it->second;
	}
	files.clear();

	typedef std::map<std::string, std::ostreambuf_iterator<char>*>::iterator NItr;
	for( NItr it = file_itrs.begin(); it != file_itrs.end(); ++it ) {
		delete it->second;
	}
	file_itrs.clear();

}

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List split_runs_numeric( NumericVector X ) {

	List out( X.size() );

	std::vector< std::vector< double > > all_nums;
	std::vector< double > curr_nums;

	// initial stuff
	curr_nums.push_back( X[0] );

	for( NumericVector::iterator it = X.begin()+1; it != X.end(); ++it ) {
		if( (*it) != (*(it-1)) ) {
			all_nums.push_back( curr_nums );
			curr_nums.clear();
			curr_nums.push_back( *it );
		} else {
			curr_nums.push_back( *it );
		}
	}

	// push the final vector in
	all_nums.push_back( curr_nums );

	return wrap( all_nums );

}

// [[Rcpp::export]]
List split_runs_character( std::vector< std::string > X ) {

	std::vector< std::vector< std::string > > all_nums;
	std::vector< std::string > curr_nums;

	// initial stuff
	curr_nums.push_back( X[0] );

	for( std::vector< std::string >::iterator it = X.begin() + 1; it != X.end(); ++it ) {
		if( *it != *(it-1) ) {
			all_nums.push_back( curr_nums );
			curr_nums.clear();
			curr_nums.push_back( *it );
		} else {
			curr_nums.push_back( *it );
		}
	}

	// push the final vector in
	all_nums.push_back( curr_nums );

	return wrap( all_nums );

}

// [[Rcpp::export]]
List split_runs_one( std::string x ) {

	std::vector< std::string > out;

	std::string curr_str;
	curr_str.append( x, 0, 1 );

	for( std::string::const_iterator it = x.begin()+1; it != x.end(); ++it ) {
		if( *it != *(it-1) ) {
			out.push_back( curr_str );
			curr_str.erase();
			curr_str.push_back( *it );
		} else {
			curr_str.push_back( *it );
		}
	}

	out.push_back( curr_str );

	return wrap(out);

}

#include <Rcpp.h>
using namespace Rcpp;

template <int RTYPE, class container>
inline
Vector<RTYPE> stack( List& X, int index ) {
	std::vector<container> out;
	int x_size = X.size();
	for( int i=0; i < x_size; ++i ) {
		List tmp = as<List>(X[i]);
		std::vector<container> tmp2 = tmp[index];
		int tmp2_size = tmp2.size();
		for( int j=0; j < tmp2_size; ++j ) {
			out.push_back( tmp2[j] );
		}
	}
	return wrap(out);
}

// [[Rcpp::export]]
List stack_list_df( List& X,
		std::vector< std::string > classes,
		int num_elem,
		bool make_row_names,
		std::string name,
		bool keep_list_index,
		std::string index_name ) {

	List out(num_elem);

	// loop through the columns to generate the stacked DF
	List tmp = as<List>(X[0]);

	for( int i=0; i < num_elem; ++i ) {

		switch( TYPEOF(tmp[i]) ) {
		case STRSXP:
			out[i] = stack<STRSXP, std::string>(X, i);
			break;
		case REALSXP:
			out[i] = stack<REALSXP, double>(X, i);
			break;
		case INTSXP:
			out[i] = stack<INTSXP, int>(X, i);
			break;
		case LGLSXP:
			out[i] = stack<LGLSXP, int>(X, i);
			break;
		case RAWSXP:
			out[i] = stack<RAWSXP, unsigned char>(X, i);
			break;
		}

	}

	// add the list indices as a vector
	if( keep_list_index ) {
		std::vector<int> list_indices;
		int counter = 1;
		for( int i=0; i < X.size(); ++i ) {
			List tmp = as<List>( X[i] );
			for( int j=0; j < ::Rf_length( tmp[0] ); ++j ) {
				list_indices.push_back(counter);
			}
			++counter;
		}
		out["index"] = wrap( list_indices );
	}

	// get row names to assign to vector of df
	if( make_row_names ) {
		std::vector< std::string > row_names;
		for( int i=0; i < X.size(); ++i ) {
			std::vector< std::string > rownames = as<std::vector< std::string > >( as<List>( X[i] ).attr("row.names") );
			int rownames_size = rownames.size();
      for( int j=0; j < rownames_size; ++j ) {
				row_names.push_back( rownames[j] );
			}
		}

		out["which"] = wrap( row_names );
	}

	std::vector<std::string> col_names = as<List>(X[0]).attr("names");

	if( keep_list_index ) {
		col_names.push_back( index_name );
	}

	if( make_row_names ) {
		col_names.push_back( name );
	}

	out.attr("names") = col_names;
	out.attr("class") = "data.frame";
	return out;

}

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector str_collapse_list(List x) {
  int n = x.size();
  CharacterVector output = no_init(n);
  for (int i=0; i < n; ++i) {
    output[i] = collapse( as<CharacterVector>(x[i]) );
  }
  
  output.attr("names") = x.attr("names");
  
  return output;
}

// [[Rcpp::export]]
String str_collapse_str(CharacterVector x) {
  return collapse(x);
}

