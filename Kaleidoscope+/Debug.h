#ifndef _KALEIDOSCOPE_DEBUG
#define _KALEIDOSCOPE_DEBUG 
#include <iostream>


//#define DEBUG_TOKEN_ON
#ifdef DEBUG_TOKEN_ON
#define DEBUG_TOKEN(x) std::cout<<(x)<<std::endl;
#else
#define DEBUG_TOKEN(x)
#endif // DEBUG_TOKEN_ON



#define DEBUG_CERR_ON
#ifdef DEBUG_CERR_ON
#define DEBUG_CERR(x) std::cerr<<(x)<<std::endl;
#else
#define DEBUG_CERR(X)
#endif


#endif	//_KALEIDOSCOPE_DEBUG