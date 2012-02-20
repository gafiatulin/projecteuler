#include <new>
#include <cmath>
#include <iostream>
#include <exception>
#include <sys/time.h>

typedef unsigned long long ulong;

using namespace std;

class SieveOfAtkin
{
	private :
		ulong Limit;
		static bool *IsPrime;
		
	public :
		SieveOfAtkin() { }
		SieveOfAtkin(ulong Limit) : Limit(Limit)
		{
			try
			{
				IsPrime = new bool[Limit];
				for (ulong i = 0; i <= Limit; i++)
				{
					IsPrime[i] = false;
				}
			}
			catch(bad_alloc&)
			{
				cerr << "  Memory allocation unsuccessful." << endl;
			}
		}
		static long NoOfPrime;
		void PrimeGen();
		void Display();
		
};

bool *SieveOfAtkin :: IsPrime;
void SieveOfAtkin :: PrimeGen()
{
	ulong SqrtLimit = (ulong)ceil(sqrt((double)Limit));
	
	for (ulong x = 1; x <= SqrtLimit; x++)

        {
        	for (ulong y = 1; y <= SqrtLimit; y++)
        	{
        		ulong xsquared = x*x;
           		ulong ysquared = y*y;
				ulong n = (xsquared << 2) + ysquared; // n = 4*x*x + y*y
            	if (n <= Limit && (n % 12 == 1 || n % 12 == 5))
            	{
                	IsPrime[n] ^= true;
            	}
            	n -= xsquared;  // n = 3*x*x + y*y
            	if (n <= Limit && n % 12 == 7)
            	{
               		IsPrime[n] ^= true;
           		}
            	if (x > y)
            	{
               		n -= (ysquared << 1);  // n = 3*x*x - y*y	
                	if (n <= Limit && n % 12 == 11)
                	{
                    		IsPrime[n] ^= true;
                	}
            	}
			}
        }
        for (ulong n = 5; n <= SqrtLimit; n++)
        {
			if (IsPrime[n])
            {
               	ulong s = n * n;
               	for (ulong k = s; k <= Limit; k += s)
               	{
                   		IsPrime[k] = false;
               	}
            }
        }
        IsPrime[2] = true;
        IsPrime[3] = true;
}

void SieveOfAtkin :: Display()
{
	long long Counter = 0;
	for ( ulong i = 2; i <= Limit; i++)
	{
		if (IsPrime[i])
		{
			Counter += i;
		}
	}
	cout << Counter << endl;
}

int main ()
{
	SieveOfAtkin Object = SieveOfAtkin(2000000LLU);
	Object.PrimeGen();
	Object.Display();
	return 0;
}
