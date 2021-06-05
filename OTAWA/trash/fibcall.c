int fib(int n) {
	int i, Fnew, Fold, temp, ans; 
	
	Fnew = 1; Fold = 0;
	i = 2; 
	
	while(i <= n)	
	{ 
		temp = Fnew; 
		Fnew = Fnew + Fold;
		Fold = temp;
		i++;
	}

	ans = Fnew; 
	return ans;
}

main() {
	int a; 
	a = 30; 
	fib(a);
}
