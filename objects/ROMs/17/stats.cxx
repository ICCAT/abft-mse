//stats.cxx
/*
	Statistical distributions commonly used in ADMB programs.
	Based on the documenation found in R.
	All distributions are in negative log-space.
	Author: Martell
	Date: 12/18/2007
*/


#include <admodel.h>

//dvariable round2(const dvector& x, const int& nn)
//{
//  vector temp(
//  return round(value(x),2);
//}


const double pi=3.141593;
//plogis
dvariable plogis(const dvariable& x, const double& mu, const dvariable& std)
{
	return 1./(1.+mfexp((mu-x)/std));
}

dvar_vector plogis(const dvector& x, const dvariable& mu, const dvariable& std)
{
	return 1./(1.+mfexp((mu-x)/std));
}

dvector plogis(const dvector& x, const double& mu, const double& std)
{
	return 1./(1.+mfexp((mu-x)/std));
}

dvar_vector plogis(const dvar_vector& x, const dvariable& mu, const dvariable& std)
{
	return 1./(1.+mfexp((mu-x)/std));
}


//uniform distribution
dvariable dunif(const dvariable& x, const double min, const double max)
{
	return log(max-min);
}

//beta distribution
dvariable dbeta(const dvariable& x, const double a, const double b)
{
	return - gammln(a+b)+(gammln(a)+gammln(b))-(a-1.)*log(x)-(b-1.)*log(1.-x);
}

//inverse-gamma
dvariable dinvgamma(const dvariable& x, const double a, const double b)
{
	return -a*log(b)+gammln(a)+(a+1)*log(x)+b/x;
}

//gamma
dvariable dgamma(const dvariable &x, const double a, const double b)
{
	//E(x)=a/b;
	//V(x)=a/b^2
	return -a*log(b)+gammln(a)-(a-1)*log(x)+b*x;
}

//normal distribution
dvariable dnorm(const dvariable& x, const double& mu, const double& std)
{
	double pi=3.141593;
	return 0.5*log(2.*pi)+log(std)+0.5*square(x-mu)/(std*std);
}

dvariable dnorm(const dvariable& x, const double& mu, const dvariable& std)
{
	double pi=3.141593;
	return 0.5*log(2.*pi)+log(std)+0.5*square(x-mu)/(std*std);
}

//normal distribution
dvariable dnorm(const dvariable& x, const dvariable& mu, const double& std)
{
	double pi=3.141593;
	return 0.5*log(2.*pi)+log(std)+0.5*square(x-mu)/(std*std);
}

/*dvariable dnorm(const dvar_vector& residual, const dvariable std)
{
	RETURN_ARRAYS_INCREMENT();
	double pi=3.141593;
	long n=size_count(residual);
	dvariable SS=norm2(residual);
	RETURN_ARRAYS_DECREMENT();
	return(n*(0.5*log(2.*pi)+log(std))+0.5*SS/(std*std));
	//return(n*(0.5*log(2.*pi)-log(std))+0.5*SS*(std*std));
}

dvariable dnorm(const dvar_vector& residual, const double std)
{
	RETURN_ARRAYS_INCREMENT();
	double pi=3.141593;
	long n=size_count(residual);
	dvariable SS=norm2(residual);
	RETURN_ARRAYS_DECREMENT();
	return(n*(0.5*log(2.*pi)+log(std))+0.5*SS/(std*std));
	//return(n*(0.5*log(2.*pi)-log(std))+0.5*SS*(std*std));
}

dvariable dnorm(const dvar_vector& residual, const dvector std)
{
	RETURN_ARRAYS_INCREMENT();
	double pi=3.141593;
	int n=size_count(residual);
	dvector var=elem_prod(std,std);
	dvar_vector SS=elem_prod(residual,residual);
	RETURN_ARRAYS_DECREMENT();
	return(0.5*n*log(2.*pi)+sum(log(std))+sum(elem_div(SS,2.*var)));
}

dvariable dnorm(const dvar_vector& residual, const dvar_vector std)
{
	RETURN_ARRAYS_INCREMENT();
	double pi=3.141593;
	int n=size_count(residual);
	dvar_vector var=elem_prod(std,std);
	dvar_vector SS=elem_prod(residual,residual);
	RETURN_ARRAYS_DECREMENT();
	return(0.5*n*log(2.*pi)+sum(log(std))+sum(elem_div(SS,2.*var)));
}
*/

//log normal distribution
dvariable dlnorm(const dvariable& x, const double& mu, const double& std)
{
	double pi=3.141593;
	return 0.5*log(2.*pi)+log(std)+log(x)+square(log(x)-mu)/(2.*std*std);
}

// log multinomial distribution
/**
	Mulitnomial distribution.
*/
dvariable dmultinom(const dvector& x, const dvar_vector& p)
{
	if(x.indexmin() != p.indexmin() || x.indexmax() != p.indexmax())
	{
		cerr << "Index bounds do not macth in"
		" dmultinom(const dvector& x, const dvar_vector& p)\n";
		ad_exit(1);
	}

	double n=sum(x);
	return -gammln(n+1.)+sum(gammln(x+1.))-x*log(p/sum(p));
}

double neff(const dvector& obs, const dvar_vector& pred)
{
	dvector resid=value(obs-pred);
	dvector var=value(elem_prod(1.-pred,pred));
	return sum(var)/norm2(resid);
}

//poisson distribution
dvariable dpois(const double& x, const dvariable& lambda)
{
	return -x*log(lambda)+lambda+gammln(x+1);
}

//The following function has been depricated.
// multivariate logistic negative log likelihood
dvariable dmvlogistic(const dmatrix o, const dvar_matrix& p, double& tau2)
{
	//returns the negative loglikelihood using the MLE for the variance
	RETURN_ARRAYS_INCREMENT();

	int a = o.colmin();
	int A=o.colmax();
	double tiny=0.001/(A-a+1);
	int t=o.rowmin();
	int T=o.rowmax();
	dvariable tau_hat2;		//mle of the variance
	dvar_matrix nu(t,T,a,A);
	for(int i=t; i<=T; i++)
	{
		dvector o1=o(i)/sum(o(i));
		dvar_vector p1=p(i)/sum(p(i));
		//p1=log(p1)-mean(p1);
		//p1=log(p1)-mean(log(p1));
		//p1 = exp(p1)/sum(exp(p1));
		nu(i) = log(o1+tiny)-log(p1+tiny) - mean(log(o1+tiny)-log(p1+tiny));
	}
	tau_hat2 = 1./((A-a)*(T-t+1))*norm2(nu);
	dvariable nloglike =((A-a)*(T-t+1))*log(tau_hat2);
	tau2=value(tau_hat2); //mle of the variance
	RETURN_ARRAYS_DECREMENT();
	return(nloglike);
}
dvariable dmvlogistic(const dvector o, const dvar_vector& p, double& tau)
{
	//returns the negative loglikelihood using the MLE for the variance (copy of Steve's now simplified to a vector)
	//int a =o.indexmin();
	int nobs=o.indexmax();
	double tiny=0.001/nobs;//(A-a+1);
	dvariable tau_hat2;		//mle of the variance
	dvar_vector nu(1,nobs);
	//for(int i=t; i<=T; i++)
	//{
	nu = log(o+tiny)-log(p+tiny) - mean(log(o+tiny)-log(p+tiny));
	//}
	tau_hat2 = 1./(nobs*norm2(nu));
	dvariable nloglike =nobs*log(tau_hat2);
	return(nloglike);
}

// multivariate logistic negative log likelihood
dvariable dmvlogistic(const dmatrix o, const dvar_matrix& p,dvar_matrix& nu, double& tau2,const double minp)
{	//returns the negative loglikelihood using the MLE for the variance
	/*
		This is a modified version of the dmvlogistic negative log likelihood
		where proportions at age less than minp are pooled into the consecutive
		age-classes.  See last paragraph in Appendix A of Richards, Schnute and
		Olsen 1997.

		NB minp must be greater than 0, otherwise this algorithm returns an
		error if one of the observed proportions is zero.

		-1) first count the number of observations for each year > minp
		-2) normalized observed and predicted age-proportions
		-3) loop over ages, and check if observed proportion is < minp
				-if yes, then add observed proprtion to bin k
				-if no then add observed proportion to bin k and increment
				 bin k if k is currently less than the number of bins.
		-4) do the same grouping for the predicted proportions.
		-5) use ivector iiage to correctly assign residuals into nu

		FEB 8, 2011.  Fixed a bug in the variance calculation &
		likelihood scaling that was discovered at the 2011 Hake
		assessment STAR panel in Seattle.
	*/
	RETURN_ARRAYS_INCREMENT();
	int i,j,k,n;
	int age_counts=0;
	int a = o.colmin();
	int A=o.colmax();
	double tiny=0.001/(A-a+1);
	int t=o.rowmin();
	int T=o.rowmax();
	dvariable tau_hat2;		//mle of the variance
	//dvar_matrix nu(t,T,a,A);
	nu.initialize();

	for(i=t; i<=T; i++)
	{
		n=0;
		dvector oo = o(i)/sum(o(i));
		dvar_vector pp = p(i)/sum(p(i));

		//count # of observations greater than minp (2% is a reasonable number)
		for(j=a;j<=A;j++)
			if(oo(j) > minp)n++;

		ivector iiage(1,n);
		dvector o1(1,n); o1.initialize();
		dvar_vector p1(1,n); p1.initialize();
		k=1;
		for(j=a;j<=A;j++)
		{
			if(oo(j)<=minp)
			{
				o1(k)+=oo(j);
				p1(k)+=pp(j);
			}
			else
			{
				o1(k)+=oo(j);
				p1(k)+=pp(j);
				if(k<=n)iiage(k)=j;		//ivector for the grouped residuals
				if(k<n) k++;
			}
		}

		//assign residuals to nu based on iiage index
		dvar_vector t1 = log(o1)-log(p1) - mean(log(o1)-log(p1));

		for(j=1;j<=n;j++)
			nu(i)(iiage(j))=t1(j);

		age_counts += n-1;
	}
	//Depricated  Wrong Variance & likelihood calculation.
	//tau_hat2 = 1./(age_counts*(T-t+1))*norm2(nu);
	//dvariable nloglike =(age_counts*(T-t+1))*log(tau_hat2);

	//Feb 8, 2011  Fixed variance & likelihood
	tau_hat2 = 1./(age_counts)*norm2(nu);
	dvariable nloglike =(age_counts)*log(tau_hat2);
	tau2=value(tau_hat2); //mle of the variance
	RETURN_ARRAYS_DECREMENT();
	return(nloglike);
}

// random multivariate logistic.
dvector rmvlogistic(const dvector& p, const double& tau2, const int& seed)
{
	int a=p.indexmin();
	int A=p.indexmax();
	random_number_generator rng(seed);
	dvector epsilon(a,A);
	epsilon.fill_randn(rng);
	dvector x = log(p)+tau2*epsilon;
	x -= mean(x);
	return exp(x)/sum(exp(x));
}

dvariable dmultinom(const dmatrix o, const dvar_matrix& p,dvar_matrix& nu,double& tau2,const double minp)
{	// returns the negative loglikelihood
	/*
     uses Martell dmvlogistic code for grouping age classes
     with observed proportions < minp
     NB minp must be greater than 0, otherwise algorithm returns
     an error if one of the observed proportions is zero.
     tau2 returns the median absolute standardized residual

	FIX ME SM I'm getting an array out of Bounds error in here for gear3
		has to do with the if statement (if minp >1.-4) because Ncount is only
		1.  I've commented the if statement out for now.
	*/
	RETURN_ARRAYS_INCREMENT();
	int i,j,k,n;
	int a = o.colmin();
	int A=o.colmax();
	int t=o.rowmin();
	int T=o.rowmax();
	dvector tmptau(1,A*T);	// vector of residuals
    int Ncount=1;
    dvariable Nsamp;           // multinomial sample size
    dvariable proc_err=0.009;   // allow for process error in the pred.age freq...fixed value based on HCAM assessments
	nu.initialize();
	dvariable nloglike=0.;
    //ofstream ofs("check.tmp");

	for(i=t; i<=T; i++)
	{
		//cout<<"Ok to here 1"<<endl;
		Nsamp=sum(o(i))/(1.+proc_err*sum(o(i)));
		n=0;
		dvector oo = o(i)/sum(o(i));
		dvar_vector pp = p(i)/sum(p(i));

		//count # of observations greater than minp (2% is a reasonable number)
		for(j=a;j<=A;j++)
			if(oo(j) > minp)n++;

		ivector iiage(1,n);
		dvector o1(1,n); o1.initialize();
		dvar_vector p1(1,n); p1.initialize();
		k=1;
		for(j=a;j<=A;j++)
		{
			if(oo(j)<=minp)
			{
				o1(k)+=oo(j);
				p1(k)+=pp(j);
			}
			else
			{
				o1(k)+=oo(j);
				p1(k)+=pp(j);
				if(k<=n)iiage(k)=j;		//ivector for the grouped residuals
				if(k<n) k++;
			}
		}
		/*
		//assign residuals to nu based on iiage index
		dvar_vector t1 = elem_div(o1-p1,sqrt(elem_prod(p1,1.-p1)/Nsamp));
		for(j=1;j<=n;j++)
		{
			nu(i)(iiage(j))=t1(j);
			tmptau(Ncount++)=sqrt(square(value(t1(j))));
		}
		*/

		//CHANGED Later addition from Viv to prevent crashes if
		//min(p1) is very small.
		//if(min(p1)>1e-4)
		{
			dvar_vector t1 = elem_div(o1-p1,sqrt(elem_prod(p1,1.-p1)/Nsamp));
			for(j=1;j<=n;j++)
			{
				nu(i)(iiage(j))=t1(j);
				tmptau(Ncount++)=sqrt(square(value(t1(j))));
			}
		}
		//end of addition.
		// negative log Mulitinomial with constant is:
		// r = -1.*(gammln(Nsamp+1)+sum(Nsamp*o1(log(p1))-gammln(Nsamp+1)));
		// TODO add calculation for effective sample size.
		/*
			TODO Neff = sum(elem_prod(p1,1.-p1))/sum(square(o1-p1));
			for each year.  Plot the Nsamp vs Neff and look for a 1:1 slope.
		*/

		nloglike+=sum(-1.*elem_prod(Nsamp*o1,log(p1))+
					elem_prod(Nsamp*o1,log(o1)));
		//cout<<"Ok to here 2"<<endl;
	}

	dvector w=sort(tmptau(1,Ncount-1));
	//cout<<"All good "<<Ncount<<endl;
	tau2=w(int(Ncount/2.)); //mle of the variance

	RETURN_ARRAYS_DECREMENT();
	return(nloglike);
}


//robust normal approximation to the multinomial distribution
dvariable multifan(const dmatrix oprop,const dvar_matrix& pprop, const int& Nsamp)
{	//Vivian Haist.
    dvariable extra=0.1/14.;
    dvar_matrix resid=elem_div((oprop-pprop),sqrt((elem_prod(pprop,1.-pprop)+extra)/Nsamp));
    return sum(0.5*log(elem_prod(pprop,1. -pprop)+extra) -log(mfexp(-0.5*elem_prod(resid,resid))+0.01));
}

dvariable multifan(const int& n, const dmatrix obs, const dvar_matrix pred,double& nef)
{
	int A=obs.colmax()-obs.colmin()+1;
	//dvar_matrix xi=(elem_prod(1.-pred,pred)+0.1/A)/n; //variance from Fourniers paper.
	dvar_matrix xi=(elem_prod(1.-obs,obs)+0.1/A)/n;	 //variance from the multifanCL manual.
	dvar_matrix resid=obs-pred;
	nef=value(sum(elem_prod(1.-pred,pred))/sum(elem_prod(resid,resid)));
	return sum(0.5*log(2.*pi*xi)-log(mfexp(-0.5*elem_div(elem_prod(resid,resid),xi))+0.01));
}

dvariable multifan(const double& s,const dvector obsQ,const dvar_vector& preQ, double& nmle)
{
	//using Fournier's robust likelihood for length frequency data.
	//s is the sample size
	//neff is the sample size limit  This seems to be fucked...
	//RETURN_ARRAYS_INCREMENT();
	double pi=3.141593;
	dvariable like;
	dvariable tau;
	int lb=obsQ.indexmin();
	int nb=obsQ.indexmax();

	dvar_vector epsilon(lb,nb);
	dvar_vector Q=obsQ/sum(obsQ);
	dvar_vector Qhat=preQ/sum(preQ);

	//dvariable nmle;		//effective sample size
	nmle=value(sum(elem_prod(Qhat,1.-Qhat))/norm2(Q-Qhat));
	cout<<nmle<<endl;
	tau=1./s;
	epsilon=elem_prod(1.-Qhat,Qhat);

	like=0.5*sum(log(2.*pi*(epsilon+0.1/nb)))+nb*log(sqrt(tau));
	like+= -1.*sum(log(mfexp(-1.*elem_div(square(Q-Qhat),2.*tau*(epsilon+0.1/nb)))+0.01));
	//RETURN_ARRAYS_DECREMENT();
	return like;
}

dvar_matrix ALK(dvar_vector mu, dvar_vector sig, dvector x)
{
	//RETURN_ARRAYS_INCREMENT();
	int i, j;
	dvariable z1;
	dvariable z2;
	int si,ni; si=mu.indexmin(); ni=mu.indexmax();
	int sj,nj; sj=x.indexmin(); nj=x.indexmax();
	dvar_matrix pdf(si,ni,sj,nj);
	pdf.initialize();
	double xs=0.5*(x[sj+1]-x[sj]);
	for(i=si;i<=ni;i++) //loop over ages
	{
		 for(j=sj;j<=nj;j++) //loop over length bins
		{
			z1=((x(j)-xs)-mu(i))/sig(i);
			z2=((x(j)+xs)-mu(i))/sig(i);
			pdf(i,j)=cumd_norm(z2)-cumd_norm(z1);
		}//end nbins
		//pdf(i)/=sum(pdf(i));
	}//end nage
	pdf/=sum(pdf);
	//RETURN_ARRAYS_DECREMENT();
	return(pdf);
}

dvector pearson_residuals(long m, dvector obs_p, dvector pred_p)
{
	{
		dvector O=obs_p/sum(obs_p);
		dvector P=pred_p/sum(pred_p);

		//double neff;		//effective sample size
		//neff=norm(elem_prod(pred_p,1.-pred_p))/norm2(obs_p-pred_p);
		dvector var=elem_prod(P,(1.-P))/m;
		//max(var)<=0 ? var=1.: var=var;
		if(max(var)<=0) var=1;
		dvector r=elem_div(O-P,sqrt(var+0.01/14));
		if(sum(P)==0) r=0;
		return(r);
	}
}

dvar_vector eplogis(const dvar_vector& x, const dvariable& alpha, const dvariable& beta, const dvariable& gamma)
{
	//exponential logistic based on Grant Thompson (1994) Paper, CJFAS.
	return (1./(1.-gamma))*pow((1.-gamma)/gamma,gamma)*elem_div(exp(alpha*gamma*(beta-x)),1.+exp(alpha*(beta-x)));
}

dvector eplogis(const dvector& x, const double& alpha, const double& beta, const double& gamma)
{
	//exponential logistic based on Grant Thompson (1994) Paper, CJFAS.
	return (1./(1.-gamma))*pow((1.-gamma)/gamma,gamma)*elem_div(exp(alpha*gamma*(beta-x)),1.+exp(alpha*(beta-x)));
}


