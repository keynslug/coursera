Question 1
==========

Your goal this week is to write a program to compute discrete log modulo _a_ prime _p_. Let ![g](http://latex.codecogs.com/png.latex?{g}) be some element in ![Z-p-star](http://latex.codecogs.com/png.latex?%5cmathbb{Z}_{p}^{*}) and suppose you are given ![h in Z-p-star](http://latex.codecogs.com/png.latex?{h}%5ctext{&space;in&space;}%5cmathbb{Z}_{p}^{*}) such that ![h=g^x](http://latex.codecogs.com/png.latex?{h}={g}^{x}) where ![x](http://latex.codecogs.com/png.latex?{1}%5cleq{x}%5cleq{2}^{40}). Your goal is to find ![x](http://latex.codecogs.com/png.latex?{x}). More precisely, the input to your program is ![p,g,h](http://latex.codecogs.com/png.latex?{p},{g},{h}) and the output is ![x](http://latex.codecogs.com/png.latex?{x}).

The trivial algorithm for this problem is to try all ![2^40](http://latex.codecogs.com/png.latex?{2}^{40}) possible values of ![x](http://latex.codecogs.com/png.latex?{x}) until the correct one is found, that is until we find an ![x](http://latex.codecogs.com/png.latex?{x}) satisfying ![h=g^x in Z-p-star](http://latex.codecogs.com/png.latex?{h}={g}^{x}%5ctext{&space;in&space;}%5cmathbb{Z}_{p}^{*}). This requires ![2^40](http://latex.codecogs.com/png.latex?{2}^{40}) multiplications. In this project you will implement an algorithm that runs in time roughly ![2^20](http://latex.codecogs.com/png.latex?%5csqrt{{2}^{40}}={2}^{20}) using a meet in the middle attack.

Let ![B=2^20](http://latex.codecogs.com/png.latex?{B}={2}^{20}). Since ![x](http://latex.codecogs.com/png.latex?{x}) is less than ![B^2](http://latex.codecogs.com/png.latex?{B}^{2}) we can write the unknown ![x](http://latex.codecogs.com/png.latex?{x}) base
![B](http://latex.codecogs.com/png.latex?{B}) as ![x=x0*B+x1 where x0, x1 are in range {0,B−1}](http://latex.codecogs.com/png.latex?{x}={x}_{0}*{B}&plus;{x}_{1}%5ctext{&space;where&space;}{x}_{0},{x}_{1}%5cin&space;[{0},{B-1}]). Then

![h=g^x=g^(x0B+x1)=(g^B)^x0*g^x1 in Z-p](http://latex.codecogs.com/png.latex?{h}={g}^{x}={g}^{{x}_{0}{B}&plus;{x}_{1}}=%28{g}^{B}%29^{{x}_{0}}%5ccdot{g}^{{x}_{1}}%5ctext{&space;in&space;}%5cmathbb{Z}_{p})

By moving the term ![g^x1](http://latex.codecogs.com/png.latex?{g}^{{x}_{1}}) to the other side we obtain

![h/g^x1=(g^B)^x0 in Z-p](http://latex.codecogs.com/png.latex?{h}/{g}^{{x}_{1}}=%28{g}^{B}%29^{{x}_{0}}%5ctext{&space;in&space;}%5cmathbb{Z}_{p})

The variables in this equation are ![x0,x1](http://latex.codecogs.com/png.latex?{x}_{0},{x}_{1}) and everything else is known: you are given ![g, h and B=2^20](http://latex.codecogs.com/png.latex?{g},{h}%5ctext{&space;and&space;}{B}={2}^{20}). Since the variables ![x0 and x1](http://latex.codecogs.com/png.latex?{x}_{0},{x}_{1}) are now on different sides of the equation we can find a solution using meet in the middle (Lecture 3.3):

 - First build a hash table of all possible values of the left hand side ![h/g^x1](http://latex.codecogs.com/png.latex?{h}/{g}^{{x}_{1}}) for ![x1=0,1,...,2^20](http://latex.codecogs.com/png.latex?{x}_{1}=0,1,2,%5cdots,{2}^{20}).
 - Then for each value ![x0=0,1,...,2^20](http://latex.codecogs.com/png.latex?{x}_{0}=0,1,2,%5cdots,{2}^{20}) check if the right hand side ![(g^B)^x0](http://latex.codecogs.com/png.latex?%28{g}^{B}%29^{{x}_{0}}) is in this hash table. If so, then you have found a solution ![(x0,x1)](http://latex.codecogs.com/png.latex?%28{x}_{0},{x}_{1}%29) from which you can compute the required ![x](http://latex.codecogs.com/png.latex?{x}) as ![x=x0*B+x1](http://latex.codecogs.com/png.latex?{x}={x}_{0}{B}&plus;{x}_{1}).

The overall work is about ![B=2^20](http://latex.codecogs.com/png.latex?{B}={2}^{20}) multiplications to build the table and another ![B=2^20](http://latex.codecogs.com/png.latex?{B}={2}^{20}) lookups in this table.

Now that we have an algorithm, here is the problem to solve:

```
p = 13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084171
g = 11717829880366207009516117596335367088558084999998952205599979459063929499736583746670572176471460312928594829675428279466566527115212748467589894601965568
h = 3239475104050450443565264378728065788649097520952449527834792452971981976143292558073856937958553180532878928001494706097394108577585732452307673444020333
```

Each of these three numbers is about 153 digits. Find `x` such that ![h=g^x in Z-p](http://latex.codecogs.com/png.latex?{h}={g}^{x}%5ctext{&space;in&space;}%5cmathbb{Z}_{p}).

To solve this assignment it is best to use an environment that supports multi-precision and modular arithmetic. In __Python__ you could use the __gmpy2__ or __numbthy__ modules. Both can be used for modular inversion and exponentiation. In __C__ you can use __GMP__. In __Java__ use a `BigInteger` class which can perform `mod`, `modPow` and `modInverse` operations.
