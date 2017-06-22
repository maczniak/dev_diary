# [Fundamentals of Physics: Mechanics, Relativity, and Thermodynamics][homepage] by R. Shankar, Yale University Press (2014)

[PHYS 200: Fundamentals of Physics I][phys_200] (with videos,
 [Open Yale Courses][open_yale_courses], [Books][open_yale_courses_books])

"Basic Training in Mathematics: A Fitness Program for Science Students"
 (Springer, 2008) of the same author

[homepage]: http://yalebooks.com/book/9780300192209/fundamentals-physics
[phys_200]: http://oyc.yale.edu/physics/phys-200
[open_yale_courses]: http://oyc.yale.edu/
[open_yale_courses_books]: http://yalebooks.com/series/the-open-yale-courses-series

## 1. The Structure of Mechanics

*v*<sup>2</sup> - *v*<sub>0</sub><sup>2</sup> = 2*a*(*x* - *x*<sub>0</sub>)

## 2. Motion in Higher Dimensions

## 3. Newton's Law I

If Newton's law of inertia works for you, you are called an *inertial observer*
 and your frame of reference is called an *inertial frame*. You must know the
 earth is not precisely inertial. The earth has an acceleration. But the
 acceleration due to motion around the sun at speed *v* and radius *r* is *a* =
 *v*<sup>2</sup>/*r* = .006 *ms*<sup>-2</sup>, which is a very small number, say
 compared to *g*. The same goes for the acceleration due to the earth's rotation
 about its own axis, which is roughly .03*ms*<sup>-2</sup> or roughly *g*/300 at
 the equator. It's a useful concept even on the earth, because the earth is
 approximately inertial.<br>
You should know at all times how you measure anything that enters your theory or
 calculation. If not, you are just doing math or playing with symbols. You are
 not doing physics. This discussion (using a spring) also tells you that the
 mass of an object has nothing to do with gravitation but with how much it hates
 to accelerate in response to a force.<br>
free-body diagrams<br>
Every [massless] spring/rope is pushed or pulled by equal and opposite forces
 ±*F* at the two ends since otherwise it would have *a* = *F*/0 = ∞.

## 4. Newton's Law II

You might expect more fiction because there's more contact. But that is not so,
 for reasons not readily explained within our elementary treatment.<br>
I use *μ<sub>s</sub>*, the static coefficient, and not *μ<sub>k</sub>*, the
 kinetic one, even though the car is moving, because we are discussing the force
 in the radial direction and the car has no velocity in that direction, unless
 it is skidding.

## 5. Law of Conservation of Energy

*K*<sub>2</sub> - *K*<sub>1</sub> =
 ∫<sub>*x*<sub>1</sub></sub><sup>*x*<sub>2</sub></sup> *F*(*x*)*dx* =
 *G*(*x*<sub>2</sub>) - *G*(*x*<sub>1</sub>) ≡ *G*<sub>2</sub> -
 *G*<sub>1</sub>, *U*(*x*) = -*G*(*x*), *F*(*x*) = -*dU*/*dx*<br>
I cannot evaluate the integral of *f* (the force of friction) given just
 *x*<sub>1</sub> and *x*<sub>2</sub>, because it can go from *x*<sub>1</sub> to
 *x*<sub>2</sub> directly or after one or more oscillations.<br>
Divide the forces into *conservative* forces (gravity, spring) that depend on
 location only and the non-conservative like friction (which is the only
 non-conservative force we will consider). The integral over each conservative
 force will turn into an associated potential energy difference. Let
 *W*<sub>*f*</sub> be the work done by friction. *E*<sub>2</sub> -
 *E*<sub>1</sub> = *W*<sub>*f*</sub> with *E* = ½*mv*<sup>2</sup> +
 *U*<sub>*s*</sub> + *U*<sub>*g*</sub> + ...

## 6. Conservation of Energy in *d*=2

I have shown you that if we took a random force, the work done is dependent on
 the path. For this *non-conservative force*, you cannot define a potential
 energy, whereas in one dimension any force other than friction allowed you to
 define a potential energy.<br>
Our quest for a conserved energy leads us to search for a *conservative force*,
 a force for which the work done in going from 1 to 2 is path-independent. Take
 any function *U*(*x*,*y*). The corresponding conservative force is F =
 -**i**∂*U*/∂*x* - **j**∂*U*/∂*y*. Are there other ways to manufacture the
 conservative force? No! If **F** is conservative, ∂*F*<sub>*x*</sub>/∂*y* =
 ∂*F*<sub>*y*</sub>/∂*x*.<br>
However, according to laws of quantum mechanics, a particle with energy
 *E*<sub>2</sub> can disappear from the region *BC* and tunnel to *D* with the
 same energy.<br>
There is **F**<sub>*T*</sub>, the normal force of the track. Luckily, this
 normal force does no work, because **F**<sub>*T*</sub>⋅*d***r** = 0 in every
 portion.

## 7. The Kepler Problem

First of all, planets are not moving just under the influence of the sun but
 also other planets, especially Jupiter. Secondly, the Newtonian law of
 gravitation has been modifed by Einstein's general theory of relativity. Both
 these effects prevent the orbit from being closed. The major axis slowly
 rotates with time, and this is called *precession of the perihelion*, the
 effect being most pronounced for Mercury. After all the corrections explicable
 in Newtonian terms, a tiny amount, 43 degrees of an arc (1/3600 of a degree)
 per century, remained unexplained. The general theory of relativity explained
 that last of discrepancy.<br>
What is not obvious, but true, is that the gravitational force will be zero
 inside the *entire* shell.<br>
*m* *d*<sup>2</sup>**r**/*dt*<sup>2</sup> = -*GMm*/*r*<sup>2</sup>
 **e**<sub>*r*</sub>. This is now a problem in calculus, and everything Kepler
 said should come out of its solution. Even centuries later, I find it takes the
 class in advanced mechanics quite an effort to solve this equation. But Newton
 did all that hundreds of years ago. We think we know the underlying equations
 of motion and forces between quarks. But we do not yet have a way to show,
 *analytically*, that the underlying equations imply the phenomenon or particles
 that we see. However, by solving them approximately on big computers, we are
 fairly certain, after years of work, that the equations are correct.<br>
*U*(*r*) = -*GMm*/*r*<sup>3</sup>
If you see a comet and want to know if it will come back again, add the kinetic
 and potential engergies. If it's positive, it won't come back; if it's
 negative, the comet is trapped. Zero is the dividing line, when the comet will
 collapse at the finish line at infinity.<br>
It should stop growing once the orbit size passes the observed radius of the
 matter in the galaxy. But it keeps growing for a considerable distance beyond,
 telling us there is a halo of dark matter. Even dark matter cannot hide its
 gravitational effect. Every galaxy seems to have a dark matter halo that
 extends beyond the visible part.

## 8. Multi-particle Dynamics

(center-of-mass coordinate, CM) All the internal forces have canceled out, and
 what remains is the external force.<br>
My earlier description of the sun sitting still and the earth moving around it
 is not acceptable. The CM of tne earth and sun should not accelerate.<br>
The boat will keep moving because there's no force on the boat; it will keep
 moving. The answer is that an external force has now come into play: the force
 of friction between you and the earth.<br>
That's where there are two extreme cases. One is called the *totally inelastic
 collision* in which the two masses stick together and move at a common velocity
 *v*'<sub>1</sub> = *v*'<sub>2</sub> = *v*'. The other kind of collision is
 called *totally elastic*. Here the kinetic energy is conserved. You cannot use
 the law of conservation of energy in an inelastic collision.<br>
which may be rewritten in terms of momenta as
 *p*<sub>1</sub><sup>2</sup>/2*m*<sub>1</sub> +
 *p*<sub>2</sub><sup>2</sup>/2*m*<sub>2</sub> =
 *p'*<sub>1</sub><sup>2</sup>/2*m*<sub>1</sub> +
 *p'*<sub>2</sub><sup>2</sup>/2*m*<sub>2</sub>

## 9. Rotational Dynamics I

(moment of inertia) *K* = ½*ω*<sup>2</sup>Σ*mr*<sup>2</sup> ≡ ½*Iω*<sup>2</sup>
 (½*mv*<sup>2</sup>)<br>
(angular momentum) *L* = *Iω* (*mv*)<br>
(torque) *τ* = *Iα* = Σ*F*<sub>*T*</sub>*r* (*ma*)<br>
*dW* = *F*<sub>*T*</sub>*ds* = *τdθ*

## 10. Rotational Dynamics II

The parallel axis theorem: *I* = *I*<sub>*CM*</sub> + *Md*<sup>2</sup><br>
*K* = *K*<sub>*CM*</sub> + *K*<sub>*rot*</sub>

## 11. Rotational Dynamics III

## 12. Special Relativity I: The Lorentz Transformation

## 13. Special Relativity II: Some Consequences

## 14. Special Relativity III: Past, Present, and Future

## 15. Four-momentum

## 16. Mathmatical Methods

## 17. Simple Harmonic Motion

## 18. Waves I

## 19. Waves II

## 20. Fluids

## 21. Heat

## 22. Thermodynamics I

## 23. Thermodynamics II

## 24. Entropy and Irreversibility

