################################################
#####       Bat Abundance estimates       ######
#####       Tim Lucas                     ######
################################################

# Units are:
# Intensity in decibels
# Length in metres
# frequency in hertz (NB NOT kilohertz)
# temperature in celsius
# humidity in percent saturation
# angles in radians
# velocity in metres per second
# area in metres squared


# A function to calculate sound attenuation (alpha dB/m) from frequency (in hertz), humidity and temperature (celsius).
Attenuation <-  function(f,temp,hum=30){
                  T <- 273.15+temp  # Convert celsius to kelvin
                  T01 <- 273.16
                  T0 <- 293.15
                  psat_ps0 <- 10^{ -6.8346*{T01/T}^1.261+4.6151} # saturation pressure ratio from ISO   
                  h <- hum*psat_ps0/1
                  c0 <- 331 # reference sound speed
                  c_iso <- {1+0.16*h}*c0*sqrt(T/T01)
                  taur <- T/T0
                  fr0 <- {24+40400*h*{0.02+h}/{0.391+h}}
                  frN <- {taur}^{-1/2}*{9+280*h*exp(-4.17*{{taur}^{-1/3}-1})};

                  b1 <- 0.1068*exp(-3352/T)/{frN+f^2/frN};
                  b2 <- 0.01275*exp(-2239.1/T)/{fr0+f^2/fr0};

                  alpha <- 8.686*f^2*taur^{0.5}*{1.84*10^{-11}/1+taur^{-3}*{b1+b2}}
                  return(alpha)
                }
            	
# Calculate the radius of detection from attenuation, source level and threshold intensity
# 'its' is the number of iterations for the newton-raphson numerical method. 6 will generally be accurate to >10 decimal places
Radius   <-     function(f,temp, I0, It,hum=30,its=6){
		  attenuation <- Attenuation(f,temp,hum)
		  est <- 10 # First guess. Smooth declining function so not an important choice.
	 	  for(i in 1:its){ # Newton-raphson
		    est <- est - {I0-It-attenuation*est-20*log10(10*est)}/{-attenuation-{20}/{log(10)*est}}
		  }
                  return(est)
                }

# Convert degrees to radians
Rads <- function(angle){
          angle <- angle*pi/180 # Convert degrees to radians
          return(angle)
        }

# For time-expansion recorders (defaults is with an expansion of x10). Compute actual recording time from survey time.
RecordTime <- function(time,expansion=10){
                new.time <- time/{expansion+1}
                return(new.time)
              }

# Calculate call angle (theta_a) from half angle in radians, source level and threshold intensity                
CallAngle <-     function(half.angle, I0, It){
                  call.angle <- 4*half.angle*{I0-It}/I0
                  return(call.angle)
                }          

# Calculate abundance from density (animals per metre^2) and area (m^2).
Abund    <-     function(density, area){
                  abundance <- density*area
                  return(abundance)
                }


                
# Calculate density with an immobile detector. Angles in radians.
ImmobileDens <- function(captures, time, cam.angle, call.angle, r, v, sig=0.05){
                  d <- cam.angle
                  a <- call.angle
                  q <- r*v*{ cos(d)+sin(d/2)*sin(a/2)-cos(a/2+d)}/pi # calculate area covered per unit time
                  D <- captures/{time*q} # Calculate density
                  low <- qchisq(sig/2,2*captures)/{2*time*q} # lower confidence limit
                  high <- qchisq(1-sig/2,2*captures+2)/{2*time*q} # upper confidence limit
                  return(list(Density=D,LowerConf=low,UpperConf=high))
                }



# Calculate density with unequal speeds for detector and animal.
# M is the number of points to calculate in numeric solutions
UnequalDens  <- function( captures, time, cam.angle, call.angle, r, va, vd, M=10000 ){
                   a  <- call.angle
                   d  <- cam.angle
                   m1 <- seq({pi-d}/2,pi/2,length.out=M)
                   m2 <- seq(d,d+a/2,length.out=M)
                   q1 <- sin(d/2)*{r*d}/{2*pi*{M-1}}
                   q2 <- sum( sin(m1[2:M])*sqrt( va^2+vd^2-2*va*vd*cos(pi/4+m1[2:M]/2) ) + sin(m1[1:{M-1}])*sqrt(va^2+vd^2-2*va*vd*cos(pi/4+m1[1:{M-1}]/2) ) )
                   q3 <- {a*r}/{4*pi*{M-1}}
                   q4 <- sum( sin(m2[2:M])*sqrt( va^2+vd^2-2*va*vd*cos(pi/2+d/4-m2[2:M]/2) ) + sin(m2[1:{M-1}])*sqrt( va^2+vd^2-2*va*vd*cos(pi/2+d/4-m2[1:{M-1}]/2) ) ) # numerical integration to calculate area covered
                   D  <- captures/{time*{q1*q2+q3*q4}}  # Calculate density
                   return(D)
                }


