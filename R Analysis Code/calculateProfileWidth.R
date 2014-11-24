calcProfileWidth <- function(theta_a, theta_s, r){
        if(theta_a > 2*pi | theta_a < 0) 
		stop('theta_a is out of bounds. theta_a should be in 0<a<2*pi')
        if(theta_s > 2*pi | theta_s < 0) 
		stop('theta_s is out of bounds. theta_s should be in 0<a<2*pi')

	if(theta_a > pi){ # Animal call is greater than pi
	        if(theta_a < 4*pi - 2*theta_s){ 
		        	p <- r*(theta_s - cos(theta_a/2) + 1)/pi
		        	x<-"Model 1"
                } else if(theta_a <= 3*pi - theta_s){
                        p <- r*(theta_s - cos(theta_a/2) + cos(theta_a/2 + theta_s))/pi
                        x<-"Model 2"
                } else {
                        p <- r*(theta_s + 2*sin(theta_s/2))/pi
                        x<-"Model 3"
                }
        } else { # Animal call is less than pi
        	if(theta_a < 4*pi - 2*theta_s){ # Less than the p322 line
                        p <- r*(theta_s*sin(theta_a/2) - cos(theta_a/2) + 1)/pi
                        x<-"Model 4"
 		} else {# Greater than the p322 line
                        p <- r*(theta_s*sin(theta_a/2) - cos(theta_a/2) + cos(theta_a/2 + theta_s))/pi
                        x<-"Model 5"
                }
        }
        return(list(p,x))
}