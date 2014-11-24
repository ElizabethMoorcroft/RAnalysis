
pickmodel<-function(sensor,call){
	if(sensor<pi & call<pi){ # SW
		if(sensor<pi/2){ # SW4-9
			if(call<pi-2*sensor){ # SW 7-9
				if(sensor<call){ model<-"SW7"}
				else if(sensor>2*call){model <-"SW9"}
				else{model<-"SW8"}
			}else{ # SW4-6
				if(sensor<call){ model<-"SW4"}
				else if(sensor>2*call){model <-"SW5"}
				else{model<-"SW6"}				
			}
		}
		else{ # SW1-3
			if(call>sensor){model<-"SW1"}
			else if(call<2*sensor-pi){model<-"SW3"}
			else{model<-"SW2"}
		}	
	}
	
	####
	
	else if(sensor>=pi & call<pi){ # SE 1-4
		if(call<2*pi-sensor){model<-"SE4"}
		else if (call<4*pi-2*sensor){model<-"SE3"}
		else if(sensor>=2*pi-0.0001){model<-"SE1"}
		else{model<-"SE2"}
	}
	
	####
	
	else if (call>=pi & sensor<pi){ #NW 1-NW7 +REM
		if(sensor<pi/2){ # NW5-7 + REM
			if(call<2*pi-2*sensor){model<-"NW7"}
			else if(call<2*pi-sensor){model<-"NW6"}
			else if(call>=2*pi-0.0001){model<-"REM"}
			else {model<-"NW5"}
		}
		else{ #NW1-4
			if(call<2*pi-sensor){model<-"NW4"}
			else if(call<3*pi - 2*sensor){model<-"NW3"}
			else if(call>=2*pi-0.0001){model<-"NW1"}
			else{model<-"NW2"}
		}	
	}
	
	####
	
	else{ # NE1-3 +Gas
		if (call< 4*pi-2*sensor){model<-"NE3"}
		else if (call< 3*pi - sensor){model <-"NE2"}
		else if(sensor>=2*pi-0.0001){model<-"Gas"}
		else{model<-"NE1"}
		
	}
	return(model)
}