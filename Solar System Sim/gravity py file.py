import numpy as np
import matplotlib.pyplot as plt
import unyt as u
import numpy.testing as npt

Mercury=(0.330104*10**24,0.38709843,0.20563661,19.08833658*(np.pi/180),77.45771895*(np.pi/180))
Venus=(4.86732*10**24,0.72332102,0.00676399,286.68776598*(np.pi/180),131.76755713*(np.pi/180))
Earth=(5.97219*10**24,1.00000018,0.01673163,331.16416467*(np.pi/180),102.93005885*(np.pi/180))
Mars=(0.641693*10**24,1.52371243,0.09336511,326.20022408*(np.pi/180),-23.91744784*(np.pi/180))
Jupiter=(1898.13*10**24,5.20248019,0.04853590,242.48991503*(np.pi/180),14.27495244*(np.pi/180))
Saturn=(568.319*10**24,9.54149883,0.05550825,281.55831164*(np.pi/180),92.86136063*(np.pi/180))
Uranus=(86.8103*10**24,19.18797948,0.04685740,29.32733579*(np.pi/180),172.43404441*(np.pi/180))
Neptune=(102.41*10**24,30.06952752,0.00895439,351.14513741*(np.pi/180),46.68158724*(np.pi/180))


#my_planet=(M,a,eps,phi,omega)




def get_planet_r(planet):
    """Arguments: planet=(M,a,eps,phi,omega)
          ======================
       Returns distance from planet to sun
    """

    a=planet[1] #Splitting planet tuple to get a,eps,phi, and omega
    eps=planet[2]
    phi=planet[3]
    omega=planet[4]
    
    position_sun=(a*(1-eps**2))/(1+eps*np.cos(phi-omega)) # Equation for distance from planet to sun
    return position_sun

def get_planet_coords(planet): 
    """Argument: planet=(M,a,eps,phi,omega)
            =================
       returns planet position in cartesian Coordinates 
    """
    x=get_planet_r(planet)*np.cos(planet[3]) # from cylindrical to cartesian coords 
    y=get_planet_r(planet)*np.sin(planet[3])
    z=0
    return np.array([x,y,z])


def get_planet_orbit(planet, phi_linspace):
    """Argument: planet=(M,a,eps,phi,omega), tuple of phi values
                   =================
        returns x and y position arrays
    """
    
     #insert initial phi value into phi array
    Coords1=[] #x and y empty array
    Coords2=[]
    xy=[planet[0],planet[1],planet[2],phi_linspace,planet[4]] #Combining planet with phi linspace
    for i in range(len(phi_linspace)):   #iterate over phi linspace to get all x and y values 
        x=get_planet_coords([xy[0],xy[1],xy[2],xy[3][i],xy[4]])
        x_array=x[0]
        y_array=x[1]
        Coords1.append(x_array) #insert values into array 
        Coords2.append(y_array)
    return Coords1,Coords2

def update_planet_position(planet,dt): 
    """Argument: planet, and dt
             ===========
       returns new phi position 
    """
    a=planet[1]
    eps=planet[2]
    phi=planet[3]
    omega=planet[4]
    G=6.67*10**(-11)
    M_dot=1.9885*10**30
    r=get_planet_r(planet)

    Kepler_Second=(G*M_dot*a*1.496e11*(1-eps**2))**(1/2)/(1.496e11*r)**2   #Usng Kepler Second Law to Calculate angular velocity
    d_phi=Kepler_Second*dt            #New phi is angular velocity times time
    New_phi=phi+d_phi                 #New phi is initial plus the change in phi
    return (planet[0],a,eps,New_phi,omega)
    

def update_all_planets(planets,dt): 
    """Arguments:planets and time dt
           ==========
        returns Final position for all planets
    """
    Final_position=[]   #Final position array
    for i in range(len(planets)):  #for loop to iterate over all planets
        update_positions=update_planet_position(planets[i],dt) #calling planet position with to iterate over i
        Final_position.append(update_positions)
    return Final_position 

def accel_g_sun(vec_r): 
    """Arguments:vec_r=numpy array [x,y,z
          ========
       returns vector componets of acceleration due to sun
    """
    
    G=6.674*10**-11*6.68459e-12**3
    Mag_vec_r=np.dot(vec_r,vec_r)**(1/2)   #Magnitude of vector R
    M_sun=1.989*10**30
    coefficient=-G*M_sun/Mag_vec_r**3 #coefficient of vector
    Final=vec_r*coefficient
    return Final
        
    

def accel_g_planet(vec_r,planet):
    """Arguments: vec_r and planet tuple
               ==========
      returns vector of gravitational acceleration due to planet
      [ax,ay,az]
    """
    
    G=6.674*10**-11*6.68459e-12**3
    M=planet[0]
    R=get_planet_coords(planet) #Radius of planet from sun
    dr=vec_r-R
    Mag_dr=np.dot(dr,dr)**(1/2) #Magnitude of vec_r and R of planet
    coefficient=-G*M/(Mag_dr)**3
    Final=coefficient*dr
    return Final

def update_position(vec_r,vec_v,planets,dt): 
    
    """
    Arguments: vecotr position r, velocity vector, list of planets, time dt 
                         ============
          returns new vecotr position r and new velocity vector in two arrays
          [rx,ry,rz] and [vx,vy,vz] 
    """

    accel_planet=[]
    accel_sun=accel_g_sun(vec_r)
    planet = np.array(planets)
    for i in (planet):                     #Iterate over planet list to get all planet accelerations
        x=accel_g_planet(vec_r,i)
        accel_planet.append(x)
    Final_accel=sum(accel_planet)         #sum up all planet accelerations
    Total_accel=accel_sun+Final_accel     #Total acceleration of sun acceleration plus total planet acceleration 
    vec_v_new=Total_accel*dt+vec_v        #New velocity vector 
    vec_r_new=vec_v_new*dt+vec_r          #New position vecotr
    
    return vec_r_new,  vec_v_new

def get_planet_distances(vec_r,planets):
    
    """Arguments: vector position r and list of planets
                   ============
       returns distance between the planet and position r 
    """
    
    vec_planet=[]
    for i in (planets):           #iterates over the whole planet list 
        x=get_planet_coords(i)
        dx=vec_r-x                  #difference between position vector and planet vector 
        distance=np.dot(dx,dx)**(1/2)     #gets magnitude of the new position vector
        
        vec_planet.append(distance)      
    return vec_planet


def parse_line_csv(line):
    
    """Arguments: data file line
           ============
       returns list of the file into a float object
    """
    
    line.strip()
    sample2=line.split(',')
    sample=[]
    for i in sample2:
        sample.append(float(i))   
    return sample


def API():
    
    #Test 1: Acceleration of Sun on Earth is approximatley 4.01e-14 Au/s^2
    Earth_position=get_planet_coords(Earth)
    A_vec=accel_g_sun(Earth_position) #Earth's Acceleration due to sun Test
    Sun_Earth_accel=np.dot(A_vec,A_vec)**(1/2)
    npt.assert_allclose(4.01e-14,Sun_Earth_accel, atol=2e-15)
    
    #Test 2: Distance between Earth and Mars is between 0.36Au and 1.5Au
    Distance_EM=get_planet_distances(Earth_position,[Mars]) #Distance from Earth to Mars: compared to minimum and maximum distance 
    npt.assert_allclose(0.93, Distance_EM, atol=0.57)#0.93 average distance, error is between maximum and minimum distances
    
    #Test 3: Distance of Earth from Sun is approximately 1 Au
    Earth_from_sun=get_planet_r(Earth)    #Earth distance from sun 
    npt.assert_allclose(1, Earth_from_sun, atol=0.02) #should be between 0.98 and 1.02 Au 
    
    #Test 4: Acceleration of Earth is approximately 9.8m/s^2 or 6.56e-11 Au/s^2
    Surface_Earth=get_planet_coords(Earth)+np.array([4.259013e-5,0,0]) #Position vector of Surface of the Earth
    Earth_accel_array=accel_g_planet(Surface_Earth,Earth)    #Acceleration vector at Earth Surface
    Earth_Mag_accel=np.dot(Earth_accel_array,Earth_accel_array)**(1/2) #Magintude of Earth acceleration vector
    npt.assert_allclose(6.55089538e-11, Earth_Mag_accel, atol=0.1)
    
    #Test 5: Sun like planet matches Sun Acceleration 
    Sun_planet=(1.989*10**30,0,0,0,0 ) #Only need mass to test so rest on dummy variables
    Test_position=np.array([1,1,1])
    accel_sun_value=accel_g_sun(Test_position) #Value from accel_g_sun
    Planet_sun_accel=accel_g_planet(Test_position,Sun_planet)  #Value from accel_g_planet
    npt.assert_allclose(accel_sun_value,Planet_sun_accel,atol=1e-5)
    
    #Test 6: Using Accel_g_sun to get the Gravitational Constant 
    Position2=np.array([1,0,0])
    M_sun=1.989*10**30
    Test_Value2=accel_g_sun(Position2)[0]/M_sun #Test value over Mass of Sun to get Gravitational Constant
    npt.assert_allclose(6.674*10**-11*6.68459e-12**3,Test_Value2,atol=1e-4)
    
        
API()



    
    


def find_trajectory(vec_r0, vec_v0, planets, t_steps):
    """
    Main loop for solar system gravitation project.
    
    Arguments:
    =====
    * vec_r0: Initial 3-vector position of the small mass (Cartesian coordinates.)  
    * vec_v0: Initial 3-vector velocity of the small mass (Cartesian coordinates.)
    * planets: a list of planet tuples, at their initial positions.
        A planet tuple has the form:
            (M, a, eps, phi, omega)
        where M is the planet's mass, phi is the planet's angular position, 
        and a, eps, omega are orbital parameters.
    * t_steps: NumPy array (linspace or arange) specifying the range of times to simulate
        the trajectory over, regularly spaced by timestep dt.
        
    Returns:
    =====
    A tuple of the form (traj, planet_distance).
    
    "traj" contains the coordinates (x,y,z) of the test mass at each 
    corresponding time in t_steps, as a (3) x (Nt) array.
    "planet_distance" contains the distances from the small mass
    to each planet in planets, in order, as a function of time - this is a
    (len(planets)) x (Nt) array.
    
    Example usage:  (using kg-AU-s units)
    =====
    >> import unyt as u
    >> earth = (5.97219e24, 1.0, 0.01673163, 5.779905, 1.88570)
    >> r0 = np.array([-0.224, 0.98, 0.0])  # AU
    >> v0 = np.array([2e-9, 0.0, 0.0]) # AU/s 
    >> t = (np.arange(0, 4*365) * u.day).to_value('s')  # evolve for 4 years
    >> traj, pd = find_trajectory(r0, v0, [earth], t)
    
    """
    
    dt = t_steps[1] - t_steps[0]
    Nt = len(t_steps)
    
    traj = np.zeros((3, Nt))
    traj[:,0] = vec_r0
    
    planet_distance = np.zeros((len(planets), Nt))
    planet_distance[:,0] = get_planet_distances(vec_r0, planets)
    
    vec_v = vec_v0
    
    for i in range(Nt-1):
        (traj[:,i+1], vec_v) = update_position(traj[:,i], vec_v, planets, dt)        
        planets = update_all_planets(planets, dt)
        planet_distance[:,i+1] = get_planet_distances(traj[:,i+1], planets)
        
    return (traj, planet_distance)






                  

                  

                  
    