# -*- coding: utf-8 -*-
"""
Bryson Cook
ISYE6501, Spring 2018
HW9, 13.2

http://simpy.readthedocs.io/en/latest/examples/carwash.html


In this problem you, can simulate a simplified airport security system at a busy airport. Passengers arrive
according to a Poisson distribution with λ1 = 5 per minute (i.e., mean interarrival rate μ1 = 0.2 minutes)
to the ID/boarding-pass check queue, where there are several servers who each have exponential
service time with mean rate μ2 = 0.75 minutes. [Hint: model them as one block that has more than one
resource.] After that, the passengers are assigned to the shortest of the several personal-check queues,
where they go through the personal scanner (time is uniformly distributed between 0.5 minutes and 1
minute).
Use the Arena software (PC users) or Python with SimPy (PC or Mac users) to build a simulation of the
system, and then vary the number of ID/boarding-pass checkers and personal-check queues to
determine how many are needed to keep average wait times below 15 minutes. [If you’re using SimPy,
or if you have access to a non-student version of Arena, you can use λ1 = 50 to simulate a busier airport.]

"""
import simpy as sy
import numpy as np

random.seed(1)
lambda1 = 5 #arrivals per minute
arrival = ((lambda1^i)*np.exp(i)/np.factorial(i))
lambda2 = .75^-1 #mean service rate of ID checks

num_servers = 10

num_scanners = 10



class Airport(object):
    """ID/boarding pass check queue and security scanning
    """
    def __init__(self, env, num_servers, lambda2):
        self.env = env
        self.server = sy.Resource(env, num_servers)
        self.service_time = lambda2*np.exp(-lambda2*i)
        self.scanner = sy.Resource(env, num_scanners)
        self.scan_time = np.random.uniform(low = 0.5, high = 1)

    def IDcheck(self, passenger):
        """The ID check process. It takes a "person", checks their ID,then 
        passes him/her to the next step."""
        passenger  = i
        lambda2 = .75^-1 #mean service rate of ID checks
        ID_service_time = lambda2*np.exp(-lambda2*i)
        yield self.env.timeout(ID_service_time)

    def scan(self, passenger):
        """Security Scan. Passenergs are sent to shortest queue then scanned"""
        scan_time = np.random.uniform(low = 0.5, high = 1)
        yield self.env.timeout(scan_time)
        
        
def passenger(env, number, action):
    """Passnegers arrive, to the first available server for the ID check then
    sent to the first available scanner.
    """
    with action.server.request() as request:
        yield request
        yield env.process(action.IDcheck(number))
    
    #arrange passenger to go to shortest scanner queue here
    #arrange passenger to go to shortest scanner queue here
    #arrange passenger to go to shortest scanner queue here
    #arrange passenger to go to shortest scanner queue here
    #arrange passenger to go to shortest scanner queue here
    
    with action.scanner.request() as request:
        yield request
        yield env.process(action.scan(number))
    
    