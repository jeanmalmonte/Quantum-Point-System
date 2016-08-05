# Quantum-Point-System
Quantum System described in Yanofsky's and Mannucci's "Quantum Computing for Computer Scientists" book.


     An arbitrary integer number is given to denote the state (or point) of a quantum system.
     
     -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --  
     
     The StateMatrix is the current state of the system represented by complex numbers.
     It is also the superposition of the basic states of the system
     and represents the points where it can be "detected" i.e measured.
     A point in this case can represent a particle (e.g a subatomic particle).
     
     
Example of a quantum point system described in this exercise:
```Haskell
pointSystem1 :: (RealFloat a) => StateMatrix a
pointSystem1 = [(0, (-3) :+ (-1)), (1, 0 :+ (-2)), (2, 0 :+ 1), (3, 2 :+ 0)]
```