{-   A Quantum Point System
     Describes a quantum system as described in "Quantum Computing for Computer Scientists" by Yanofsky and Mannucci.
     An arbitrary integer number is given to denote the state (or point) of a quantum system.
     
     -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --  

     The StateMatrix is the current state of the system represented by complex numbers.
     It is also the superposition of the basic states of the system
     and represents the points where it can be "detected" i.e measured.
     A point in this case can represent a particle (e.g a subatomic particle)
-}

module QuantumPointSystem (
        obsP,
        transP,
        findPointMod,
        normalize,
        bra,
        sumModulus,
        nthSumMod
    ) where

import Data.Complex
import Data.List

----------------------------------------------------------------------------------------------------------
-- Type definitions

type Point         = Int 
type State a       = (Point, Complex a)
type StateMatrix a = [State a]

----------------------------------------------------------------------------------------------------------
-- Functions of Quantum Point System

-- transP (Transition Amplitude)
-- Computes transition probability from one state to another given two kets (ket corresponds to state in quantum mechanics)
-- Determines how likely the state of the system before a specific measurement will change to another
transP :: (RealFloat a) => StateMatrix a -> StateMatrix a -> Complex a
transP startS endS = innerProd startS (bra endS) 

-- obsP
-- Probability that after observing the particle we will detect it at a given point 'p' 
obsP :: (RealFloat a) => Point -> StateMatrix a -> Maybe a 
obsP p sm = fmap (/((sumModulus sm)^2)) (findPointMod p sm) 

-- findPointMod
-- Given a point and a StateMatrix, search for the point and compute the modulus of the point
findPointMod :: (RealFloat a) => Point -> StateMatrix a -> Maybe a
findPointMod p sm = getStateTuple p sm >>= getComplexState >>= safeModulus

-- normalize
-- Normalizes the StateMatrix 
normalize :: (RealFloat a) => StateMatrix a -> StateMatrix a 
normalize xs = let f = sumModulus xs in fmap (\(x, y) -> (x, y / (f :+ 0))) xs 

-- bra
-- Complex conjugate of the end state you wish to compute
bra :: (RealFloat a) => StateMatrix a -> StateMatrix a
bra xs = complexConjugate xs

-- sumModulus
-- Given a StateMatrix, computes the complex sum of all of the points represented by the system and takes the sqrt
sumModulus :: (RealFloat a) => StateMatrix a -> a
sumModulus xs = sqrt $ foldr (\(_,y) x -> modulus y + x) 0 xs

-- nthSumMod
--Takes the nth exponent of the sumModulus 
nthSumMod :: (RealFloat a) => Integer -> StateMatrix a -> a
nthSumMod n xs = (sumModulus xs)^n

----------------------------------------------------------------------------------------------------------
-- Helper functions

-- Inner Product of Two Matrices
innerProd :: (RealFloat a) => StateMatrix a -> StateMatrix a -> Complex a
innerProd xs ys = sum $ zipWith f xs ys 
                    where f a b = (snd a) * (snd b)

-- Complex Conjugate of a StateMatrix a
complexConjugate :: (RealFloat a) => StateMatrix a -> StateMatrix a
complexConjugate xs = fmap (\(x,y) -> (x, conjugate y) ) xs

-- Inner Product of Two Matrices
safeInnerProd :: (RealFloat a) => StateMatrix a -> StateMatrix a -> Maybe (Complex a)
safeInnerProd xs ys =
    case checkStateLenghts xs ys of
        True -> Just $ innerProd xs ys
        _    -> Nothing

-- Searches a given StateMatrix for a point in the matrix
getStateTuple :: (RealFloat a) => Point -> StateMatrix a -> Maybe (State a)
getStateTuple p [] = Nothing
getStateTuple p (x:xs)
        | p == fst x = Just x
        | otherwise  = getStateTuple p xs

-- Deconstructs Just type to return the second component
getComplexState :: (RealFloat a) => State a -> Maybe (Complex a)
getComplexState a = Just $ snd a

-- Calculates the modulus of a complex number |a|^2
modulus :: (RealFloat a) => Complex a -> a
modulus a = realPart $ a * conjugate a

safeModulus :: (RealFloat a) => Complex a -> Maybe a
safeModulus a = Just $ realPart $ a * conjugate a

-- Checks if the two states are the same length
checkStateLenghts :: StateMatrix a -> StateMatrix a -> Bool
checkStateLenghts xs ys
    | length xs == length ys = True
    | otherwise              = False


----------------------------------------------------------------------------------------------------------
-- Test States
a :: (RealFloat a) => StateMatrix a
a = [(1, 1 :+1), (2, 1 :+ 1), (3, 1:+ 1)]

b :: (RealFloat a) => StateMatrix a
b = [(0, (-3) :+ (-1)), (1, 0 :+ (-2)), (2, 0 :+ 1), (3, 2 :+ 0)]

b' :: (RealFloat a) => StateMatrix a
b' = [(0, (-3) :+ (-1)), (1, 0 :+ (-2)), (2, 0 :+ 1), (3, 2 :+ 0)]

c :: (RealFloat a) => StateMatrix a
c = [ (0 , (2 :+ (-3))), (1, (1 :+ 2))]

d :: (RealFloat a) => StateMatrix a
d = [(0, (sqrt(2) / 2) * (1 :+ 0)), (1, (sqrt(2) / 2) * ((0 :+ 1)))]

d' :: (RealFloat a) => StateMatrix a
d' = [(0, (sqrt(2) / 2) * (0:+ 1)), (1, (sqrt(2) / 2) * (((-1) :+ 0)))]
