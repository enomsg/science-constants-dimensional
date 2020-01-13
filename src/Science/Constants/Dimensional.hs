{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Constants.Dimensional
-- Copyright   :  (c) Anton Vorontsov <anton@enomsg.org>
-- License     :  BSD3
--
-- Maintainer  :  Anton Vorontsov <anton@enomsg.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- Dimensional Scientific Constants
--
-----------------------------------------------------------------------------

module Science.Constants.Dimensional where

import qualified Prelude
import qualified Science.Constants as C
import Science.Constants.Dimensional.Private
import Numeric.NumType.DK.Integers (neg1,neg2,neg3,pos2,pos3,pos4)
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.SIUnits
import Numeric.Units.Dimensional.Quantities

planckConstant  = C.planckConstant
    *~ (square meter * kilo gram / second)
    ~~ joule * second
plancksConstant = planckConstant

reducedPlanckConstant  = C.reducedPlanckConstant
    *~ (square meter * kilo gram / second)
    ~~ joule * second
reducedPlancksConstant = reducedPlanckConstant
planckConstantOver2Pi  = reducedPlanckConstant
plancksConstantOver2Pi = reducedPlanckConstant
hbar                   = reducedPlanckConstant
hslash                 = reducedPlanckConstant

speedOfLight = C.speedOfLight *~ (meter / second)

vacuumPermeability      = C.vacuumPermeability
    *~ (kilo gram * meter * second^neg2 * ampere^neg2)
    -- ~~ newton * ampere^neg2
permeabilityOfFreeSpace = vacuumPermeability
magneticConstant        = vacuumPermeability
mu_0                    = vacuumPermeability

vacuumPermittivity = C.vacuumPermittivity
    *~ (ampere^pos2 * second^pos4 * kilo gram^neg1 * meter^neg3)
    ~~ farad * meter^neg1
    ~~ coulomb^pos2 * newton^neg1 * meter^neg2
electricConstant   = vacuumPermittivity
eps_0              = vacuumPermittivity

coulombsConstant      = C.coulombsConstant
    *~ (kilo gram * meter^pos3 * coulomb^neg2 * second^neg2)
    ~~ meter / farad
    ~~ newton * meter^pos2 * coulomb^neg2
coulombConstant       = coulombsConstant
electricForceConstant = coulombsConstant
electrostaticConstant = coulombsConstant

rydbergConstant  = C.rydbergConstant *~ (meter^neg1)
rydbergsConstant = rydbergConstant

avogadroConstant = C.avogadroConstant *~ (mole^neg1)

boltzmannConstant  = C.boltzmannConstant
    *~ (meter^pos2 * kilo gram * second^neg2 * kelvin^neg1)
    ~~ joule / kelvin
boltzmannsConstant = boltzmannConstant

molarGasConstant     = C.molarGasConstant
    *~ (meter^pos2 * kilo gram * second^neg2 * kelvin^neg1 * mole^neg1)
    ~~ joule * mole^neg1 * kelvin^neg1
idealGasConstant     = molarGasConstant
universalGasConstant = molarGasConstant

molarMassConstant     = C.molarMassConstant *~ (kilo gram / mole)

gravitationalConstant = C.gravitationalConstant
    *~ (kilo gram^neg1 * meter^pos3 * second^neg2)
    ~~ newton * meter^pos2 * kilo gram^neg2
newtonConstant        = gravitationalConstant
newtonsConstant       = gravitationalConstant
bigG                  = gravitationalConstant

standardAccelerationOfGravity = C.standardAccelerationOfGravity
    *~ (meter * second^neg2)
earthAccelerationOfGravity    = standardAccelerationOfGravity

atomicMassConstant = C.atomicMassConstant *~ (kilo gram)
atomicMassUnit     = atomicMassConstant
dalton             = atomicMassConstant

electronMolarMass   = C.electronMolarMass
    *~ (kilo gram / mole)
molarMassOfElectron = electronMolarMass

massOfElectron = C.massOfElectron *~ kilo gram

massOfProton = C.massOfProton *~ kilo gram

elementaryCharge = C.elementaryCharge *~ coulomb
chargeOfProton   = elementaryCharge

chargeOfElectron = (-C.elementaryCharge) *~ coulomb
