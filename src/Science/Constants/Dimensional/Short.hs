{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Constants.Dimensional.Short
-- Copyright   :  (c) Anton Vorontsov <anton@enomsg.org>
-- License     :  BSD3
--
-- Maintainer  :  Anton Vorontsov <anton@enomsg.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- Short Names for Scientific Constants
--
-----------------------------------------------------------------------------

module Science.Constants.Dimensional.Short where

import qualified Prelude
import qualified Science.Constants.Short as C
import Science.Constants.Dimensional
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.SIUnits

-- | Planck Constant
h   = planckConstant

-- | Reduced Planck Constant
_h  = reducedPlanckConstant

-- | Vacuum Permittivity
e_0 = vacuumPermittivity

-- | Coulomb's Constant
k_e = coulombsConstant

-- | Boltzmann Constant
k_b = boltzmannConstant

-- | Standard Acceleration of Gravity (9.8...)
g   = standardAccelerationOfGravity

-- | Atomic Mass Constant
amu = atomicMassConstant

-- | Mass of an Electron at Rest
m_e = massOfElectron

-- | Mass of a Proton
m_p = massOfProton

-- | Charge of a Proton
q_p = chargeOfProton

-- | Charge of an Electron
q_e = chargeOfElectron

-- | Tau (2*pi)
tau = C.tau *~ radian
