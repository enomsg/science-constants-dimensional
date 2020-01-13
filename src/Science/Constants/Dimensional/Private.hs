-----------------------------------------------------------------------------
-- |
-- Module      :  Science.Constants.Dimensional.Private
-- Copyright   :  (c) Anton Vorontsov <anton@enomsg.org>
-- License     :  BSD3
--
-- Maintainer  :  Anton Vorontsov <anton@enomsg.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- Private Functions
--
-----------------------------------------------------------------------------

module Science.Constants.Dimensional.Private where

import Numeric.Units.Dimensional

-- | This is a compile-time check function, it ensures that the first
-- argument has the Unit-type of the second argument.
infixl 0 ~~
(~~) :: Quantity d a -> Unit m d a -> Quantity d a
(~~) = Prelude.const
