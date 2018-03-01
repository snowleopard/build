module Development.Build.Compute.Identity (IdentityCompute, identityCompute) where

import Development.Build.Compute

-- | The identity computation that considers all keys as inputs. Note: there is
-- only one possible implementation, namely the 'id' function, since there is no
-- other way to produce a value of type @f v@.
identityCompute :: IdentityCompute k v
identityCompute = id
