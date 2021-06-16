module Helpers (fails) where

fails :: Either a b -> Bool
fails (Right a) = False
fails _ = True