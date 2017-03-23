module Exercise where
import Prelude hiding (and,or,not,(||),(&&))

and :: Bool -> Bool -> Bool
and False _ = False
and _ False = False
and True True = True

or :: Bool -> Bool -> Bool
or True _ = True
or _ True = True
or False False = False

not :: Bool -> Bool
not True = False
not False = True

imply :: Bool -> Bool -> Bool
imply True False = False
imply _ _ = True

