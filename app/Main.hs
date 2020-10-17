import Data.Char
import Data.Validation


newtype Password = Password String deriving Show

newtype Error = Error [String] deriving Show

instance Semigroup Error where
    Error xs <> Error ys = Error (xs ++ ys)


cleanWhitespace :: String -> Validation Error String
cleanWhitespace "" = Failure (Error ["Cannot be empty."])
cleanWhitespace (x:xs) =
    case (isSpace x) of
        True -> cleanWhitespace xs
        False -> Success (x:xs)


checkPasswordLength :: String -> Validation Error Password
checkPasswordLength password =
    case (length password > 20) of
        True -> Failure (Error ["Your password cannot be longer \
            \than 20 characters."])
        False -> Success (Password password)


requireAlphaNum :: String -> Validation Error String
requireAlphaNum xs =
    case (all isAlphaNum xs) of
        False -> Failure (Error ["Cannot contain white space \
            \or special characters."])
        True -> Success xs


validatePassword :: Password -> Validation Error Password
validatePassword (Password password) =
    case (cleanWhitespace password) of
        Failure err -> Failure err
        Success password2 -> requireAlphaNum password2 *> checkPasswordLength password2
