module Ch11_Programmers where

data OperatingSystem =
  GnuPlusLinux | OpenBSD | Mac | Windows
  deriving (Eq, Show)

data ProgLang =
  Haskell | Agda | Idris | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os   :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux , OpenBSD , Mac , Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers =
  [Programmer os lang
  | os   <- allOperatingSystems
  , lang <- allLanguages
  ]
