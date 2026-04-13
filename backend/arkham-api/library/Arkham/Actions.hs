module Arkham.Actions where

import Arkham.Action (Action)
import Arkham.Prelude
import Control.Monad.Fail (fail)
import GHC.OverloadedLabels

data Actions
  = SingleAction Action
  | AndActions [Actions]
  | OrActions [Actions]
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "resource" Actions where
  fromLabel = singleAction #resource

instance IsLabel "play" Actions where
  fromLabel = singleAction #play

instance IsLabel "draw" Actions where
  fromLabel = singleAction #draw

instance IsLabel "explore" Actions where
  fromLabel = singleAction #explore

instance IsLabel "circle" Actions where
  fromLabel = singleAction #circle

instance IsLabel "resign" Actions where
  fromLabel = singleAction #resign

instance IsLabel "evade" Actions where
  fromLabel = singleAction #evade

instance IsLabel "fight" Actions where
  fromLabel = singleAction #fight

instance IsLabel "investigate" Actions where
  fromLabel = singleAction #investigate

instance IsLabel "move" Actions where
  fromLabel = singleAction #move

instance IsLabel "engage" Actions where
  fromLabel = singleAction #engage

instance IsLabel "parley" Actions where
  fromLabel = singleAction #parley

type instance Element Actions = Action

singleAction :: Action -> Actions
singleAction = SingleAction

actionsToList :: Actions -> [Action]
actionsToList (SingleAction a) = [a]
actionsToList (AndActions as) = concatMap actionsToList as
actionsToList (OrActions as) = nub $ concatMap actionsToList as

isOrActions :: Actions -> Bool
isOrActions (OrActions _) = True
isOrActions _ = False

-- Smart constructors
andActions :: [Action] -> Actions
andActions [a] = SingleAction a
andActions as = AndActions (map SingleAction as)

orActions :: [Action] -> Actions
orActions [a] = SingleAction a
orActions as = OrActions (map SingleAction as)

-- Top-level branches of an OrActions (for presenting choices)
orBranches :: Actions -> [[Action]]
orBranches (OrActions as) = map actionsToList as
orBranches other = [actionsToList other]

instance ToJSON Actions where
  toJSON (SingleAction a) = object ["tag" .= ("SingleAction" :: Text), "contents" .= a]
  toJSON (AndActions as) = object ["tag" .= ("AndActions" :: Text), "contents" .= as]
  toJSON (OrActions as) = object ["tag" .= ("OrActions" :: Text), "contents" .= as]

instance FromJSON Actions where
  parseJSON v = case v of
    Array _ -> andActions <$> parseJSON v
    _ ->
      withObject
        "Actions"
        ( \o -> do
            tag <- o .: "tag"
            case (tag :: Text) of
              -- New names
              "SingleAction" -> SingleAction <$> o .: "contents"
              "AndActions" -> AndActions <$> o .: "contents"
              "OrActions" -> OrActions <$> o .: "contents"
              -- Legacy names (CardActions era)
              "CardAction" -> SingleAction <$> o .: "contents"
              "AndCardActions" -> AndActions <$> o .: "contents"
              "OrCardActions" -> OrActions <$> o .: "contents"
              _ -> fail $ "Unknown Actions tag: " <> show tag
        )
        v

instance Semigroup Actions where
  AndActions as <> AndActions bs = AndActions (as <> bs)
  AndActions as <> b = AndActions (as <> [b])
  a <> AndActions bs = AndActions (a : bs)
  a <> b = AndActions [a, b]

instance Monoid Actions where
  mempty = AndActions []
