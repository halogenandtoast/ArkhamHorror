module Arkham.Location.Cards.RuinsOfNewYork
  ( ruinsOfNewYork
  , RuinsOfNewYork(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Helpers.Ability
import Arkham.Helpers.Query
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype RuinsOfNewYork = RuinsOfNewYork LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfNewYork :: LocationCard RuinsOfNewYork
ruinsOfNewYork = location RuinsOfNewYork Cards.ruinsOfNewYork 1 (Static 3)

instance HasAbilities RuinsOfNewYork where
  getAbilities (RuinsOfNewYork a) = withBaseAbilities
    a
    [ mkAbility a 1
      $ ForcedAbility
      $ PutLocationIntoPlay Timing.After Anyone
      $ LocationWithId
      $ toId a
    ]

instance RunMessage RuinsOfNewYork where
  runMessage msg l@(RuinsOfNewYork attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      lead <- getLeadInvestigatorId
      playerCount <- getPlayerCount
      deck <- fieldMap InvestigatorDeck unDeck lead
      let
        (cards, _) = splitAt (if playerCount >= 3 then 2 else 1) deck
        polyps = map (\card -> PlayerCard $ card { pcCardCode = "xpolyp" }) cards
      pushAll
        $ map (RemovePlayerCardFromGame False . PlayerCard) cards
        <> map (`CreateEnemyWithPlacement` AtLocation (toId attrs)) polyps
      pure l
    _ -> RuinsOfNewYork <$> runMessage msg attrs
