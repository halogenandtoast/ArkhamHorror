module Arkham.Location.Cards.DowntownArkhamAsylum
  ( DowntownArkhamAsylum(..)
  , downtownArkhamAsylum
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( downtownArkhamAsylum )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype DowntownArkhamAsylum = DowntownArkhamAsylum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downtownArkhamAsylum :: LocationCard DowntownArkhamAsylum
downtownArkhamAsylum =
  location DowntownArkhamAsylum Cards.downtownArkhamAsylum 4 (PerPlayer 2)

instance HasAbilities DowntownArkhamAsylum where
  getAbilities (DowntownArkhamAsylum x) | locationRevealed x =
    withBaseAbilities x
      $ [ limitedAbility (PlayerLimit PerGame 1)
          $ restrictedAbility
              x
              1
              (Here <> InvestigatorExists (You <> InvestigatorWithAnyHorror))
          $ ActionAbility Nothing
          $ ActionCost 1
        ]
  getAbilities (DowntownArkhamAsylum attrs) = getAbilities attrs

instance RunMessage DowntownArkhamAsylum where
  runMessage msg l@(DowntownArkhamAsylum attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      l <$ push (HealHorror (InvestigatorTarget iid) 3)
    _ -> DowntownArkhamAsylum <$> runMessage msg attrs
