module Arkham.Location.Cards.DowntownArkhamAsylum
  ( DowntownArkhamAsylum(..)
  , downtownArkhamAsylum
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Damage
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards ( downtownArkhamAsylum )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher

newtype DowntownArkhamAsylum = DowntownArkhamAsylum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downtownArkhamAsylum :: LocationCard DowntownArkhamAsylum
downtownArkhamAsylum =
  location DowntownArkhamAsylum Cards.downtownArkhamAsylum 4 (PerPlayer 2)

instance HasAbilities DowntownArkhamAsylum where
  getAbilities (DowntownArkhamAsylum x) | locationRevealed x = withBaseAbilities
    x
    [ limitedAbility (PlayerLimit PerGame 1)
      $ restrictedAbility
          x
          1
          (Here <> InvestigatorExists
            (HealableInvestigator (toSource x) HorrorType You)
          )
      $ ActionAbility Nothing
      $ ActionCost 1
    ]
  getAbilities (DowntownArkhamAsylum attrs) = getAbilities attrs

instance RunMessage DowntownArkhamAsylum where
  runMessage msg l@(DowntownArkhamAsylum attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      mHealHorror <- getHealHorrorMessage attrs 3 iid
      for_ mHealHorror push
      pure l
    _ -> DowntownArkhamAsylum <$> runMessage msg attrs
