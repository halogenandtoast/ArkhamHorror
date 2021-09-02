module Arkham.Types.Location.Cards.DowntownArkhamAsylum
  ( DowntownArkhamAsylum(..)
  , downtownArkhamAsylum
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (downtownArkhamAsylum)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target

newtype DowntownArkhamAsylum = DowntownArkhamAsylum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downtownArkhamAsylum :: LocationCard DowntownArkhamAsylum
downtownArkhamAsylum = location
  DowntownArkhamAsylum
  Cards.downtownArkhamAsylum
  4
  (PerPlayer 2)
  Triangle
  [Moon, T]

instance HasAbilities DowntownArkhamAsylum where
  getAbilities (DowntownArkhamAsylum x) | locationRevealed x =
    withBaseAbilities x $
      [ restrictedAbility
          x
          1
          (Here <> InvestigatorExists (You <> InvestigatorWithAnyHorror))
          (ActionAbility Nothing $ ActionCost 1)
        & abilityLimitL
        .~ PlayerLimit PerGame 1
      ]
  getAbilities (DowntownArkhamAsylum attrs) =
    getAbilities attrs

instance LocationRunner env => RunMessage env DowntownArkhamAsylum where
  runMessage msg l@(DowntownArkhamAsylum attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (HealHorror (InvestigatorTarget iid) 3)
    _ -> DowntownArkhamAsylum <$> runMessage msg attrs
