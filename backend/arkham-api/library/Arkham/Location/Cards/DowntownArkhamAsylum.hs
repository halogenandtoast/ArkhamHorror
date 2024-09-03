module Arkham.Location.Cards.DowntownArkhamAsylum (
  DowntownArkhamAsylum (..),
  downtownArkhamAsylum,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Damage
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards (downtownArkhamAsylum)
import Arkham.Location.Runner
import Arkham.Matcher

newtype DowntownArkhamAsylum = DowntownArkhamAsylum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downtownArkhamAsylum :: LocationCard DowntownArkhamAsylum
downtownArkhamAsylum = location DowntownArkhamAsylum Cards.downtownArkhamAsylum 4 (PerPlayer 2)

instance HasAbilities DowntownArkhamAsylum where
  getAbilities (DowntownArkhamAsylum x) =
    withRevealedAbilities x
      $ [ limitedAbility (PlayerLimit PerGame 1)
            $ withCriteria (mkAbility x 1 $ ActionAbility [] $ ActionCost 1)
            $ Here
            <> InvestigatorExists (HealableInvestigator (toSource x) HorrorType You)
        ]

instance RunMessage DowntownArkhamAsylum where
  runMessage msg l@(DowntownArkhamAsylum attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      canHeal <- canHaveHorrorHealed (attrs.ability 1) iid
      pushWhen canHeal $ HealHorror (toTarget iid) (attrs.ability 1) 3
      pure l
    _ -> DowntownArkhamAsylum <$> runMessage msg attrs
