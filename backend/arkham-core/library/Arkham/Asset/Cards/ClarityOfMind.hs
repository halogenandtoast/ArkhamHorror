module Arkham.Asset.Cards.ClarityOfMind (
  clarityOfMind,
  ClarityOfMind (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher

newtype ClarityOfMind = ClarityOfMind AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

clarityOfMind :: AssetCard ClarityOfMind
clarityOfMind = asset ClarityOfMind Cards.clarityOfMind

instance HasAbilities ClarityOfMind where
  getAbilities (ClarityOfMind a) =
    [ controlledAbility
        a
        1
        (exists $ HealableInvestigator (toAbilitySource a 1) #horror $ InvestigatorAt YourLocation)
        $ actionAbilityWithCost (assetUseCost a Charge 1)
    ]

instance RunMessage ClarityOfMind where
  runMessage msg a@(ClarityOfMind attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      iidsWithHeal <- getInvestigatorsWithHealHorror attrs 1 (colocatedWith iid)
      player <- getPlayer iid
      push $ chooseOrRunOne player $ map (uncurry targetLabel . second only) iidsWithHeal
      pure a
    _ -> ClarityOfMind <$> runMessage msg attrs
