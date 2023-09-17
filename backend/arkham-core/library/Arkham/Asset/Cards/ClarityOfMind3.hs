module Arkham.Asset.Cards.ClarityOfMind3 (
  clarityOfMind3,
  ClarityOfMind3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher

newtype ClarityOfMind3 = ClarityOfMind3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clarityOfMind3 :: AssetCard ClarityOfMind3
clarityOfMind3 = asset ClarityOfMind3 Cards.clarityOfMind3

instance HasAbilities ClarityOfMind3 where
  getAbilities (ClarityOfMind3 a) =
    [ restrictedAbility
        a
        1
        ( ControlsThis
            <> InvestigatorExists
              (InvestigatorAt YourLocation <> InvestigatorWithAnyHorror)
        )
        $ actionAbilityWithCost (assetUseCost a Charge 1)
    ]

instance RunMessage ClarityOfMind3 where
  runMessage msg a@(ClarityOfMind3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      iidsWithHeal <- getInvestigatorsWithHealHorror attrs 1 $ colocatedWith iid
      pushAll $ replicate 2 $ chooseOrRunOne iid $ map (uncurry targetLabel . second only) iidsWithHeal
      pure a
    _ -> ClarityOfMind3 <$> runMessage msg attrs
