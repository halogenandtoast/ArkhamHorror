module Arkham.Asset.Cards.TheCustodian (
  theCustodian,
  TheCustodian (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Projection

newtype TheCustodian = TheCustodian AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCustodian :: AssetCard TheCustodian
theCustodian = asset TheCustodian Cards.theCustodian

instance HasAbilities TheCustodian where
  getAbilities (TheCustodian a) =
    [ restrictedAbility a 1 ControlsThis $ freeReaction (PhaseBegins #when #investigation)
    , withCriteria (mkAbility a 2 #parley)
        $ Uncontrolled
        <> OnSameLocation
        <> InvestigatorExists (You <> InvestigatorWithAnyClues)
    ]

instance RunMessage TheCustodian where
  runMessage msg a@(TheCustodian attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      iids <- select $ InvestigatorAt $ locationWithAsset attrs
      for_ iids $ \iid -> pushM $ drawCards iid (toAbilitySource attrs 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ parley iid (toAbilitySource attrs 2) attrs #intellect (Fixed 3)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      clueCount <- field AssetClues (toId a)
      takeControl <- (clueCount + 1 >=) <$> perPlayer 1
      pushAll
        $ InvestigatorSpendClues iid 1
        : PlaceClues (toAbilitySource attrs 1) (toTarget attrs) 1
        : [TakeControlOfAsset iid (toId a) | takeControl]
      pure a
    _ -> TheCustodian <$> runMessage msg attrs
