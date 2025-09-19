module Arkham.Asset.Assets.TheCustodian (theCustodian) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype TheCustodian = TheCustodian AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCustodian :: AssetCard TheCustodian
theCustodian = asset TheCustodian Cards.theCustodian

instance HasAbilities TheCustodian where
  getAbilities (TheCustodian a) =
    [ controlled_ a 1 $ freeReaction (PhaseBegins #when #investigation)
    , skillTestAbility $ withCriteria (mkAbility a 2 #parley) $ Uncontrolled <> OnSameLocation
    ]

instance RunMessage TheCustodian where
  runMessage msg a@(TheCustodian attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (InvestigatorAt $ locationWithAsset attrs) \iid -> drawCards iid (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 2) attrs #intellect (Fixed 3)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      hasClues <- fieldMap InvestigatorClues (> 0) iid
      when hasClues do
        clueCount <- field AssetClues (toId a)
        moveTokens (attrs.ability 1) iid attrs #clue 1
        takeControl <- (clueCount + 1 >=) <$> perPlayer 1
        when takeControl $ takeControlOfAsset iid attrs
      pure a
    _ -> TheCustodian <$> liftRunMessage msg attrs
