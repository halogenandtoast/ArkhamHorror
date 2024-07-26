module Arkham.Asset.Cards.Microscope (microscope, Microscope (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyDefeated, EnemyEvaded)
import Arkham.Asset.Uses
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigate
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message (getChoiceAmount)
import Arkham.Modifier
import Arkham.Projection

newtype Microscope = Microscope AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

microscope :: AssetCard Microscope
microscope = asset Microscope Cards.microscope

instance HasAbilities Microscope where
  getAbilities (Microscope x) =
    [ restrictedAbility x 1 ControlsThis
        $ ReactionAbility
          ( oneOf
              [ EnemyDefeated #after Anyone ByAny $ EnemyAt YourLocation
              , EnemyEvaded #after Anyone $ EnemyAt YourLocation
              ]
          )
          (exhaust x)
    , restrictedAbility x 2 ControlsThis $ ActionAbility [#investigate] (ActionCost 2)
    ]

instance RunMessage Microscope where
  runMessage msg a@(Microscope attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs Evidence 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      when (attrs.use Evidence > 0) $ do
        skillTestModifier sid (attrs.ability 2) iid (SkillModifier #intellect $ min 3 $ attrs.use Evidence)
      pushM $ mkInvestigate sid iid (attrs.ability 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      withLocationOf iid \lid -> do
        clues <- field LocationClues lid
        when (clues > 1) do
          chooseAmounts
            iid
            "Evidence to spend"
            (MaxAmountTarget 2)
            [("Evidence", (0, min 2 $ min clues (attrs.use Evidence)))]
            attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "Evidence" -> n) (isTarget attrs -> True) -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 2) iid (DiscoveredClues n)
      pure a
    _ -> Microscope <$> liftRunMessage msg attrs
