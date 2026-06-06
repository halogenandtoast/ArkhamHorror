module Arkham.Asset.Assets.TheMilitarysPlan (theMilitarysPlan) where

import Arkham.Ability
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Oozified))

newtype TheMilitarysPlan = TheMilitarysPlan AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMilitarysPlan :: AssetCard TheMilitarysPlan
theMilitarysPlan = asset TheMilitarysPlan Cards.theMilitarysPlan

instance HasModifiersFor TheMilitarysPlan where
  getModifiersFor (TheMilitarysPlan a) =
    modifySelect a (LocationWithAnyHorror <> LocationWithTrait Oozified) [ScenarioModifier "explosives"]

instance HasAbilities TheMilitarysPlan where
  getAbilities (TheMilitarysPlan a) =
    [ scenarioI18n
        $ withI18nTooltip "theMilitarysPlan.action"
        $ restricted (proxied LocationWithAnyHorror a) 1 Here actionAbility
    , restricted a 1 criteria $ forced $ RoundEnds #when
    ]
   where
    criteria = if toResultDefault False a.meta then NoRestriction else Never

instance RunMessage TheMilitarysPlan where
  runMessage msg a@(TheMilitarysPlan attrs) = runQueueT $ case msg of
    BeginRound -> do
      pure $ TheMilitarysPlan $ attrs & setMeta True
    UseThisAbility iid source@(isProxySource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (AbilitySource source 1) attrs [#intellect, #agility] (Fixed 3)
      pure a
    PassedThisSkillTest iid (AbilitySource (isProxySource attrs -> True) 1) -> do
      withLocationOf iid \lid -> removeTokens (attrs.ability 1) lid #horror 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      removeTokens (attrs.ability 2) attrs #damage 1
      pure a
    _ -> TheMilitarysPlan <$> liftRunMessage msg attrs
