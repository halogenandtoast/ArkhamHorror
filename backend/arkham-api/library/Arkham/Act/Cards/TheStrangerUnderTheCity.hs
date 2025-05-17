module Arkham.Act.Cards.TheStrangerUnderTheCity (theStrangerUnderTheCity, theStrangerUnderTheCityEffect) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.CurtainCall.Helpers
import Arkham.Trait (Trait (Ally, Item))

newtype TheStrangerUnderTheCity = TheStrangerUnderTheCity ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerUnderTheCity :: ActCard TheStrangerUnderTheCity
theStrangerUnderTheCity = act (2, A) TheStrangerUnderTheCity Cards.theStrangerUnderTheCity Nothing

instance HasAbilities TheStrangerUnderTheCity where
  getAbilities (TheStrangerUnderTheCity a) =
    [ mkAbility a 1
        $ Objective
        $ forced
        $ EnemyWouldBeDiscarded #when
        $ enemyIs Enemies.theManInThePallidMask
    ]

instance RunMessage TheStrangerUnderTheCity where
  runMessage msg a@(TheStrangerUnderTheCity attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      moveTheManInThePalidMaskToLobbyInsteadOfDiscarding
      addChaosToken Cultist
      addChaosToken ElderThing
      balcony <- selectJust $ LocationWithTitle "Balcony"
      placeTokens attrs balcony #horror 1
      card <- flipCard <$> genCard (toCardDef attrs)
      push $ PlaceNextTo ActDeckTarget [card]
      createCardEffect Cards.theStrangerUnderTheCity Nothing attrs attrs
      advanceActDeck attrs
      pure a
    _ -> TheStrangerUnderTheCity <$> liftRunMessage msg attrs

newtype TheStrangerUnderTheCityEffect = TheStrangerUnderTheCityEffect EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerUnderTheCityEffect :: EffectArgs -> TheStrangerUnderTheCityEffect
theStrangerUnderTheCityEffect = cardEffect TheStrangerUnderTheCityEffect Cards.theStrangerUnderTheCity

instance HasAbilities TheStrangerUnderTheCityEffect where
  getAbilities (TheStrangerUnderTheCityEffect attrs) =
    [ playerLimit PerRound
        $ skillTestAbility
        $ mkAbility (proxied (LocationMatcherSource LocationWithAnyHorror) attrs) 1
        $ forced
        $ Leaves #after You ThisLocation
    ]

instance RunMessage TheStrangerUnderTheCityEffect where
  runMessage msg e@(TheStrangerUnderTheCityEffect attrs) = runQueueT $ case msg of
    UseThisAbility iid p@(isProxySource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (AbilitySource p 1) attrs #willpower (Fixed 4)
      pure e
    FailedThisSkillTest iid (isProxyAbilitySource attrs 1 -> True) -> do
      assets <- select $ assetControlledBy iid <> hasAnyTrait [Item, Ally] <> DiscardableAsset
      if null assets
        then assignDamage iid attrs.source 2
        else chooseTargetM iid assets $ toDiscardBy iid attrs
      pure e
    _ -> TheStrangerUnderTheCityEffect <$> liftRunMessage msg attrs
