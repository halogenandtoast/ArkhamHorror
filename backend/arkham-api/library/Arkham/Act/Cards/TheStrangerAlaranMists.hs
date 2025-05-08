module Arkham.Act.Cards.TheStrangerAlaranMists (theStrangerAlaranMists, theStrangerAlaranMistsEffect) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.CurtainCall.Helpers

newtype TheStrangerAlaranMists = TheStrangerAlaranMists ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerAlaranMists :: ActCard TheStrangerAlaranMists
theStrangerAlaranMists = act (2, A) TheStrangerAlaranMists Cards.theStrangerAlaranMists Nothing

instance HasAbilities TheStrangerAlaranMists where
  getAbilities (TheStrangerAlaranMists a) =
    [ mkAbility a 1
        $ Objective
        $ forced
        $ EnemyWouldBeDiscarded #when
        $ enemyIs Enemies.theManInThePallidMask
    ]

instance RunMessage TheStrangerAlaranMists where
  runMessage msg a@(TheStrangerAlaranMists attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      moveTheManInThePalidMaskToLobbyInsteadOfDiscarding
      addChaosToken Cultist
      addChaosToken Tablet
      backstage <- selectJust $ LocationWithTitle "Backstage"
      placeTokens attrs backstage #horror 1
      card <- flipCard <$> genCard (toCardDef attrs)
      push $ PlaceNextTo ActDeckTarget [card]
      createCardEffect Cards.theStrangerAlaranMists Nothing attrs attrs
      advanceActDeck attrs
      pure a
    _ -> TheStrangerAlaranMists <$> liftRunMessage msg attrs

newtype TheStrangerAlaranMistsEffect = TheStrangerAlaranMistsEffect EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerAlaranMistsEffect :: EffectArgs -> TheStrangerAlaranMistsEffect
theStrangerAlaranMistsEffect = cardEffect TheStrangerAlaranMistsEffect Cards.theStrangerAlaranMists

instance HasAbilities TheStrangerAlaranMistsEffect where
  getAbilities (TheStrangerAlaranMistsEffect attrs) =
    [ playerLimit PerRound
        $ skillTestAbility
        $ mkAbility (proxied (LocationMatcherSource LocationWithAnyHorror) attrs) 1
        $ forced
        $ WouldMove #when You AnySource ThisLocation Anywhere
    ]

instance RunMessage TheStrangerAlaranMistsEffect where
  runMessage msg e@(TheStrangerAlaranMistsEffect attrs) = runQueueT $ case msg of
    UseThisAbility iid p@(isProxySource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (AbilitySource p 1) attrs #willpower (Fixed 3)
      pure e
    FailedThisSkillTest iid (isProxyAbilitySource attrs 1 -> True) -> do
      withI18n $ chooseOneM iid do
        labeled' "cancelMove" $ cancelMovement attrs.source iid
        scenarioI18n $ labeled' "theStrangerAlaranMists.endYourTurn" do
          afterMove attrs.source iid do
            setActions iid attrs.source 0
            endYourTurn iid

      pure e
    _ -> TheStrangerAlaranMistsEffect <$> liftRunMessage msg attrs
