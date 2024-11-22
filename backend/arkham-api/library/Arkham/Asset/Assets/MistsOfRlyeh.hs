module Arkham.Asset.Assets.MistsOfRlyeh (mistsOfRlyeh, MistsOfRlyeh (..), mistsOfRlyehEffect) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Effect.Import
import Arkham.Evade
import Arkham.Game.Helpers (getAccessibleLocations)
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.SkillTest (getSkillTest, getSkillTestInvestigator)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.SkillTest.Base
import Arkham.SkillTestResult

newtype MistsOfRlyeh = MistsOfRlyeh AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistsOfRlyeh :: AssetCard MistsOfRlyeh
mistsOfRlyeh = asset MistsOfRlyeh Cards.mistsOfRlyeh

instance HasAbilities MistsOfRlyeh where
  getAbilities (MistsOfRlyeh a) = [restricted a 1 ControlsThis $ evadeAction $ assetUseCost a Charge 1]

instance RunMessage MistsOfRlyeh where
  runMessage msg a@(MistsOfRlyeh attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      let tokens = oneOf [#skull, #cultist, #tablet, #elderthing, #autofail]
      onRevealChaosTokenEffect sid tokens attrs attrs do
        chooseAndDiscardCard iid (attrs.ability 1)
      createCardEffect Cards.mistsOfRlyeh Nothing source sid
      aspect iid source (#willpower `InsteadOf` #agility) (mkChooseEvade sid iid source)
      pure a
    _ -> MistsOfRlyeh <$> liftRunMessage msg attrs

newtype MistsOfRlyehEffect = MistsOfRlyehEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mistsOfRlyehEffect :: EffectArgs -> MistsOfRlyehEffect
mistsOfRlyehEffect = cardEffect MistsOfRlyehEffect Cards.mistsOfRlyeh

instance RunMessage MistsOfRlyehEffect where
  runMessage msg e@(MistsOfRlyehEffect attrs) = runQueueT $ case msg of
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> do
      whenJustM getSkillTestInvestigator \iid -> do
        mSkillTestResult <- fmap skillTestResult <$> getSkillTest
        case mSkillTestResult of
          Just (SucceededBy _ _) -> do
            unblockedConnectedLocationIds <- getAccessibleLocations iid attrs
            chooseOrRunOneM iid do
              labeled "Do not move to a connecting location" nothing
              targets unblockedConnectedLocationIds $ moveTo attrs iid
          _ -> pure ()
        disable attrs
      pure e
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> disableReturn e
    _ -> MistsOfRlyehEffect <$> liftRunMessage msg attrs
