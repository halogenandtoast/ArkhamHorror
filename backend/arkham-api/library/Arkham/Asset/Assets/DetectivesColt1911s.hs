module Arkham.Asset.Assets.DetectivesColt1911s (detectivesColt1911s) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Deck qualified as Deck
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach)
import Arkham.Investigator.Deck
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype DetectivesColt1911s = DetectivesColt1911s AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

detectivesColt1911s :: AssetCard DetectivesColt1911s
detectivesColt1911s = asset DetectivesColt1911s Cards.detectivesColt1911s

instance HasModifiersFor DetectivesColt1911s where
  getModifiersFor (DetectivesColt1911s a) = for_ a.controller \iid -> do
    -- without the line below this suffers the zebra effect
    withoutModifiersOf a do
      toolAssetsWithHands <- take 2 <$> select (assetControlledBy iid <> #tool <> AssetWithSlot #hand)
      modifyEach a toolAssetsWithHands [DoNotTakeUpSlot #hand]

instance HasAbilities DetectivesColt1911s where
  getAbilities (DetectivesColt1911s a) = [controlled_ a 1 $ fightAction $ assetUseCost a Ammo 1]

instance RunMessage DetectivesColt1911s where
  runMessage msg a@(DetectivesColt1911s attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [DamageDealt 1, SkillModifier #combat 1]
      chooseFightEnemy sid iid source
      pure a
    EnemyDefeated _ _ (isAbilitySource attrs 1 -> True) _ -> do
      for_ attrs.controller \iid -> do
        insights <- filterCards (card_ $ #insight <> #event) <$> field InvestigatorDiscard iid
        unless (null insights) do
          chooseOneM iid do
            labeled "Do not move an insight" nothing
            targets insights $ putCardOnBottomOfDeck iid (Deck.InvestigatorDeckByKey iid HunchDeck)
      pure a
    _ -> DetectivesColt1911s <$> liftRunMessage msg attrs
