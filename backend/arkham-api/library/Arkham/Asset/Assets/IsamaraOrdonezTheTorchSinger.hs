module Arkham.Asset.Assets.IsamaraOrdonezTheTorchSinger (
  isamaraOrdonezTheTorchSinger,
  isamaraOrdonezTheTorchSingerEffect,
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype IsamaraOrdonezTheTorchSinger = IsamaraOrdonezTheTorchSinger AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

isamaraOrdonezTheTorchSinger :: AssetCard IsamaraOrdonezTheTorchSinger
isamaraOrdonezTheTorchSinger = ally IsamaraOrdonezTheTorchSinger Cards.isamaraOrdonezTheTorchSinger (1, 3)

instance HasModifiersFor IsamaraOrdonezTheTorchSinger where
  getModifiersFor (IsamaraOrdonezTheTorchSinger a) = controllerGets a [SkillModifier #willpower 1]

instance HasAbilities IsamaraOrdonezTheTorchSinger where
  getAbilities (IsamaraOrdonezTheTorchSinger x) =
    [ controlled x 1 (exists $ NonEliteEnemy <> at_ (orConnected_ YourLocation))
        $ FastAbility (exhaust x <> HorrorCost (x.ability 1) (toTarget x) 1)
    ]

instance RunMessage IsamaraOrdonezTheTorchSinger where
  runMessage msg a@(IsamaraOrdonezTheTorchSinger attrs) = runQueueT $ case msg of
    ReadyExhausted -> do
      mods <- getModifiers attrs
      unless (CannotReady `elem` mods) do
        case attrs.controller of
          Just iid -> do
            modifiers <- getModifiers iid
            unless (ControlledAssetsCannotReady `elem` modifiers) do
              chooseOneM iid $ withI18n do
                questionLabeledCard attrs
                labeled "Ready" $ push $ Ready $ toTarget attrs
                skip_
          _ -> push (Ready $ toTarget attrs)
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ NonEliteEnemy <> at_ (orConnected_ $ locationWithInvestigator iid)
      chooseTargetM iid enemies \enemy -> do
        createCardEffect Cards.isamaraOrdonezTheTorchSinger Nothing attrs enemy
      pure a
    _ -> IsamaraOrdonezTheTorchSinger <$> liftRunMessage msg attrs

newtype IsamaraOrdonezTheTorchSingerEffect = IsamaraOrdonezTheTorchSingerEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

isamaraOrdonezTheTorchSingerEffect :: EffectArgs -> IsamaraOrdonezTheTorchSingerEffect
isamaraOrdonezTheTorchSingerEffect = cardEffect IsamaraOrdonezTheTorchSingerEffect Cards.isamaraOrdonezTheTorchSinger

instance HasModifiersFor IsamaraOrdonezTheTorchSingerEffect where
  getModifiersFor (IsamaraOrdonezTheTorchSingerEffect a) = modified_ a a.target [CannotMove]

instance RunMessage IsamaraOrdonezTheTorchSingerEffect where
  runMessage msg e@(IsamaraOrdonezTheTorchSingerEffect attrs) = runQueueT $ case msg of
    Ready (AssetTarget aid) -> do
      whenMatch aid (assetIs Cards.isamaraOrdonezTheTorchSinger) $ disable attrs
      pure e
    RemoveFromPlay (AssetSource aid) -> do
      whenMatch aid (assetIs Cards.isamaraOrdonezTheTorchSinger) $ disable attrs
      pure e
    _ -> IsamaraOrdonezTheTorchSingerEffect <$> liftRunMessage msg attrs
