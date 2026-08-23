module Arkham.Asset.Assets.ChosenOfZburamoarteCompelledToFeed (
  chosenOfZburamoarteCompelledToFeed,
) where

import Arkham.Ability
import Arkham.Asset.Cards.ChildrenOfBlood qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosToken
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFace)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (withSkillTestSource)
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Message.Lifted.Choose

newtype ChosenOfZburamoarteCompelledToFeed = ChosenOfZburamoarteCompelledToFeed AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chosenOfZburamoarteCompelledToFeed :: AssetCard ChosenOfZburamoarteCompelledToFeed
chosenOfZburamoarteCompelledToFeed =
  asset ChosenOfZburamoarteCompelledToFeed Cards.chosenOfZburamoarteCompelledToFeed

instance HasModifiersFor ChosenOfZburamoarteCompelledToFeed where
  getModifiersFor (ChosenOfZburamoarteCompelledToFeed a) = do
    modifyEach a [toTarget BloodToken] [RevealAnotherChaosToken]
    for_ a.controller \iid ->
      modified_ a iid [SkillModifier #combat 1, SkillModifier #agility 1]

instance HasAbilities ChosenOfZburamoarteCompelledToFeed where
  getAbilities (ChosenOfZburamoarteCompelledToFeed a) =
    [ controlled a 1 (exists $ EnemyAt YourLocation)
        $ FastAbility
        $ exhaust a
        <> ReleaseChaosTokensCost 1 (SealedOnAsset (be a) (ChaosTokenFaceIs BloodToken))
    ]

instance RunMessage ChosenOfZburamoarteCompelledToFeed where
  runMessage msg a@(ChosenOfZburamoarteCompelledToFeed attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ enemyAtLocationWith iid
      chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1
      healDamage iid (attrs.ability 1) 1
      whenNone (SealedOnAsset (be attrs) (ChaosTokenFaceIs BloodToken))
        $ push
        $ Flip iid (toSource attrs) (toTarget attrs)
      pure a
    RevealChaosToken _ iid token -> do
      withSkillTestSource \_ -> do
        faces <- getModifiedChaosTokenFace token
        when (BloodToken `elem` faces) $ afterSkillTestQuiet $ sealChaosToken iid attrs token
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.chosenOfZburamoarteFightingTheHunger
      pure a
    _ -> ChosenOfZburamoarteCompelledToFeed <$> liftRunMessage msg attrs
