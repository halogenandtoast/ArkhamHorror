module Arkham.Asset.Assets.AceOfRods1 (aceOfRods1, aceOfRods1Effect) where

import Arkham.Ability
import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (DuringTurn)
import Arkham.SkillType

newtype AceOfRods1 = AceOfRods1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aceOfRods1 :: AssetCard AceOfRods1
aceOfRods1 = asset AceOfRods1 Cards.aceOfRods1

instance HasAbilities AceOfRods1 where
  getAbilities (AceOfRods1 a) =
    [ controlled a 1 (DuringTurn You) $ FastAbility Free
    , restricted a 2 InYourHand $ freeReaction (GameBegins #when)
    ]

instance RunMessage AceOfRods1 where
  runMessage msg a@(AceOfRods1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      removeFromGame attrs
      createCardEffect Cards.aceOfRods1 Nothing attrs iid
      pure a
    InHand iid (UseThisAbility iid' (isSource attrs -> True) 2) | iid == iid' -> do
      putCardIntoPlay iid attrs
      pure a
    _ -> AceOfRods1 <$> liftRunMessage msg attrs

newtype Meta = Meta {active :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype AceOfRods1Effect = AceOfRods1Effect (EffectAttrs `With` Meta)
  deriving anyclass (IsEffect, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aceOfRods1Effect :: EffectArgs -> AceOfRods1Effect
aceOfRods1Effect =
  cardEffect (AceOfRods1Effect . (`with` Meta False)) Cards.aceOfRods1

instance HasModifiersFor AceOfRods1Effect where
  getModifiersFor (AceOfRods1Effect (a `With` meta)) = do
    modifiedWhen_ a (active meta) a.target [SkillModifier sType 2 | sType <- allSkills]
    modifiedWhen_
      a
      (not $ active meta)
      a.target
      [ GiveAdditionalAction
          $ AdditionalAction "Ace of Rods" (toSource a)
          $ EffectAction "Use Ace of Rods (1) extra action with +2 to each skill"
          $ toId a
      ]

instance RunMessage AceOfRods1Effect where
  runMessage msg e@(AceOfRods1Effect (attrs `With` meta)) = runQueueT $ case msg of
    UseEffectAction iid eid _ | eid == toId attrs -> do
      gainActions iid attrs 1
      pure $ AceOfRods1Effect (attrs `with` Meta True)
    FinishAction -> do
      when (active meta) $ disable attrs
      pure e
    EndTurn iid | isTarget iid attrs.target -> do
      disableReturn e
    _ -> AceOfRods1Effect . (`with` meta) <$> liftRunMessage msg attrs
