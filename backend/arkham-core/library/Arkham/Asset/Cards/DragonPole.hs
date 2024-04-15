module Arkham.Asset.Cards.DragonPole (dragonPole, DragonPole (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Fight
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection

newtype DragonPole = DragonPole AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dragonPole :: AssetCard DragonPole
dragonPole = asset DragonPole Cards.dragonPole

instance HasAbilities DragonPole where
  getAbilities (DragonPole a) = [restrictedAbility a 1 ControlsThis fightAction_]

instance HasModifiersFor DragonPole where
  getModifiersFor (InvestigatorTarget iid) (DragonPole attrs) | attrs `controlledBy` iid = do
    mSource <- getSkillTestSource
    case mSource of
      Just (isAbilitySource attrs 1 -> True) -> do
        slots <- count (not . isEmptySlot) . findWithDefault [] #arcane <$> field InvestigatorSlots iid
        pure $ toModifiers attrs (SkillModifier #combat slots : [DamageDealt 1 | slots >= 2])
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage DragonPole where
  runMessage msg a@(DragonPole attrs) = case msg of
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      push $ AddSlot iid ArcaneSlot (Slot (toSource attrs) [])
      DragonPole <$> runMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushM $ mkChooseFight iid (attrs.ability 1)
      pure a
    _ -> DragonPole <$> runMessage msg attrs
