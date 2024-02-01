module Arkham.Asset.Cards.DragonPole (
  dragonPole,
  DragonPole (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype DragonPole = DragonPole AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

dragonPole :: AssetCard DragonPole
dragonPole = asset DragonPole Cards.dragonPole

instance HasAbilities DragonPole where
  getAbilities (DragonPole a) = [restrictedAbility a 1 ControlsThis fightAction_]

instance RunMessage DragonPole where
  runMessage msg a@(DragonPole attrs) = case msg of
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      push $ AddSlot iid ArcaneSlot (Slot (toSource attrs) [])
      DragonPole <$> runMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      slots <-
        count (not . isEmptySlot) . findWithDefault [] #arcane <$> field InvestigatorSlots iid
      pushAll
        [ skillTestModifiers source iid (SkillModifier #combat slots : [DamageDealt 1 | slots >= 2])
        , chooseFightEnemy iid source #combat
        ]
      pure a
    _ -> DragonPole <$> runMessage msg attrs
