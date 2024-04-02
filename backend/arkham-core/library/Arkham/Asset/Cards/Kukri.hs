module Arkham.Asset.Cards.Kukri (kukri, Kukri (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection

newtype Kukri = Kukri AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kukri :: AssetCard Kukri
kukri = asset Kukri Cards.kukri

instance HasAbilities Kukri where
  getAbilities (Kukri a) = [restrictedAbility a 1 ControlsThis fightAction_]

instance RunMessage Kukri where
  runMessage msg a@(Kukri attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll [skillTestModifier source iid (SkillModifier #combat 1), chooseFight]
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      actionRemainingCount <- field InvestigatorRemainingActions iid
      player <- getPlayer iid
      pushWhen (actionRemainingCount > 0)
        $ chooseOne
          player
          [ Label
              "Spend 1 action to deal +1 damage"
              [LoseActions iid (attrs.ability 1) 1, skillTestModifier attrs iid (DamageDealt 1)]
          , Label "Skip additional Kukri damage" []
          ]
      pure a
    _ -> Kukri <$> runMessage msg attrs
