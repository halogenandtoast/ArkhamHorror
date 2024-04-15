module Arkham.Asset.Cards.TheHungeringBlade1 (theHungeringBlade1, TheHungeringBlade1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (Offering)
import Arkham.Fight
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Prelude
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheHungeringBlade1 = TheHungeringBlade1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHungeringBlade1 :: AssetCard TheHungeringBlade1
theHungeringBlade1 = asset TheHungeringBlade1 Cards.theHungeringBlade1

instance HasAbilities TheHungeringBlade1 where
  getAbilities (TheHungeringBlade1 attrs) = [restrictedAbility attrs 1 ControlsThis fightAction_]

instance RunMessage TheHungeringBlade1 where
  runMessage msg a@(TheHungeringBlade1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll
        [ skillTestModifiers source iid $ DamageDealt 1
            : [ ForEach
                  (CountTreacheries $ treacheryIs Treacheries.bloodlust <> TreacheryIsAttachedTo (toTarget attrs))
                  [SkillModifier #combat 1]
              ]
        , chooseFight
        ]
      pure a
    EnemyDefeated _ _ (isAbilitySource attrs 1 -> True) _ -> do
      push $ PlaceTokens (attrs.ability 1) (toTarget attrs) Offering 1
      pure a
    _ -> TheHungeringBlade1 <$> runMessage msg attrs
