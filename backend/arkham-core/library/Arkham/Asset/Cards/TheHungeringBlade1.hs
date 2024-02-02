module Arkham.Asset.Cards.TheHungeringBlade1 (
  theHungeringBlade1,
  TheHungeringBlade1 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheHungeringBlade1 = TheHungeringBlade1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theHungeringBlade1 :: AssetCard TheHungeringBlade1
theHungeringBlade1 = asset TheHungeringBlade1 Cards.theHungeringBlade1

instance HasAbilities TheHungeringBlade1 where
  getAbilities (TheHungeringBlade1 attrs) = [restrictedAbility attrs 1 ControlsThis fightAction_]

instance RunMessage TheHungeringBlade1 where
  runMessage msg a@(TheHungeringBlade1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      bloodlusts <-
        selectCount $ treacheryIs Treacheries.bloodlust <> TreacheryIsAttachedTo (toTarget attrs)
      pushAll
        [ skillTestModifiers
            (toAbilitySource attrs 1)
            iid
            (DamageDealt 1 : [SkillModifier #combat bloodlusts | bloodlusts > 0])
        , chooseFightEnemy iid (toAbilitySource attrs 1) #combat
        ]
      pure a
    EnemyDefeated _ _ (isAbilitySource attrs 1 -> True) _ -> do
      push $ PlaceTokens (toAbilitySource attrs 1) (toTarget attrs) Offering 1
      pure a
    _ -> TheHungeringBlade1 <$> runMessage msg attrs
