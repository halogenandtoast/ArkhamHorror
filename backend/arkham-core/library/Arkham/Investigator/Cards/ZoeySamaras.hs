module Arkham.Investigator.Cards.ZoeySamaras where

import Arkham.Ability
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype ZoeySamaras = ZoeySamaras InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zoeySamaras :: InvestigatorCard ZoeySamaras
zoeySamaras =
  investigator ZoeySamaras Cards.zoeySamaras
    $ Stats {health = 9, sanity = 6, willpower = 4, intellect = 2, combat = 4, agility = 2}

instance HasAbilities ZoeySamaras where
  getAbilities (ZoeySamaras x) =
    [ restrictedAbility x 1 (Self <> CanGainResources)
        $ freeReaction
        $ EnemyEngaged #after You AnyEnemy
    ]

instance HasChaosTokenValue ZoeySamaras where
  getChaosTokenValue iid ElderSign (ZoeySamaras attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage ZoeySamaras where
  runMessage msg i@(ZoeySamaras attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ takeResources iid (toAbilitySource attrs 1) 1
      pure i
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      push $ skillTestModifier attrs attrs (DamageDealt 1)
      pure i
    _ -> ZoeySamaras <$> runMessage msg attrs
