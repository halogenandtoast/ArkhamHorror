module Arkham.Investigator.Cards.ZoeySamaras where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype ZoeySamaras = ZoeySamaras InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zoeySamaras :: InvestigatorCard ZoeySamaras
zoeySamaras =
  investigator
    ZoeySamaras
    Cards.zoeySamaras
    Stats
      { health = 9
      , sanity = 6
      , willpower = 4
      , intellect = 2
      , combat = 4
      , agility = 2
      }

instance HasAbilities ZoeySamaras where
  getAbilities (ZoeySamaras x) =
    [ restrictedAbility
        x
        1
        (Self <> Negate (SelfHasModifier CannotGainResources))
        $ ReactionAbility (EnemyEngaged Timing.After You AnyEnemy) Free
    ]

instance HasChaosTokenValue ZoeySamaras where
  getChaosTokenValue iid ElderSign (ZoeySamaras attrs)
    | iid == toId attrs =
        pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage ZoeySamaras where
  runMessage msg i@(ZoeySamaras attrs) = case msg of
    UseCardAbility _ (InvestigatorSource iid) 1 _ _
      | iid == toId attrs ->
          i <$ push (TakeResources (toId attrs) 1 (toAbilitySource attrs 1) False)
    ResolveChaosToken _drawnToken ElderSign iid
      | iid == toId attrs ->
          i
            <$ push
              ( CreateWindowModifierEffect
                  EffectSkillTestWindow
                  (EffectModifiers $ toModifiers attrs [DamageDealt 1])
                  (toSource attrs)
                  (toTarget attrs)
              )
    _ -> ZoeySamaras <$> runMessage msg attrs
