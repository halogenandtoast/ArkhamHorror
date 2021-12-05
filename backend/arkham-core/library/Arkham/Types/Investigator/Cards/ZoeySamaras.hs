module Arkham.Types.Investigator.Cards.ZoeySamaras where

import Arkham.Prelude

import Arkham.Investigator.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Game.Helpers
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Timing qualified as Timing

newtype ZoeySamaras = ZoeySamaras InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zoeySamaras :: InvestigatorCard ZoeySamaras
zoeySamaras = investigator
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

instance HasTokenValue env ZoeySamaras where
  getTokenValue (ZoeySamaras attrs) iid ElderSign | iid == toId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 1)
  getTokenValue _ _ token = pure $ TokenValue token mempty

instance InvestigatorRunner env => RunMessage env ZoeySamaras where
  runMessage msg i@(ZoeySamaras attrs) = case msg of
    UseCardAbility _ (InvestigatorSource iid) _ 1 _ | iid == toId attrs ->
      i <$ push (TakeResources (toId attrs) 1 False)
    ResolveToken _drawnToken ElderSign iid | iid == toId attrs -> i <$ push
      (CreateWindowModifierEffect
        EffectSkillTestWindow
        (EffectModifiers $ toModifiers attrs [DamageDealt 1])
        (toSource attrs)
        (toTarget attrs)
      )
    _ -> ZoeySamaras <$> runMessage msg attrs
