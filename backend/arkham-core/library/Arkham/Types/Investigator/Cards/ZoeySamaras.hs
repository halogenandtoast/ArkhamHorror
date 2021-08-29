module Arkham.Types.Investigator.Cards.ZoeySamaras where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
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
import Arkham.Types.Stats
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Token
import Arkham.Types.Trait

newtype ZoeySamaras = ZoeySamaras InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

zoeySamaras :: ZoeySamaras
zoeySamaras = ZoeySamaras $ baseAttrs
  "02001"
  "Zoey Samaras"
  Guardian
  Stats
    { health = 9
    , sanity = 6
    , willpower = 4
    , intellect = 2
    , combat = 4
    , agility = 2
    }
  [Believer, Hunter]

instance HasAbilities env ZoeySamaras where
  getAbilities _ _ (ZoeySamaras x) = pure
    [ restrictedAbility
          x
          1
          (Self <> Negate (SelfHasModifier CannotGainResources))
        $ ReactionAbility (EnemyEngaged Timing.After You AnyEnemy) Free
    ]

instance HasTokenValue env ZoeySamaras where
  getTokenValue (ZoeySamaras attrs) iid ElderSign | iid == toId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 1)
  getTokenValue (ZoeySamaras attrs) iid token = getTokenValue attrs iid token

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
