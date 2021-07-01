module Arkham.Types.Asset.Cards.RitualCandles
  ( ritualCandles
  , RitualCandles(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Window

newtype RitualCandles = RitualCandles AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritualCandles :: AssetCard RitualCandles
ritualCandles = hand RitualCandles Cards.ritualCandles

ability :: AssetAttrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (ReactionAbility Free)

instance HasActions env RitualCandles where
  getActions iid (WhenRevealToken You token) (RitualCandles x) = pure
    [ ActivateCardAbilityAction iid (ability x)
    | token `elem` [Skull, Cultist, Tablet, ElderSign]
    ]
  getActions iid window (RitualCandles x) = getActions iid window x

instance HasModifiersFor env RitualCandles where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env RitualCandles where
  runMessage msg a@(RitualCandles attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessages
        [ CreateWindowModifierEffect EffectSkillTestWindow
            (EffectModifiers $ toModifiers attrs [AnySkillValue 1])
            source
            (InvestigatorTarget iid)
        ]
    _ -> RitualCandles <$> runMessage msg attrs
