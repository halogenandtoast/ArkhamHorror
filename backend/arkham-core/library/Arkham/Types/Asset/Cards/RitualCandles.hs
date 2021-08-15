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
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Window

newtype RitualCandles = RitualCandles AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritualCandles :: AssetCard RitualCandles
ritualCandles = hand RitualCandles Cards.ritualCandles

ability :: AssetAttrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (ResponseAbility Free)

instance HasAbilities env RitualCandles where
  getAbilities iid (WhenRevealToken who token) (RitualCandles x)
    | iid == who && ownedBy x iid
    = pure
      [ ability x
      | tokenFace token `elem` [Skull, Cultist, Tablet, ElderThing]
      ]
  getAbilities iid window (RitualCandles x) = getAbilities iid window x

instance HasModifiersFor env RitualCandles

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env RitualCandles where
  runMessage msg a@(RitualCandles attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [skillTestModifier attrs (InvestigatorTarget iid) (AnySkillValue 1)]
    _ -> RitualCandles <$> runMessage msg attrs
