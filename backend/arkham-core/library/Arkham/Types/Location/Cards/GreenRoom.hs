module Arkham.Types.Location.Cards.GreenRoom
  ( greenRoom
  , GreenRoom(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype GreenRoom = GreenRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greenRoom :: LocationCard GreenRoom
greenRoom = location GreenRoom Cards.greenRoom 5 (PerPlayer 1) Plus [Triangle]

instance HasAbilities GreenRoom where
  getAbilities (GreenRoom attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here
      $ ActionAbility (Just Action.Investigate)
      $ ActionCost 1
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env GreenRoom where
  runMessage msg l@(GreenRoom attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ pushAll
      [ skillTestModifier
        source
        (InvestigatorTarget iid)
        (SkillModifier SkillIntellect 3)
      , Investigate iid (toId attrs) source Nothing SkillIntellect False
      , DiscardHand iid
      ]
    _ -> GreenRoom <$> runMessage msg attrs
