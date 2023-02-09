module Arkham.Location.Cards.GreenRoom
  ( greenRoom
  , GreenRoom(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Message
import Arkham.SkillType
import Arkham.Target

newtype GreenRoom = GreenRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greenRoom :: LocationCard GreenRoom
greenRoom = location GreenRoom Cards.greenRoom 5 (PerPlayer 1)

instance HasAbilities GreenRoom where
  getAbilities (GreenRoom attrs) = withBaseAbilities
    attrs
    [ withTooltip
        "{action} _Investigate_. You get +3 {intellect} for this investigation. After this skill test ends, discard each card in your hand."
      $ restrictedAbility attrs 1 Here
      $ ActionAbility (Just Action.Investigate)
      $ ActionCost 1
    | locationRevealed attrs
    ]

instance RunMessage GreenRoom where
  runMessage msg l@(GreenRoom attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> l <$ pushAll
      [ skillTestModifier
        source
        (InvestigatorTarget iid)
        (SkillModifier SkillIntellect 3)
      , Investigate iid (toId attrs) source Nothing SkillIntellect False
      , DiscardHand iid (toAbilitySource attrs 1)
      ]
    _ -> GreenRoom <$> runMessage msg attrs
