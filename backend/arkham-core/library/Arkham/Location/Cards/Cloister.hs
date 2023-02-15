module Arkham.Location.Cards.Cloister
  ( cloister
  , Cloister(..)
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
import Arkham.ScenarioLogKey
import Arkham.SkillType
import Arkham.Target

newtype Cloister = Cloister LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloister :: LocationCard Cloister
cloister = location Cloister Cards.cloister 2 (PerPlayer 1)

instance HasAbilities Cloister where
  getAbilities (Cloister a) = withBaseAbilities
    a
    [ restrictedAbility a 1 (Here <> NoCluesOnThis)
        $ ActionAbility (Just Action.Parley) Free
    ]

instance RunMessage Cloister where
  runMessage msg l@(Cloister attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> l <$ push
      (beginSkillTest
        iid
        source
        (InvestigatorTarget iid)
        (Just Action.Parley)
        SkillWillpower
        3
      )
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> l <$ push (Remember FoundAGuide)
    _ -> Cloister <$> runMessage msg attrs
