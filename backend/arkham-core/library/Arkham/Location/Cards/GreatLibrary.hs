module Arkham.Location.Cards.GreatLibrary
  ( greatLibrary
  , GreatLibrary(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Message
import Arkham.ScenarioLogKey
import Arkham.SkillType
import Arkham.Target

newtype GreatLibrary = GreatLibrary LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greatLibrary :: LocationCard GreatLibrary
greatLibrary = location GreatLibrary Cards.greatLibrary 2 (Static 4)

instance HasAbilities GreatLibrary where
  getAbilities (GreatLibrary attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here
      $ ActionAbility Nothing
      $ ActionCost 1
      <> PerPlayerClueCost 1
    ]

instance RunMessage GreatLibrary where
  runMessage msg l@(GreatLibrary attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ BeginSkillTest
        iid
        (toSource attrs)
        (InvestigatorTarget iid)
        Nothing
        SkillIntellect
        3
      pure l
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        push $ Remember FoundTheProcess
        pure l
    _ -> GreatLibrary <$> runMessage msg attrs
