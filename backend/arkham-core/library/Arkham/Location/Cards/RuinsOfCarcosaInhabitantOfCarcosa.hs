module Arkham.Location.Cards.RuinsOfCarcosaInhabitantOfCarcosa (
  ruinsOfCarcosaInhabitantOfCarcosa,
  RuinsOfCarcosaInhabitantOfCarcosa (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story
import Arkham.Timing qualified as Timing

newtype RuinsOfCarcosaInhabitantOfCarcosa = RuinsOfCarcosaInhabitantOfCarcosa LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfCarcosaInhabitantOfCarcosa
  :: LocationCard RuinsOfCarcosaInhabitantOfCarcosa
ruinsOfCarcosaInhabitantOfCarcosa =
  locationWith
    RuinsOfCarcosaInhabitantOfCarcosa
    Cards.ruinsOfCarcosaInhabitantOfCarcosa
    2
    (PerPlayer 1)
    ((canBeFlippedL .~ True) . (revealedL .~ True))

instance HasAbilities RuinsOfCarcosaInhabitantOfCarcosa where
  getAbilities (RuinsOfCarcosaInhabitantOfCarcosa a) =
    withBaseAbilities
      a
      [ mkAbility a 1 $
          ForcedAbility $
            DiscoveringLastClue
              Timing.After
              You
              (LocationWithId $ toId a)
      ]

instance RunMessage RuinsOfCarcosaInhabitantOfCarcosa where
  runMessage msg l@(RuinsOfCarcosaInhabitantOfCarcosa attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ InvestigatorAssignDamage iid source DamageAny 1 0
      pure l
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.inhabitantOfCarcosa
      pure . RuinsOfCarcosaInhabitantOfCarcosa $ attrs & canBeFlippedL .~ False
    _ -> RuinsOfCarcosaInhabitantOfCarcosa <$> runMessage msg attrs
