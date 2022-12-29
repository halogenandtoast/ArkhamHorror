module Arkham.Location.Cards.RuinsOfCarcosaInhabitantOfCarcosa
  ( ruinsOfCarcosaInhabitantOfCarcosa
  , RuinsOfCarcosaInhabitantOfCarcosa(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Damage
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype RuinsOfCarcosaInhabitantOfCarcosa = RuinsOfCarcosaInhabitantOfCarcosa LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfCarcosaInhabitantOfCarcosa
  :: LocationCard RuinsOfCarcosaInhabitantOfCarcosa
ruinsOfCarcosaInhabitantOfCarcosa = locationWith
  RuinsOfCarcosaInhabitantOfCarcosa
  Cards.ruinsOfCarcosaInhabitantOfCarcosa
  2
  (PerPlayer 1)
  ((canBeFlippedL .~ True) . (revealedL .~ True))

instance HasAbilities RuinsOfCarcosaInhabitantOfCarcosa where
  getAbilities (RuinsOfCarcosaInhabitantOfCarcosa a) = withBaseAbilities
    a
    [ mkAbility a 1 $ ForcedAbility $ DiscoveringLastClue
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
    ResolveStory _ story' | story' == Story.inhabitantOfCarcosa -> do
      targets <- selectListMap InvestigatorTarget
        $ HealableInvestigator (toSource attrs) HorrorType Anyone
      setAsideRuinsOfCarcosa <- getSetAsideCardsMatching
        $ CardWithTitle "Ruins of Carcosa"
      otherRuinsOfCarcosa <- case setAsideRuinsOfCarcosa of
        [] -> error "missing"
        (x : xs) -> sample (x :| xs)
      pushAll
        $ [ HealHorror target (toSource attrs) 3 | target <- targets ]
        <> [ReplaceLocation (toId attrs) otherRuinsOfCarcosa]
      pure l
    _ -> RuinsOfCarcosaInhabitantOfCarcosa <$> runMessage msg attrs
