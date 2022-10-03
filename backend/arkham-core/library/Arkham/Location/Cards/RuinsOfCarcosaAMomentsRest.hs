module Arkham.Location.Cards.RuinsOfCarcosaAMomentsRest
  ( ruinsOfCarcosaAMomentsRest
  , RuinsOfCarcosaAMomentsRest(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Query
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype RuinsOfCarcosaAMomentsRest = RuinsOfCarcosaAMomentsRest LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfCarcosaAMomentsRest :: LocationCard RuinsOfCarcosaAMomentsRest
ruinsOfCarcosaAMomentsRest = locationWith
  RuinsOfCarcosaAMomentsRest
  Cards.ruinsOfCarcosaAMomentsRest
  2
  (PerPlayer 1)
  ((canBeFlippedL .~ True) . (revealedL .~ True))

instance HasAbilities RuinsOfCarcosaAMomentsRest where
  getAbilities (RuinsOfCarcosaAMomentsRest a) = withBaseAbilities
    a
    [ mkAbility a 1 $ ForcedAbility $ DiscoveringLastClue
        Timing.After
        You
        (LocationWithId $ toId a)
    ]

instance RunMessage RuinsOfCarcosaAMomentsRest where
  runMessage msg l@(RuinsOfCarcosaAMomentsRest attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ InvestigatorAssignDamage iid source DamageAny 1 0
      pure l
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.aMomentsRest
      pure . RuinsOfCarcosaAMomentsRest $ attrs & canBeFlippedL .~ False
    ResolveStory iid story' | story' == Story.aMomentsRest -> do
      enemies <- selectList
        $ EnemyOneOf [NotEnemy ExhaustedEnemy, EnemyIsEngagedWith Anyone]
      hasHorror <- fieldP InvestigatorHorror (> 0) iid
      let
        handleEnemy enemy = targetLabel
          enemy
          [Exhaust (EnemyTarget enemy), DisengageEnemyFromAll enemy]
        choices =
          [ Label
              "Choose an enemy in play. Exhaust that enemy and disengage it from all investigators"
              [chooseOne iid $ map handleEnemy enemies]
          | notNull enemies
          ]
          <> [ Label "You heal 5 horror" [HealHorror (InvestigatorTarget iid) 5]
             | hasHorror
             ]
      setAsideRuinsOfCarcosa <- getSetAsideCardsMatching
        $ CardWithTitle "Ruins of Carcosa"
      otherRuinsOfCarcosa <- case setAsideRuinsOfCarcosa of
        [] -> error "missing"
        (x : xs) -> sample (x :| xs)
      pushAll
        $ [ chooseOrRunOne iid choices | notNull choices ]
        <> [ReplaceLocation (toId attrs) otherRuinsOfCarcosa]
      pure l
    _ -> RuinsOfCarcosaAMomentsRest <$> runMessage msg attrs
