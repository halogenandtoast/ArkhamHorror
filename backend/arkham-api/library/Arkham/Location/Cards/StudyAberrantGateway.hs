module Arkham.Location.Cards.StudyAberrantGateway (StudyAberrantGateway (..), studyAberrantGateway) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (studyAberrantGateway)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype StudyAberrantGateway = StudyAberrantGateway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

studyAberrantGateway :: LocationCard StudyAberrantGateway
studyAberrantGateway = location StudyAberrantGateway Cards.studyAberrantGateway 3 (PerPlayer 1)

instance HasAbilities StudyAberrantGateway where
  getAbilities (StudyAberrantGateway attrs) =
    extendRevealed
      attrs
      [ restricted attrs 1 (Here <> youExist LeadInvestigator) $ ActionAbility [] $ ActionCost 2
      , mkAbility attrs 2 $ forced $ EnemyAttemptsToSpawnAt #when AnyEnemy LocationNotInPlay
      ]

getMatcher :: [Window] -> LocationMatcher
getMatcher [] = error "Expected a window"
getMatcher ((windowType -> Window.EnemyAttemptsToSpawnAt _ matcher) : _) = matcher
getMatcher (_ : rest) = getMatcher rest

instance RunMessage StudyAberrantGateway where
  runMessage msg l@(StudyAberrantGateway attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCardsIfCan iid (attrs.ability 1) 3
      pure l
    UseCardAbility _ (isSource attrs -> True) 2 (getMatcher -> matcher) _ -> do
      case matcher of
        LocationWithTitle title -> push (PlaceLocationMatching $ CardWithTitle title)
        _ -> error "Expected everything to use titles"
      pure l
    _ -> StudyAberrantGateway <$> liftRunMessage msg attrs
