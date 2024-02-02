module Arkham.Location.Cards.BaseOfTheSteps (
  baseOfTheSteps,
  BaseOfTheSteps (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection

newtype BaseOfTheSteps = BaseOfTheSteps LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

baseOfTheSteps :: LocationCard BaseOfTheSteps
baseOfTheSteps = location BaseOfTheSteps Cards.baseOfTheSteps 3 (PerPlayer 1)

instance HasModifiersFor BaseOfTheSteps where
  getModifiersFor target (BaseOfTheSteps a) | a `is` target = do
    blocked <- selectAny $ locationIs Cards.sevenHundredSteps <> LocationWithAnyClues
    pure $ toModifiers a [Blocked | blocked]
  getModifiersFor _ _ = pure []

instance HasAbilities BaseOfTheSteps where
  getAbilities (BaseOfTheSteps a) =
    withRevealedAbilities
      a
      [forcedAbility a 1 $ Enters #after (You <> HandWith (LengthIs $ atLeast 1)) $ be a]

instance RunMessage BaseOfTheSteps where
  runMessage msg l@(BaseOfTheSteps attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      handSize <- fieldMap InvestigatorHand length iid
      push $ beginSkillTest iid (toAbilitySource attrs 1) iid #willpower handSize
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      player <- getPlayer iid
      hand <- field InvestigatorHand iid
      for_ hand \card -> do
        pushAll
          [ FocusCards [card]
          , chooseOne
              player
              [ Label "Discard" [UnfocusCards, toMessage $ discardCard iid (toAbilitySource attrs 1) card]
              , Label "Take 1 horror" [UnfocusCards, assignHorror iid (toAbilitySource attrs 1) 1]
              ]
          ]
      pure l
    _ -> BaseOfTheSteps <$> runMessage msg attrs
