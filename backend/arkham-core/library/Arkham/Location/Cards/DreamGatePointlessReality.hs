module Arkham.Location.Cards.DreamGatePointlessReality (
  dreamGatePointlessReality,
  DreamGatePointlessReality (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Movement
import Arkham.Projection

newtype DreamGatePointlessReality = DreamGatePointlessReality LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

dreamGatePointlessReality :: LocationCard DreamGatePointlessReality
dreamGatePointlessReality = location DreamGatePointlessReality Cards.dreamGatePointlessReality 6 (Static 0)

instance HasModifiersFor DreamGatePointlessReality where
  getModifiersFor target (DreamGatePointlessReality a) | a `is` target = do
    pure $ toModifiers a [CannotBeEnteredBy AnyEnemy]
  getModifiersFor (InvestigatorTarget iid) (DreamGatePointlessReality a) = do
    notLuke <- iid <!=~> investigatorIs Investigators.lukeRobinson
    pure $ toModifiers a [CannotEnter (toId a) | notLuke]
  getModifiersFor _ _ = pure []

instance HasAbilities DreamGatePointlessReality where
  getAbilities (DreamGatePointlessReality attrs) =
    withRevealedAbilities attrs
      $ [ mkAbility attrs 1
            $ freeReaction
            $ SkillTestResult
              #after
              You
              (WhileInvestigating $ LocationWithId $ toId attrs)
              (SuccessResult AnyValue)
        , restrictedAbility attrs 2 (exists $ You <> investigatorIs Investigators.lukeRobinson)
            $ ForcedAbility
            $ PhaseEnds #when #investigation
        ]

instance RunMessage DreamGatePointlessReality where
  runMessage msg l@(DreamGatePointlessReality attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      wondrousJourney <- genCard Locations.dreamGateWondrousJourney
      push $ ReplaceLocation (toId attrs) wondrousJourney Swap
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ RemoveLocation (toId attrs)
      here <- fieldMap InvestigatorLocation (== Just (toId attrs)) iid
      when here do
        revealedLocations <- selectList (RevealedLocation <> NotLocation (LocationWithId $ toId attrs))
        when (notNull revealedLocations) $ do
          player <- getPlayer iid
          push
            $ chooseOne
              player
              [ targetLabel lid [Move $ move (toSource attrs) iid lid, assignHorror iid (toAbilitySource attrs 2) 2]
              | lid <- revealedLocations
              ]

      pure l
    _ -> DreamGatePointlessReality <$> runMessage msg attrs
