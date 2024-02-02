module Arkham.Location.Cards.AbbeyTowerThePathIsOpen (
  abbeyTowerThePathIsOpen,
  AbbeyTowerThePathIsOpen (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.ScenarioLogKey

newtype AbbeyTowerThePathIsOpen = AbbeyTowerThePathIsOpen LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

abbeyTowerThePathIsOpen :: LocationCard AbbeyTowerThePathIsOpen
abbeyTowerThePathIsOpen =
  location AbbeyTowerThePathIsOpen Cards.abbeyTowerThePathIsOpen 3 (PerPlayer 2)

instance HasModifiersFor AbbeyTowerThePathIsOpen where
  getModifiersFor target (AbbeyTowerThePathIsOpen attrs) | isTarget attrs target = do
    foundAGuide <- remembered FoundTheTowerKey
    pure $ toModifiers attrs [Blocked | not (locationRevealed attrs) && not foundAGuide]
  getModifiersFor (InvestigatorTarget iid) (AbbeyTowerThePathIsOpen attrs) = do
    here <- iid `isAt` attrs
    cardsInHand <- fieldMap InvestigatorHand length iid
    pure $ toModifiers attrs [CannotDiscoverClues | here, cardsInHand == 0]
  getModifiersFor _ _ = pure []

instance HasAbilities AbbeyTowerThePathIsOpen where
  getAbilities (AbbeyTowerThePathIsOpen a) =
    withBaseAbilities a
      $ [restrictedAbility a 1 (Here <> exists (You <> HandWith (HasCard NonWeakness))) actionAbility]

instance RunMessage AbbeyTowerThePathIsOpen where
  runMessage msg l@(AbbeyTowerThePathIsOpen attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      maxDiscardAmount <- selectCount $ InHandOf (InvestigatorWithId iid) <> BasicCardMatch NonWeakness
      player <- getPlayer iid
      push
        $ chooseAmounts
          player
          "Discard up to 3 cards from your hand"
          (MaxAmountTarget 3)
          [("Cards", (0, maxDiscardAmount))]
          (toTarget attrs)
      pure l
    ResolveAmounts iid choices target | isTarget attrs target -> do
      let discardAmount = getChoiceAmount "Cards" choices
      when (discardAmount > 0)
        $ pushAll
        $ replicate discardAmount (toMessage $ chooseAndDiscardCard iid (toAbilitySource attrs 1))
      pure l
    _ -> AbbeyTowerThePathIsOpen <$> runMessage msg attrs
