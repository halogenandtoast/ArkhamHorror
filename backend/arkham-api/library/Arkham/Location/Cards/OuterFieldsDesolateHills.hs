module Arkham.Location.Cards.OuterFieldsDesolateHills (outerFieldsDesolateHills) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Scenario (getScenarioDeck)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck

newtype Meta = Meta {topCards :: [Card]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype OuterFieldsDesolateHills = OuterFieldsDesolateHills (LocationAttrs `With` Meta)
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outerFieldsDesolateHills :: LocationCard OuterFieldsDesolateHills
outerFieldsDesolateHills =
  symbolLabel $ locationWith (OuterFieldsDesolateHills . (`with` Meta [])) Cards.outerFieldsDesolateHills 2 (Static 3) connectsToAdjacent

instance HasAbilities OuterFieldsDesolateHills where
  getAbilities (OuterFieldsDesolateHills (With a _)) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> DuringTurn You)
      $ FastAbility Free

instance RunMessage OuterFieldsDesolateHills where
  runMessage msg (OuterFieldsDesolateHills (With attrs meta)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      allCards <- getScenarioDeck EnemyDeck
      let (top, rest) = splitAt n allCards
      push $ SetScenarioDeck EnemyDeck rest
      focusCards top do
        chooseUpToNM_ iid (length top) do
          targets top $ handleTarget iid (attrs.ability 1)
        doStep 1 msg
        unfocusCards
      pure $ OuterFieldsDesolateHills $ attrs `with` Meta top
    HandleTargetChoice _iid (isAbilitySource attrs 1 -> True) (CardIdTarget cid) -> do
      pure $ OuterFieldsDesolateHills $ attrs `with` Meta (filter ((/= cid) . toCardId) (topCards meta))
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      let remaining = topCards meta
      when (notNull remaining) do
        chooseOneAtATimeM iid do
          targets remaining $ putCardOnTopOfDeck iid EnemyDeck
      pure $ OuterFieldsDesolateHills $ attrs `with` Meta []
    _ -> OuterFieldsDesolateHills . (`with` meta) <$> liftRunMessage msg attrs
