module Arkham.Location.Cards.KeziahsRoom (keziahsRoom) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Discover
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenario.Deck
import Arkham.Trait (Trait (Hex))

newtype Metadata = Metadata {revealTopCard :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype KeziahsRoom = KeziahsRoom (LocationAttrs `With` Metadata)
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keziahsRoom :: LocationCard KeziahsRoom
keziahsRoom = location (KeziahsRoom . (`with` Metadata False)) Cards.keziahsRoom 3 (Static 0)

instance HasAbilities KeziahsRoom where
  getAbilities (KeziahsRoom (a `With` _)) =
    extendRevealed
      a
      [ mkAbility a 1 $ freeReaction $ SkillTestResult #when You (WhileInvestigating $ be a) #success
      , haunted
          "Discard cards from the top of the encounter deck until a Hex card is discarded. Draw that card."
          a
          2
      ]

instance RunMessage KeziahsRoom where
  runMessage msg l@(KeziahsRoom (attrs `With` meta)) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      pure $ KeziahsRoom $ attrs `with` Metadata True
    Msg.DiscoverClues iid d | revealTopCard meta && d.location == DiscoverAtLocation attrs.id && d.isInvestigate == IsInvestigate -> do
      push $ DrawCards iid $ targetCardDraw attrs UnknownPlacesDeck 1
      pure $ KeziahsRoom $ attrs `with` Metadata False
    DrewCards iid drewCards | maybe False (isTarget attrs) drewCards.target -> do
      for_ drewCards.cards \card -> do
        lid <- placeLabeledLocation "unknownPlaces" card
        chooseOneM iid do
          labeled "Do not move" nothing
          labeled "Move to location" $ moveTo attrs iid lid
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discardUntilFirst iid attrs Deck.EncounterDeck (basic $ withTrait Hex)
      pure l
    RequestedEncounterCard (isSource attrs -> True) (Just iid) mcard -> do
      for_ mcard (drawCard iid)
      pure l
    _ -> KeziahsRoom . (`with` meta) <$> liftRunMessage msg attrs
