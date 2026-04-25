module Arkham.Act.Cards.SearchingForDrArmitage (searchingForDrArmitage) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card (filterCards)
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers (unDeck)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Scenario (getEncounterDeck, getEncounterDiscard)
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted.Choose
import Arkham.Modifier (ModifierType (DoNotTakeUpSlot))
import Arkham.Placement
import Arkham.Scenario.Deck
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries

newtype SearchingForDrArmitage = SearchingForDrArmitage ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchingForDrArmitage :: ActCard SearchingForDrArmitage
searchingForDrArmitage = act (3, A) SearchingForDrArmitage Cards.searchingForDrArmitage Nothing

instance HasAbilities SearchingForDrArmitage where
  getAbilities (SearchingForDrArmitage a) =
    [ mkAbility a 1
        $ Objective
        $ triggered (RoundEnds #when) (GroupClueCost (PerPlayer 3) (LocationWithTitle "Orne Library"))
    ]

instance RunMessage SearchingForDrArmitage where
  runMessage msg a@(SearchingForDrArmitage attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #clues attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      drArmitage <- getSetAsideCard Assets.drHenryArmitage_c2026
      lead <- getLead
      iids <- select UneliminatedInvestigator
      chooseOrRunOneM lead do
        targets iids \iid -> do
          armitage <- createAssetAt drArmitage (InPlayArea iid)
          gameModifier attrs (AssetTarget armitage) (DoNotTakeUpSlot #ally)

      miskatonicQuad <- selectJust $ locationIs Locations.miskatonicQuad_c2026
      createSetAsideEnemy_ Enemies.servantOfFlameRagingFury miskatonicQuad
      placeCluesOn attrs 3 miskatonicQuad

      n <- perPlayer 1
      deck <- unDeck <$> getEncounterDeck
      encounterDiscard <- getEncounterDiscard RegularEncounterDeck
      let allFire = filterCards (cardIs Treacheries.fire1) (deck <> encounterDiscard)
      let toDraw = take n allFire

      for_ toDraw $ \ec -> do
        if ec `elem` deck
          then do
            push $ RemoveFromEncounterDeck ec
            push $ InvestigatorDrewEncounterCard lead ec
          else do
            push $ RemoveFromEncounterDiscard ec
            push $ InvestigatorDrewEncounterCard lead ec

      when (any (`elem` deck) toDraw) $ push $ ShuffleDeck Deck.EncounterDeck

      advanceActDeck attrs
      pure a
    _ -> SearchingForDrArmitage <$> liftRunMessage msg attrs
