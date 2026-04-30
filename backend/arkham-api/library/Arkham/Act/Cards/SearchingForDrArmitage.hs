module Arkham.Act.Cards.SearchingForDrArmitage (searchingForDrArmitage) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Modifier (ModifierType (DoNotTakeUpSlot))
import Arkham.Scenarios.SpreadingFlames.Helpers
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
      lead <- getLead
      iids <- select UneliminatedInvestigator
      chooseOrRunOneM lead do
        targets iids \iid -> do
          drArmitage <- createAsset =<< getSetAsideCard Assets.drHenryArmitage_c2026
          gameModifier attrs drArmitage (DoNotTakeUpSlot #ally)
          takeControlOfAsset iid drArmitage

      miskatonicQuad <- selectJust $ locationIs Locations.miskatonicQuad_c2026

      selectOne (IncludeOutOfPlayEnemy $ enemyIs Enemies.servantOfFlameRagingFury) >>= \case
        Just eid -> place eid (AtLocation miskatonicQuad)
        Nothing -> do
          card <- fetchCard Enemies.servantOfFlameRagingFury
          obtainCard card
          createEnemyAt_ card miskatonicQuad
      placeCluesCount <- perPlayer 3
      placeCluesOn attrs placeCluesCount miskatonicQuad

      n <- perPlayer 1
      inDiscard <- select $ InEncounterDiscard <> basic (cardIs Treacheries.fire1)

      chooseOrRunOneM lead $ scenarioI18n do
        when (length inDiscard >= n) do
          labeled' "searchingForDrArmitage.drawFromDiscard"
            $ for_ (take n inDiscard)
            $ drawCardFrom lead Deck.EncounterDiscard
        labeled' "searchingForDrArmitage.drawFromBoth"
          $ repeated n
          $ findAndDrawEncounterCard lead (cardIs Treacheries.fire1)

      advanceActDeck attrs
      pure a
    _ -> SearchingForDrArmitage <$> liftRunMessage msg attrs
