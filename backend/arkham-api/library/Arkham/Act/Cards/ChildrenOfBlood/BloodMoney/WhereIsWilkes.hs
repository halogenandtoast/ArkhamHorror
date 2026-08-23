module Arkham.Act.Cards.ChildrenOfBlood.BloodMoney.WhereIsWilkes (whereIsWilkes) where

import Arkham.Ability
import Arkham.Agenda.Sequence qualified as Agenda
import Arkham.Act.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.ChildrenOfBlood.Key
import Arkham.Card
import Arkham.Helpers.GameValue (getGameValue)
import Arkham.Helpers.Location (replaceLocation)
import Arkham.Helpers.Query (getPlayerCount, getSetAsideCardsMatching)
import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Locations
import Arkham.Location.Types (Field (LocationClues, LocationRevealClues))
import Arkham.Matcher
import Arkham.Projection

newtype WhereIsWilkes = WhereIsWilkes ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whereIsWilkes :: ActCard WhereIsWilkes
whereIsWilkes = act (2, A) WhereIsWilkes Cards.whereIsWilkes Nothing

instance HasAbilities WhereIsWilkes where
  getAbilities = actAbilities \x ->
    [ mkAbility x 1
        $ Objective
        $ forced
        $ Enters #after Anyone (locationIs Locations.masterBedroom)
    ]

instance RunMessage WhereIsWilkes where
  runMessage msg a@(WhereIsWilkes attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      masterBedroom <- selectJust $ locationIs Locations.masterBedroom
      wilkes <- getSetAsideCardsMatching (#enemy <> CardWithTitle "Howard Wilkes")
      for_ wilkes \card -> do
        eid <- createEnemyAt card masterBedroom
        defeatedZburamoarte <- getHasRecord InvestigatorsDefeatedZburamoarte
        if defeatedZburamoarte
          then do
            exhaustThis eid
            disengageFromAll eid
          else selectEach (investigatorAt masterBedroom) $ initiateEnemyAttack eid attrs

      perPlayer <- getPlayerCount
      selectEach (RevealedLocation <> not_ LocationWithVictory) \lid -> do
        threshold <- getGameValue =<< field LocationRevealClues lid
        current <- field LocationClues lid
        placeClues (toSource attrs) lid (min perPlayer (max 0 (threshold - current)))

      foyer <- selectJust $ locationIs Locations.foyerBoringParty
      replaceLocation foyer =<< genCard Locations.foyerBloodyNight

      whenM (selectAny $ AgendaWithSide Agenda.A <> AgendaWithStep 1) $ advanceCurrentAgenda attrs

      advanceActDeck attrs
      pure a
    _ -> WhereIsWilkes <$> liftRunMessage msg attrs
