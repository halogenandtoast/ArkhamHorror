module Arkham.Act.Cards.IntoTheGate (intoTheGate) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers (campaignI18n)
import Arkham.Campaigns.TheForgottenAge.Helpers (exploreAction_, runExplore)
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getSetAsideCardMaybe, getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move

newtype IntoTheGate = IntoTheGate ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheGate :: ActCard IntoTheGate
intoTheGate = act (2, A) IntoTheGate Cards.intoTheGate Nothing

instance HasAbilities IntoTheGate where
  getAbilities (IntoTheGate a) =
    [ mkAbility a 1 exploreAction_
    , restricted
        a
        2
        (notExists $ UneliminatedInvestigator <> not_ (InvestigatorAt $ locationIs Locations.eldritchGate))
        $ Objective
        $ triggered (RoundEnds #when) (GroupClueCost (PerPlayer 1) (locationIs Locations.eldritchGate))
    ]

instance RunMessage IntoTheGate where
  runMessage msg a@(IntoTheGate attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      runExplore iid (attrs.ability 1)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithClues attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      aDreamBetwixt <- placeSetAsideLocation Locations.aDreamBetwixt
      reveal aDreamBetwixt
      eachInvestigator \iid -> moveTo attrs iid aDreamBetwixt
      selectEach (NotLocation $ locationIs Locations.aDreamBetwixt) removeLocation
      whenJustM (getSetAsideCardMaybe Enemies.xzharah) \xzharah -> do
        xzharahId <- createEnemyAt xzharah aDreamBetwixt
        shantaks <- getSetAsideCardsMatching $ cardIs Enemies.dreadedShantak
        shuffleCardsIntoDeck Deck.EncounterDeck shantaks
        shuffleEncounterDiscardBackIn
        ankhInPlay <- selectAny $ assetIs Assets.ancientAnkh
        when ankhInPlay do
          campaignI18n $ flavor $ p "intoTheGate.ankh"
          exhaustThis xzharahId
          disengageFromAll xzharahId
      advanceActDeck attrs
      pure a
    _ -> IntoTheGate <$> liftRunMessage msg attrs
