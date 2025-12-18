module Arkham.Act.Cards.TheAscent (theAscent) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Concealed
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher

newtype TheAscent = TheAscent ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theAscent :: ActCard TheAscent
theAscent = act (3, A) TheAscent Cards.theAscent Nothing

instance HasAbilities TheAscent where
  getAbilities = actAbilities1 \a ->
    mkAbility a 1 $ Objective $ forced $ Enters #after Anyone "The Towering Vertex"

instance RunMessage TheAscent where
  runMessage msg a@(TheAscent attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectEach ConcealedCardAny removeFromGame
      selectEach (enemyIs Enemies.theRedGlovedManPurposeUnknown) removeFromGame
      theToweringVertex <- selectJust $ locationIs Locations.theToweringVertexRuinousConflux
      createEnemyAt_ Enemies.mimeticNemesisInfiltratorOfRealities theToweringVertex
      decoys <- flip replicate Decoy <$> perPlayer 2
      cards <-
        shuffle =<< traverse mkConcealedCard (MimeticNemesis : decoys)
      lead <- getLead
      let positions = [Pos 1 2, Pos 0 3, Pos (-1) 2]
      for_ cards (push . CreateConcealedCard)
      scenarioSpecific "distributeConcealedLocations" (lead, cards, positions, positions)
      advanceActDeck attrs
      pure a
    _ -> TheAscent <$> liftRunMessage msg attrs
