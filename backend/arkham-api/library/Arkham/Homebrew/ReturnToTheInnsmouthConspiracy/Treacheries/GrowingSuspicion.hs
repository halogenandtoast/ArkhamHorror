module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.GrowingSuspicion (growingSuspicion) where

import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (campaignI18n)
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Trait (Trait (Hybrid, Suspect))
import Arkham.Treachery.Import.Lifted

newtype GrowingSuspicion = GrowingSuspicion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

growingSuspicion :: TreacheryCard GrowingSuspicion
growingSuspicion = treachery GrowingSuspicion Cards.growingSuspicion

instance RunMessage GrowingSuspicion where
  runMessage msg t@(GrowingSuspicion attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      nearest <- select $ NearestEnemyToFallback iid (EnemyWithTrait Suspect)
      if null nearest
        then gainSurge attrs
        else chooseOneM iid $ campaignI18n $ scope "growingSuspicion" do
          whenM (fieldP InvestigatorClues (> 0) iid) do
            labeled "placeClue" $ chooseTargetM iid nearest \eid ->
              moveTokens attrs iid eid #clue 1
          labeled "suspectAttacks" $ chooseTargetM iid nearest \eid -> initiateEnemyAttack eid attrs iid
          whenAny (assetControlledBy iid <> AssetWithTrait Hybrid) do
            labeled "removeHybrid" do
              hybrids <- select $ assetControlledBy iid <> AssetWithTrait Hybrid
              chooseTargetM iid hybrids removeFromGame
      pure t
    _ -> GrowingSuspicion <$> liftRunMessage msg attrs
