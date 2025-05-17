module Arkham.Treachery.Cards.FineDining (fineDining) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.TheLastKing.Helpers
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FineDining = FineDining TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fineDining :: TreacheryCard FineDining
fineDining = treachery FineDining Cards.fineDining

instance RunMessage FineDining where
  runMessage msg t@(FineDining attrs) = runQueueT $ scenarioI18n $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      clueCount <- field InvestigatorClues iid
      bystanders <- select $ AssetWithTrait Bystander
      chooseOrRunOneM iid do
        when (clueCount > 0 && notNull bystanders) do
          labeled' "fineDining.place" $ chooseTargetM iid bystanders \x -> moveTokens attrs iid x #clue 1
        chooseTakeHorrorAndDamage iid attrs 1 1
      pure t
    _ -> FineDining <$> liftRunMessage msg attrs
