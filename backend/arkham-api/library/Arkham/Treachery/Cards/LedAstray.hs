module Arkham.Treachery.Cards.LedAstray (ledAstray) where

import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.EchoesOfThePast.Helpers
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LedAstray = LedAstray TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ledAstray :: TreacheryCard LedAstray
ledAstray = treachery LedAstray Cards.ledAstray

instance RunMessage LedAstray where
  runMessage msg t@(LedAstray attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      cultists <- selectMap EnemyTarget $ EnemyWithTrait Cultist
      hasNoClues <- fieldP InvestigatorClues (== 0) iid
      if null cultists || hasNoClues
        then placeDoomOnAgendaAndCheckAdvance 1
        else do
          chooseOneM iid do
            scenarioI18n $ labeled' "ledAstray.placeClue" do
              chooseTargetM iid cultists \cultist -> moveTokens attrs iid cultist #clue 1
            withI18n $ countVar 1 $ labeled' "placeAgendaDoomCanAdvance" $ placeDoomOnAgendaAndCheckAdvance 1
      pure t
    _ -> LedAstray <$> liftRunMessage msg attrs
