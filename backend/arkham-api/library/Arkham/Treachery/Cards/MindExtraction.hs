module Arkham.Treachery.Cards.MindExtraction (mindExtraction) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Agenda (getCurrentAgendaStep)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MindExtraction = MindExtraction TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindExtraction :: TreacheryCard MindExtraction
mindExtraction = treachery MindExtraction Cards.mindExtraction

instance RunMessage MindExtraction where
  runMessage msg t@(MindExtraction attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      step <- getCurrentAgendaStep
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed (step + 1))
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      mEnemy <- selectOne $ enemyIs Enemies.theBloodlessMan
      for_ mEnemy \eid -> do
        mAsset <- selectOne $ AssetControlledBy Anyone <> assetIs Assets.thePaleLanternBeguilingAura
        for_ mAsset \aid -> do
          flipOverBy iid attrs aid
          place aid (AttachedToEnemy eid)
      pure t
    _ -> MindExtraction <$> liftRunMessage msg attrs
