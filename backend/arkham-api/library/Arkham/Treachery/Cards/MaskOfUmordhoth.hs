module Arkham.Treachery.Cards.MaskOfUmordhoth (maskOfUmordhoth) where

import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MaskOfUmordhoth = MaskOfUmordhoth TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maskOfUmordhoth :: TreacheryCard MaskOfUmordhoth
maskOfUmordhoth = treachery MaskOfUmordhoth Cards.maskOfUmordhoth

instance HasModifiersFor MaskOfUmordhoth where
  getModifiersFor (MaskOfUmordhoth attrs) = case attrs.placement of
    AttachedToEnemy eid -> do
      isUnique <- elem eid <$> select UniqueEnemy
      let keyword = if isUnique then Keyword.Retaliate else Keyword.Aloof
      modified_ attrs eid [HealthModifier 2, AddKeyword keyword]
    _ -> pure mempty

instance RunMessage MaskOfUmordhoth where
  runMessage msg t@(MaskOfUmordhoth attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- select $ FarthestEnemyFrom iid $ EnemyWithTrait Cultist
      when (null enemies) $ findAndDrawEncounterCard iid $ #enemy <> CardWithTrait Cultist
      do_ msg
      pure t
    Do (Revelation iid (isSource attrs -> True)) -> do
      enemies <- select $ FarthestEnemyFrom iid $ EnemyWithTrait Cultist
      chooseOrRunOneM iid $ targets enemies $ attachTreachery attrs
      pure t
    _ -> MaskOfUmordhoth <$> liftRunMessage msg attrs
