module Arkham.Treachery.Cards.FromAnotherTime (fromAnotherTime) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Cultist))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FromAnotherTime = FromAnotherTime TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fromAnotherTime :: TreacheryCard FromAnotherTime
fromAnotherTime = treachery FromAnotherTime Cards.fromAnotherTime

instance RunMessage FromAnotherTime where
  runMessage msg t@(FromAnotherTime attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect (Fixed 4)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      enemies <- select $ EnemyWithTrait Cultist
      if null enemies
        then findAndDrawEncounterCard iid $ #enemy <> CardWithTrait Cultist
        else do
          let autoplace = n `div` length enemies
          repeated autoplace $ for_ enemies (placeDoomOn attrs 1)
          let remaining = n `mod` length enemies
          when (remaining > 0) $ do
            chooseNM iid remaining $ targets enemies (placeDoomOn attrs 1)
      pure t
    _ -> FromAnotherTime <$> liftRunMessage msg attrs
