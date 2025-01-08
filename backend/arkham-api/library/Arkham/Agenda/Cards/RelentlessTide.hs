module Arkham.Agenda.Cards.RelentlessTide (RelentlessTide (..), relentlessTide) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Trait (Trait (Midtown, Suspect))

newtype RelentlessTide = RelentlessTide AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relentlessTide :: AgendaCard RelentlessTide
relentlessTide = agenda (2, A) RelentlessTide Cards.relentlessTide (Static 5)

instance HasModifiersFor RelentlessTide where
  getModifiersFor (RelentlessTide a) = do
    enemies <- modifySelect a (EnemyWithTrait Suspect) [IgnoreAloof]
    investigators <- modifySelect a Anyone [CannotParleyWith $ EnemyWithTrait Suspect]
    pure $ enemies <> investigators

instance HasAbilities RelentlessTide where
  getAbilities (RelentlessTide a) = [mkAbility a 1 $ forced $ TurnEnds #when (You <> at_ FullyFloodedLocation)]

instance RunMessage RelentlessTide where
  runMessage msg a@(RelentlessTide attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectEach (LocationWithTrait Midtown) (push . IncreaseFloodLevel)
      angryMob <- getSetAsideCard Enemies.angryMob
      placeUnrevealedKeyOn =<< createEnemyAtLocationMatching angryMob "Innsmouth Square"
      advanceAgendaDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamageAndHorror iid (attrs.ability 1) 1 1
      pure a
    _ -> RelentlessTide <$> liftRunMessage msg attrs
