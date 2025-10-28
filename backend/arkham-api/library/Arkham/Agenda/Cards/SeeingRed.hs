module Arkham.Agenda.Cards.SeeingRed (seeingRed) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype SeeingRed = SeeingRed AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seeingRed :: AgendaCard SeeingRed
seeingRed = agenda (3, A) SeeingRed Cards.seeingRed (Static 11)

instance HasAbilities SeeingRed where
  getAbilities (SeeingRed a) =
    [ mkAbility a 1 $ forced $ CampaignEvent #after Nothing "exposed[decoy]"
    , mkAbility a 2 $ Objective $ forced $ ifEnemyDefeated Enemies.theSanguineWatcherWithTheRubySpectacles
    ]

instance RunMessage SeeingRed where
  runMessage msg a@(SeeingRed attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoomOnAgenda 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceAgenda attrs
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R2
      pure a
    _ -> SeeingRed <$> liftRunMessage msg attrs
