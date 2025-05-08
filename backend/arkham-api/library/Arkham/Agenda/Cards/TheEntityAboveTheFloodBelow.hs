module Arkham.Agenda.Cards.TheEntityAboveTheFloodBelow (theEntityAboveTheFloodBelow) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Trait

newtype TheEntityAboveTheFloodBelow = TheEntityAboveTheFloodBelow AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEntityAboveTheFloodBelow :: AgendaCard TheEntityAboveTheFloodBelow
theEntityAboveTheFloodBelow =
  agenda (2, C) TheEntityAboveTheFloodBelow Cards.theEntityAboveTheFloodBelow (Static 6)

instance HasModifiersFor TheEntityAboveTheFloodBelow where
  getModifiersFor (TheEntityAboveTheFloodBelow a) =
    modifySelect a (EnemyWithTrait Monster) [EnemyFight 1]

instance HasAbilities TheEntityAboveTheFloodBelow where
  getAbilities (TheEntityAboveTheFloodBelow a) =
    [groupLimit PerRound $ mkAbility a 1 $ FastAbility $ GroupClueCost (PerPlayer 1) Anywhere]

instance RunMessage TheEntityAboveTheFloodBelow where
  runMessage msg a@(TheEntityAboveTheFloodBelow attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide D attrs -> True) -> do
      mChapel <- selectOne $ LocationWithTitle "Chapel of St. Aubert"
      for_ (toList mChapel) (createEnemyAt_ Enemies.beastOfAldebaran)

      spawnAshleighClarke <- not <$> slain Enemies.ashleighClarke
      when spawnAshleighClarke do
        port <- selectJust $ LocationWithTitle "Porte de l'Avancée"
        createEnemyAt_ Enemies.ashleighClarke port

      lead <- getLead
      push $ RemoveAllCopiesOfCardFromGame lead "03282"
      advanceAgendaDeck attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      push AdvanceAgendaIfThresholdSatisfied
      eachInvestigator \iid -> drawCards iid (attrs.ability 1) 1
      pure a
    _ -> TheEntityAboveTheFloodBelow <$> liftRunMessage msg attrs
