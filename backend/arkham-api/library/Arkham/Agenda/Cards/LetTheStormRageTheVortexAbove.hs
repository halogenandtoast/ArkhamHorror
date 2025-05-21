module Arkham.Agenda.Cards.LetTheStormRageTheVortexAbove (letTheStormRageTheVortexAbove) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries

newtype LetTheStormRageTheVortexAbove = LetTheStormRageTheVortexAbove AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

letTheStormRageTheVortexAbove :: AgendaCard LetTheStormRageTheVortexAbove
letTheStormRageTheVortexAbove =
  agenda (2, A) LetTheStormRageTheVortexAbove Cards.letTheStormRageTheVortexAbove (Static 6)

instance HasModifiersFor LetTheStormRageTheVortexAbove where
  getModifiersFor (LetTheStormRageTheVortexAbove a) = do
    ancientEvils <- findAllCards (`isCard` Treacheries.ancientEvils)
    modifyEach a ancientEvils [AddKeyword Keyword.Surge]

instance HasAbilities LetTheStormRageTheVortexAbove where
  getAbilities (LetTheStormRageTheVortexAbove a) =
    [groupLimit PerRound $ mkAbility a 1 $ FastAbility $ GroupClueCost (PerPlayer 1) Anywhere]

instance RunMessage LetTheStormRageTheVortexAbove where
  runMessage msg a@(LetTheStormRageTheVortexAbove attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      mAbbeyTower <- selectOne $ LocationWithTitle "Abbey Tower"
      for_ mAbbeyTower (createEnemyAt_ Enemies.beastOfAldebaran)

      spawnAshleighClarke <- not <$> slain Enemies.ashleighClarke
      when spawnAshleighClarke do
        port <- selectJust $ locationIs Locations.porteDeLAvancee
        createEnemyAt_ Enemies.ashleighClarke port

      lead <- getLead
      push $ RemoveAllCopiesOfCardFromGame lead "03281"
      advanceAgendaDeck attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      push AdvanceAgendaIfThresholdSatisfied
      eachInvestigator \iid -> gainResources iid (attrs.ability 1) 2
      pure a
    _ -> LetTheStormRageTheVortexAbove <$> liftRunMessage msg attrs
