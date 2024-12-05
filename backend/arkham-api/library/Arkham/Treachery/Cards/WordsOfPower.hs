module Arkham.Treachery.Cards.WordsOfPower (wordsOfPower, WordsOfPower (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Matcher hiding (treacheryInThreatAreaOf)
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WordsOfPower = WordsOfPower TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wordsOfPower :: TreacheryCard WordsOfPower
wordsOfPower = treachery WordsOfPower Cards.wordsOfPower

instance HasModifiersFor WordsOfPower where
  getModifiersFor (WordsOfPower a) = case a.placement of
    InThreatArea iid -> do
      enemies <-
        modifySelect
          a
          (EnemyAt (locationWithInvestigator iid) <> EnemyWithAnyDoom)
          [CannotBeDamagedByPlayerSources (SourceOwnedBy $ InvestigatorWithId iid)]
      hasEnemiesWithDoom <- selectAny $ enemyAtLocationWith iid <> EnemyWithAnyDoom
      investigator <-
        modifiedWhen_ a hasEnemiesWithDoom iid [CannotDiscoverCluesAt (locationWithInvestigator iid)]
      pure $ enemies <> investigator
    _ -> pure mempty

instance HasAbilities WordsOfPower where
  getAbilities (WordsOfPower a) =
    [restrictedAbility a 1 OnSameLocation $ ActionAbility [] $ ActionCost 2]

instance RunMessage WordsOfPower where
  runMessage msg t@(WordsOfPower attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> WordsOfPower <$> runMessage msg attrs
