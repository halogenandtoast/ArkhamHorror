module Arkham.Treachery.Cards.WordsOfPower (wordsOfPower) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_, modifySelect)
import Arkham.Matcher hiding (treacheryInThreatAreaOf)
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WordsOfPower = WordsOfPower TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wordsOfPower :: TreacheryCard WordsOfPower
wordsOfPower = treachery WordsOfPower Cards.wordsOfPower

instance HasModifiersFor WordsOfPower where
  getModifiersFor (WordsOfPower a) = case a.placement of
    InThreatArea iid -> do
      modifySelect
        a
        (EnemyAt (locationWithInvestigator iid) <> EnemyWithAnyDoom)
        [CannotBeDamagedByPlayerSources (SourceOwnedBy $ InvestigatorWithId iid)]
      hasEnemiesWithDoom <- selectAny $ enemyAtLocationWith iid <> EnemyWithAnyDoom
      modifiedWhen_ a hasEnemiesWithDoom iid [CannotDiscoverCluesAt (locationWithInvestigator iid)]
    _ -> pure mempty

instance HasAbilities WordsOfPower where
  getAbilities (WordsOfPower a) = [restricted a 1 OnSameLocation doubleActionAbility]

instance RunMessage WordsOfPower where
  runMessage msg t@(WordsOfPower attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> WordsOfPower <$> liftRunMessage msg attrs
