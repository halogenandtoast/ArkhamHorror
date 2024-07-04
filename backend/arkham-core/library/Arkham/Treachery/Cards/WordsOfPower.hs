module Arkham.Treachery.Cards.WordsOfPower (wordsOfPower, WordsOfPower (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Types (Field (..))
import Arkham.Game.Helpers
import Arkham.Matcher hiding (treacheryInThreatAreaOf)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WordsOfPower = WordsOfPower TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wordsOfPower :: TreacheryCard WordsOfPower
wordsOfPower = treachery WordsOfPower Cards.wordsOfPower

instance HasModifiersFor WordsOfPower where
  getModifiersFor (EnemyTarget eid) (WordsOfPower a) = do
    maybeModified a do
      iid <- hoistMaybe $ treacheryInThreatAreaOf a
      guardM $ lift $ eid <=~> EnemyAt (locationWithInvestigator iid)
      guardM $ lift $ fieldSome EnemyDoom eid
      pure [CannotBeDamagedByPlayerSources (SourceOwnedBy $ InvestigatorWithId iid)]
  getModifiersFor (InvestigatorTarget iid) (WordsOfPower a) | treacheryInThreatArea iid a = do
    hasEnemiesWithDoom <- selectAny $ enemyAtLocationWith iid <> EnemyWithAnyDoom
    modified a [CannotDiscoverCluesAt (locationWithInvestigator iid) | hasEnemiesWithDoom]
  getModifiersFor _ _ = pure []

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
