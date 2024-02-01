module Arkham.Treachery.Cards.WordsOfPower (
  wordsOfPower,
  WordsOfPower (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Types (Field (..))
import Arkham.Game.Helpers
import Arkham.Matcher hiding (treacheryInThreatAreaOf)
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WordsOfPower = WordsOfPower TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

wordsOfPower :: TreacheryCard WordsOfPower
wordsOfPower = treachery WordsOfPower Cards.wordsOfPower

instance HasModifiersFor WordsOfPower where
  getModifiersFor (EnemyTarget eid) (WordsOfPower a) = do
    case treacheryInThreatAreaOf a of
      Nothing -> pure []
      Just iid -> do
        atSameLocation <- eid <=~> EnemyAt (locationWithInvestigator iid)
        hasDoom <- fieldP EnemyDoom (> 0) eid
        pure
          $ toModifiers
            a
            [ CannotBeDamagedByPlayerSources
              (SourceOwnedBy $ InvestigatorWithId iid)
            | atSameLocation && hasDoom
            ]
  getModifiersFor (InvestigatorTarget iid) (WordsOfPower a)
    | treacheryOnInvestigator iid a = do
        hasEnemiesWithDoom <-
          selectAny $ EnemyAt (locationWithInvestigator iid) <> EnemyWithAnyDoom
        pure
          $ toModifiers
            a
            [ CannotDiscoverCluesAt (locationWithInvestigator iid)
            | hasEnemiesWithDoom
            ]
  getModifiersFor _ _ = pure []

instance HasAbilities WordsOfPower where
  getAbilities (WordsOfPower a) =
    [ restrictedAbility a 1 OnSameLocation
        $ ActionAbility []
        $ ActionCost 2
    ]

instance RunMessage WordsOfPower where
  runMessage msg t@(WordsOfPower attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      t <$ push (toDiscardBy iid (toAbilitySource attrs 1) attrs)
    _ -> WordsOfPower <$> runMessage msg attrs
