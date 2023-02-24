module Arkham.Treachery.Cards.ShatteredAges
  ( shatteredAges
  , ShatteredAges(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ShatteredAges = ShatteredAges TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shatteredAges :: TreacheryCard ShatteredAges
shatteredAges = treachery ShatteredAges Cards.shatteredAges

instance RunMessage ShatteredAges where
  runMessage msg t@(ShatteredAges attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ RevelationSkillTest iid (toSource attrs) SkillWillpower 4
      pure t
    FailedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        locations <- selectList $ NotLocation $ locationIs Locations.nexusOfNKai
        pushAll
          [ PlaceClues (LocationTarget location) 1 | location <- locations ]
        pure t
    _ -> ShatteredAges <$> runMessage msg attrs
