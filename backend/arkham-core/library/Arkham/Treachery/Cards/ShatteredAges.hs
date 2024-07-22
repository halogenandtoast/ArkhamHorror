module Arkham.Treachery.Cards.ShatteredAges (shatteredAges, ShatteredAges (..)) where

import Arkham.Classes
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Prelude
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
      sid <- getRandom
      push $ revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      locations <- select $ NotLocation $ locationIs Locations.nexusOfNKai
      pushAll [PlaceClues (toSource attrs) (toTarget location) 1 | location <- locations]
      pure t
    _ -> ShatteredAges <$> runMessage msg attrs
