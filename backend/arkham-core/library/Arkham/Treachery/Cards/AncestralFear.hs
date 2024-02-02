module Arkham.Treachery.Cards.AncestralFear (
  ancestralFear,
  AncestralFear (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype AncestralFear = AncestralFear TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

ancestralFear :: TreacheryCard AncestralFear
ancestralFear = treachery AncestralFear Cards.ancestralFear

instance RunMessage AncestralFear where
  runMessage msg t@(AncestralFear attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      mLocation <- selectOne $ locationWithInvestigator iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [ Label
            "Place 1 doom on your location and discard Ancestral Fear (instead of placing it in the victory display)."
            [PlaceDoom (toSource attrs) (toTarget lid) 1]
          | lid <- toList mLocation
          ]
        <> [Label "Place Ancestral Fear in the victory display." [AddToVictory (toTarget attrs)]]
      pure t
    _ -> AncestralFear <$> runMessage msg attrs
