module Arkham.Treachery.Cards.AncestralFear
  ( ancestralFear
  , AncestralFear(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype AncestralFear = AncestralFear TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancestralFear :: TreacheryCard AncestralFear
ancestralFear = treachery AncestralFear Cards.ancestralFear

instance RunMessage AncestralFear where
  runMessage msg t@(AncestralFear attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mLocation <- selectOne $ locationWithInvestigator iid
      -- Due to adding to victory not triggering surge we have to manually call
      -- it. Ideally we would solve surge another way
      push
        $ chooseOrRunOne iid
        $ [ Label
              "Place 1 doom on your location and discard Ancestral Fear (instead of placing it in the victory display)."
              [PlaceDoom (idToTarget lid) 1]
          | lid <- maybeToList mLocation
          ]
        <> [ Label
               "Place Ancestral Fear in the victory display."
               [AddToVictory (toTarget attrs), Surge iid source]
           ]
      pure t
    _ -> AncestralFear <$> runMessage msg attrs
