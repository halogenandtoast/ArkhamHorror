module Arkham.Homebrew.AgainstTheWendigo.Locations.SinisterTaiga (sinisterTaiga) where

import Arkham.Direction (Direction (..))
import Arkham.Helpers.Modifiers
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (riverLocation)
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Wild)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SinisterTaiga = SinisterTaiga LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sinisterTaiga :: LocationCard SinisterTaiga
sinisterTaiga = location SinisterTaiga Cards.sinisterTaiga 2 (PerPlayer 1)

instance HasModifiersFor SinisterTaiga where
  getModifiersFor (SinisterTaiga a) = do
    -- "While you are in this location, you lose -1 [willpower]."
    modifySelect a (InvestigatorAt $ be a) [SkillModifier #willpower (-1)]
    {- "(revealed) This location is connected to the River location directly to the
    East or West, and connected to the Wild locations directly to the North and
    South." Its unrevealed side prints only an icon, which nothing answers, so
    until it is revealed the river's own connection is a one-way trip in. -}
    modifySelf
      a
      [ ConnectedToWhen RevealedLocation
          $ oneOf
            [ riverLocation <> oneOf [LocationInDirection d (be a) | d <- [LeftOf, RightOf]]
            , LocationWithTrait Wild <> oneOf [LocationInDirection d (be a) | d <- [Above, Below]]
            ]
      ]
