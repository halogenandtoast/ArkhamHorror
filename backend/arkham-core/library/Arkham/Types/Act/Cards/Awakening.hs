module Arkham.Types.Act.Cards.Awakening
  ( Awakening(..)
  , awakening
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.CardType
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Name

newtype Awakening = Awakening ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

awakening :: ActCard Awakening
awakening = act
  (1, A)
  Awakening
  Cards.awakening
  (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance ActRunner env => RunMessage env Awakening where
  runMessage msg a@(Awakening attrs) = case msg of
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs -> do
      -- Choose one of the set-aside locations, at random.
      -- Put that location into play
      locations <- selectList (SetAsideCardMatch $ CardWithType LocationType)
      location <- case locations of
        [] -> error "must be at least one location"
        (x : xs) -> sample (x :| xs)
      otherLocationCount <- selectCount
        (LocationWithUnrevealedTitle $ nameTitle $ toName location)
      let
        label = nameToLabel (toName location) <> tshow (otherLocationCount + 1)

      -- spawn the set-aside The Man in the Pallid Mask enemy at that location
      theManInThePallidMask <- fromJustNote "Must be set aside"
        <$> selectOne (SetAsideCardMatch $ cardIs Cards.theManInThePallidMask)

      -- Advance to one of the 3 copies of act 2a, at random
      nextActId <- ActId . toCardCode <$> sample
        (Cards.theStrangerACityAflame
        :| [Cards.theStrangerThePathIsMine, Cards.theStrangerTheShoresOfHali]
        )

      locationId <- getRandom
      a <$ pushAll
        [ PlaceLocation locationId (toCardDef location)
        , SetLocationLabel locationId label
        , CreateEnemyAt theManInThePallidMask locationId Nothing
        , NextAct (toId attrs) nextActId
        ]
    _ -> Awakening <$> runMessage msg attrs
