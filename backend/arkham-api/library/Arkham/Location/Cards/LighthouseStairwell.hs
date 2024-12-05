module Arkham.Location.Cards.LighthouseStairwell (lighthouseStairwell, LighthouseStairwell (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ALightInTheFog.Helpers.Location

newtype LighthouseStairwell = LighthouseStairwell LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lighthouseStairwell :: LocationCard LighthouseStairwell
lighthouseStairwell = location LighthouseStairwell Cards.lighthouseStairwell 3 (PerPlayer 1)

instance HasModifiersFor LighthouseStairwell where
  getModifiersFor (LighthouseStairwell attrs) = modifySelf attrs [CannotBeFullyFlooded]

instance HasAbilities LighthouseStairwell where
  getAbilities (LighthouseStairwell attrs) =
    extendRevealed1 attrs
      $ restricted attrs 1 (Here <> exists (SetAsideCardMatch $ #asset <> #relic))
      $ FastAbility Free

instance RunMessage LighthouseStairwell where
  runMessage msg l@(LighthouseStairwell attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- getSetAsideCardsMatching $ #asset <> #relic
      focusCards cards \unfocus -> do
        chooseOrRunOneM iid do
          targets cards $ addToHand iid . only
        push unfocus
      pure l
    PlaceGrid (GridLocation pos lid) | lid == attrs.id -> setConnectedInRow pos l
    _ -> LighthouseStairwell <$> liftRunMessage msg attrs
