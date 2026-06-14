module Arkham.Location.Cards.BarrierCoreInactive (barrierCoreInactive) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Seafloor))

newtype BarrierCoreInactive = BarrierCoreInactive LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barrierCoreInactive :: LocationCard BarrierCoreInactive
barrierCoreInactive = location BarrierCoreInactive Cards.barrierCoreInactive 4 (Static 1)

instance HasAbilities BarrierCoreInactive where
  getAbilities (BarrierCoreInactive a) =
    extendRevealed
      a
      [ restricted
          a
          1
          (LocationCount 6 (RevealedLocation <> LocationWithTrait Seafloor <> not_ FloodedLocation))
          $ forced
          $ RoundEnds #when
      , groupLimit PerRound
          $ restricted a 2 Here actionAbility
      ]

instance RunMessage BarrierCoreInactive where
  runMessage msg l@(BarrierCoreInactive attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOver iid attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ DecreaseFloodLevel attrs.id
      eligible <- select $ connectedTo (be attrs) <> CanHaveFloodLevelIncreased
      chooseTargetM iid eligible \lid -> do
        push $ IncreaseFloodLevel lid
        placeClues (attrs.ability 2) lid =<< perPlayer 3
      pure l
    Flip _ _ (isTarget attrs -> True) -> do
      let active = lookupCard Cards.barrierCoreActive attrs.cardId
      push $ ReplaceLocation attrs.id active Swap
      pure l
    _ -> BarrierCoreInactive <$> liftRunMessage msg attrs
