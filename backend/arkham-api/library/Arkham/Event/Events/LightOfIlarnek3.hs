module Arkham.Event.Events.LightOfIlarnek3 (lightOfIlarnek3) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher

newtype LightOfIlarnek3 = LightOfIlarnek3 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightOfIlarnek3 :: EventCard LightOfIlarnek3
lightOfIlarnek3 = event LightOfIlarnek3 Cards.lightOfIlarnek3

instance HasModifiersFor LightOfIlarnek3 where
  getModifiersFor (LightOfIlarnek3 a) =
    for_ a.attachedTo \target -> modified_ a target [ShroudModifier (-2)]

instance RunMessage LightOfIlarnek3 where
  runMessage msg e@(LightOfIlarnek3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      discoverAtYourLocation NotInvestigate iid attrs 1
      doStep 1 msg
      pure e
    -- "Then": the attach target is chosen after the clue is discovered, which is
    -- also where "Limit 1 per location" bites -- the facedown play is not a play
    -- at any particular location.
    DoStep 1 (PlayThisEvent iid (is attrs -> True)) -> do
      locations <-
        select
          $ RevealedLocation
          <> LocationCanHaveAttachments
          <> not_ (LocationWithAttachedEvent $ eventIs Cards.lightOfIlarnek3)
      when (notNull locations) do
        chooseOneM iid $ targets locations $ place attrs . AttachedToLocation
      pure e
    _ -> LightOfIlarnek3 <$> liftRunMessage msg attrs
