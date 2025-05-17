module Arkham.Act.Cards.InPursuitOfTheLiving (inPursuitOfTheLiving) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Campaigns.TheCircleUndone.Memento.Helpers
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Helpers.Modifiers (modifySelect)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Spectral))

newtype Metadata = Metadata {usedLocationIds :: [LocationId]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype InPursuitOfTheLiving = InPursuitOfTheLiving (ActAttrs `With` Metadata)
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inPursuitOfTheLiving :: ActCard InPursuitOfTheLiving
inPursuitOfTheLiving = act (2, A) (InPursuitOfTheLiving . (`with` Metadata [])) Cards.inPursuitOfTheLiving Nothing

instance HasModifiersFor InPursuitOfTheLiving where
  getModifiersFor (InPursuitOfTheLiving (a `With` _)) =
    modifySelect a Anyone [CannotDiscoverCluesAt $ NotLocation $ LocationWithTrait Spectral]

-- Group limit once per round at each location.
-- We handle this by using the usedLocationIds metadata
instance HasAbilities InPursuitOfTheLiving where
  getAbilities (InPursuitOfTheLiving (a `With` meta)) =
    guard (onSide A a)
      *> [ restricted
             a
             1
             ( OnLocation
                 $ LocationWithoutModifier CannotBeFlipped
                 <> locationNotOneOf (usedLocationIds meta)
             )
             $ FastAbility Free
         , restricted a 2 (ExtendedCardCount 4 $ VictoryDisplayCardMatch $ basic "Unfinished Business")
             $ Objective
             $ forced AnyWindow
         ]

instance RunMessage InPursuitOfTheLiving where
  runMessage msg a@(InPursuitOfTheLiving (attrs `With` meta)) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      discoverMemento CornHuskDoll
      push R1
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lid <- getJustLocation iid
      flipOverBy iid attrs lid
      pure . InPursuitOfTheLiving $ attrs `with` Metadata (lid : usedLocationIds meta)
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    EndRound -> pure . InPursuitOfTheLiving $ attrs `with` Metadata []
    _ -> InPursuitOfTheLiving . (`with` meta) <$> liftRunMessage msg attrs
