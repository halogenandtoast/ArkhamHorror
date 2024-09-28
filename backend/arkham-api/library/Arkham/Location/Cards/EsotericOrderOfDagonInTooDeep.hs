module Arkham.Location.Cards.EsotericOrderOfDagonInTooDeep (
  esotericOrderOfDagonInTooDeep,
  EsotericOrderOfDagonInTooDeep (..),
)
where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTooDeep.Helpers

newtype EsotericOrderOfDagonInTooDeep = EsotericOrderOfDagonInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

esotericOrderOfDagonInTooDeep :: LocationCard EsotericOrderOfDagonInTooDeep
esotericOrderOfDagonInTooDeep =
  locationWith
    EsotericOrderOfDagonInTooDeep
    Cards.esotericOrderOfDagonInTooDeep
    4
    (PerPlayer 1)
    connectsToAdjacent

instance HasAbilities EsotericOrderOfDagonInTooDeep where
  getAbilities (EsotericOrderOfDagonInTooDeep a) =
    extendRevealed
      a
      [ restricted a 1 UnrevealedKeyIsSetAside $ forced $ RevealLocation #after Anyone (be a)
      , skillTestAbility $ restricted a 2 Here $ ActionAbility [#parley] $ ActionCost 2
      ]

instance RunMessage EsotericOrderOfDagonInTooDeep where
  runMessage msg l@(EsotericOrderOfDagonInTooDeep attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeUnrevealedKeyOn attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #intellect (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      removeBarrierBetweenConnected iid attrs.id
      pure l
    _ -> EsotericOrderOfDagonInTooDeep <$> liftRunMessage msg attrs
