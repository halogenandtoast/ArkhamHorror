module Arkham.Location.Cards.TheAbyssSpiralingOblivion (theAbyssSpiralingOblivion) where

import Arkham.Ability
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Scenario
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Phase
import Arkham.Scenario.Deck
import Arkham.Trait (Trait (Shattered))

newtype TheAbyssSpiralingOblivion = TheAbyssSpiralingOblivion LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theAbyssSpiralingOblivion :: LocationCard TheAbyssSpiralingOblivion
theAbyssSpiralingOblivion = location TheAbyssSpiralingOblivion Cards.theAbyssSpiralingOblivion 5 (PerPlayer 2)

instance HasModifiersFor TheAbyssSpiralingOblivion where
  getModifiersFor (TheAbyssSpiralingOblivion attrs) = do
    phase <- getPhase
    when (phase == EnemyPhase) do
      modifySelect attrs (EnemyAt (be attrs) <> EnemyWithTrait Shattered) [RemoveKeyword Keyword.Aloof]

instance HasAbilities TheAbyssSpiralingOblivion where
  getAbilities (TheAbyssSpiralingOblivion attrs) =
    extendRevealed1 attrs
      $ restricted attrs 1 (ScenarioDeckWithCard AbyssDeck)
      $ forced
      $ Enters #after You (be attrs)

instance RunMessage TheAbyssSpiralingOblivion where
  runMessage msg l@(TheAbyssSpiralingOblivion attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      abyss <- getScenarioDeck AbyssDeck
      when (notNull abyss) $ kill (attrs.ability 1) iid
      pure l
    _ -> TheAbyssSpiralingOblivion <$> liftRunMessage msg attrs
