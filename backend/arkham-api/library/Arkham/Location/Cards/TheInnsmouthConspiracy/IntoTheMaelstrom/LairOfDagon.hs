module Arkham.Location.Cards.TheInnsmouthConspiracy.IntoTheMaelstrom.LairOfDagon (lairOfDagon) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.Helpers.Modifiers
import Arkham.Key
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.IntoTheMaelstrom.Helpers
import Arkham.Trait (Trait (Sanctum))

newtype LairOfDagon = LairOfDagon LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lairOfDagon :: LocationCard LairOfDagon
lairOfDagon =
  locationWith
    LairOfDagon
    Cards.lairOfDagon
    6
    (PerPlayer 3)
    connectsToAdjacent

instance HasModifiersFor LairOfDagon where
  getModifiersFor (LairOfDagon a) = modifySelfMaybe a do
    n <- selectCount $ LocationWithAnyKeys <> withTrait Sanctum
    guard $ n > 0
    pure [ShroudModifier (-n)]

instance HasAbilities LairOfDagon where
  getAbilities (LairOfDagon a) =
    extendRevealed
      a
      [ groupLimit PerGame
          $ restricted
            a
            1
            ( Here
                <> foldMap
                  (exists . LocationWithKey)
                  [BlueKey, RedKey, GreenKey, YellowKey, PurpleKey, WhiteKey, BlackKey]
                <> hasRecordCriteria TheOrdersRitualWasDisrupted
            )
          $ FastAbility Free
      ]

instance RunMessage LairOfDagon where
  runMessage msg l@(LairOfDagon attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flashback iid Flashback14
      pure l
    _ -> LairOfDagon <$> liftRunMessage msg attrs
